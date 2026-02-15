;;; sync.lisp - gRPC bidirectional streaming sync server
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cloodoo)

;;── gRPC Sync Server ──────────────────────────────────────────────────────────

(defvar *grpc-server* nil
  "The gRPC server instance for sync.")

(defvar *grpc-port* 50051
  "Default port for gRPC sync server.")

(defvar *connected-clients* nil
  "List of connected sync client streams for broadcasting changes.")

(defvar *clients-lock* (bt:make-lock "clients-lock")
  "Lock for synchronizing access to connected clients list.")

(defvar *sync-debug* nil
  "When true, emit verbose sync debug output to *standard-output*.
   Do NOT enable in production -- debug output includes serialized message bytes.")

;;── Username Extraction from mTLS Certificate ────────────────────────────────

(defun extract-username-from-ctx (ctx)
  "Extract authenticated username (CN) from the client certificate.
   Returns NIL for non-TLS connections (backward compat / testing)."
  (handler-case
      (let* ((conn (ag-grpc::context-connection ctx))
             (tls-stream (ag-http2:connection-stream conn)))
        (when (typep tls-stream 'pure-tls:tls-stream)
          (let ((peer-cert (pure-tls:tls-peer-certificate tls-stream)))
            (when peer-cert
              (first (pure-tls:certificate-subject-common-names peer-cert))))))
    (error () nil)))

;;── Client Registration ───────────────────────────────────────────────────────

(defstruct sync-client
  "A connected sync client with identity information."
  device-id
  username
  stream)

(defun register-sync-client (stream device-id &optional username)
  "Register a connected sync client for broadcasting."
  (bt:with-lock-held (*clients-lock*)
    (push (make-sync-client :device-id device-id
                            :username username
                            :stream stream)
          *connected-clients*)
    (llog:info "Sync client connected" :device-id device-id :username username)))

(defun unregister-sync-client-by-stream (stream device-id)
  "Unregister a specific sync client stream.
   Removes only the entry matching this specific stream, not all entries for device-id."
  (bt:with-lock-held (*clients-lock*)
    (setf *connected-clients*
          (remove-if (lambda (client) (eq (sync-client-stream client) stream))
                     *connected-clients*))
    (llog:info "Sync client disconnected" :device-id device-id)))

(defun remove-dead-client (stream device-id)
  "Remove a dead client stream from the connected clients list."
  (bt:with-lock-held (*clients-lock*)
    (setf *connected-clients*
          (remove-if (lambda (client) (eq (sync-client-stream client) stream))
                     *connected-clients*))
    (llog:info "Removed dead sync client" :device-id device-id)))

(defun broadcast-change (change-msg &optional exclude-device-id username)
  "Broadcast a change to all connected clients except the one specified.
   CHANGE-MSG should be a proto-sync-message.
   When USERNAME is non-nil, only broadcast to clients with matching username.
   Snapshots the client list under lock, then sends outside the lock
   to avoid holding the lock during network I/O.
   Failed sends result in the client being removed from the list."
  (unless change-msg
    (llog:error "broadcast-change called with NIL message")
    (return-from broadcast-change))
  (let ((clients-snapshot
          (bt:with-lock-held (*clients-lock*)
            (copy-list *connected-clients*))))
    (dolist (client clients-snapshot)
      (let ((client-device-id (sync-client-device-id client))
            (client-username (sync-client-username client))
            (client-stream (sync-client-stream client)))
        ;; Only send if: not excluded device, and username matches (when set)
        (when (and (or (null exclude-device-id)
                       (not (string= client-device-id exclude-device-id)))
                   (or (null username)
                       (null client-username)
                       (string= username client-username)))
          (handler-case
              (ag-grpc:stream-send client-stream change-msg)
            (error (e)
              (llog:warn "Failed to send to client, removing"
                         :device-id client-device-id :error (princ-to-string e))
              (remove-dead-client client-stream client-device-id))))))))

;;── List-Capable Client Broadcasting ─────────────────────────────────────────

(defun broadcast-list-change (change-msg &optional exclude-device-id username)
  "Broadcast a list change only to clients that have 'lists' capability.
   When USERNAME is non-nil, only broadcast to clients with matching username.
   Falls back to regular broadcast (all clients) since capability tracking
   per-stream is not yet implemented at the connection level."
  ;; For now, broadcast to all clients. List-unaware clients will
  ;; simply ignore the unknown message type (protobuf forward compatibility).
  (broadcast-change change-msg exclude-device-id username))

;;── SyncStream Handler ────────────────────────────────────────────────────────

(defun handle-sync-stream (ctx stream)
  "Handler for bidirectional TodoSync.SyncStream RPC.
   CTX is the call context, STREAM is for sending/receiving messages.
   Extracts username from mTLS client certificate CN for user namespacing."
  (when *sync-debug* (format t "~&[SYNC-DEBUG] handle-sync-stream entered~%"))
  (let ((client-device-id nil)
        (username (extract-username-from-ctx ctx)))
    (when username
      (llog:info "Authenticated user from certificate" :username username))
    ;; Check if the certificate CN has been revoked
    (when (and username (cert-revoked-p username))
      (llog:warn "Rejected revoked certificate" :username username)
      (handler-case
          (let ((error-ack (make-sync-ack-message
                             (now-iso) 0
                             (format nil "Certificate for '~A' has been revoked." username))))
            (ag-grpc:stream-send stream error-ack))
        (error () nil))
      (return-from handle-sync-stream))
    (unwind-protect
         (block sync-handler
           ;; Read the first message which should be SyncInit
           (when *sync-debug* (format t "~&[SYNC-DEBUG] Waiting for init message...~%"))
           (let ((init-msg (ag-grpc:stream-recv stream)))
             (unless init-msg
               (llog:warn "Client disconnected before sending init")
               (return-from sync-handler))

             (when *sync-debug* (format t "~&[SYNC-DEBUG] Got message, msg-case=~A~%" (proto-msg-case init-msg)))

             ;; Verify it's a SyncInit message
             (unless (eql (proto-msg-case init-msg) :init)
               (llog:warn "Expected init message" :got (proto-msg-case init-msg))
               (return-from sync-handler))

             (let* ((sync-init (proto-msg-init init-msg))
                    (device-id (proto-sync-init-device-id sync-init))
                    (since (proto-sync-init-since sync-init))
                    (client-time-str (proto-sync-init-client-time sync-init))
                    (client-capabilities (proto-sync-init-capabilities sync-init))
                    (client-has-lists (member "lists" client-capabilities :test #'string=)))
               (setf client-device-id device-id)

               ;; Persist device capabilities
               (handler-case
                   (db-save-device-capabilities device-id
                                                (or client-capabilities '())
                                                nil)
                 (error (e)
                   (llog:warn "Failed to save device capabilities"
                              :device-id device-id :error (princ-to-string e))))

               (when *sync-debug*
                 (format t "~&[SYNC-DEBUG] Init: device-id=~A since=~A client-time=~A capabilities=~A~%"
                         device-id since client-time-str client-capabilities))

               ;; Check clock skew if client sent its time
               (when (and client-time-str (plusp (length client-time-str)))
                 (handler-case
                     (let* ((client-time (lt:parse-timestring client-time-str))
                            (server-time (lt:now))
                            (skew-seconds (abs (lt:timestamp-difference server-time client-time))))
                       (when (> skew-seconds 60)
                         (llog:warn "Clock skew too large, rejecting connection"
                                    :device-id device-id
                                    :skew-seconds skew-seconds)
                         (let ((error-ack (make-sync-ack-message
                                           (now-iso) 0
                                           (format nil "Clock skew too large (~,1F seconds). Please sync your device clock."
                                                   skew-seconds))))
                           (ag-grpc:stream-send stream error-ack))
                         (return-from sync-handler)))
                   (error (e)
                     (llog:warn "Failed to parse client time" :error (princ-to-string e)))))

               ;; Get pending changes to send (only current versions, not historical)
               (let* ((effective-since (if (plusp (length since))
                                           since
                                           "1970-01-01T00:00:00Z"))
                      (rows (db-load-current-rows-since effective-since :user-id username))
                      (pending-count (length rows)))
                 (format t "~&[SYNC-INIT] device=~A since=~S effective=~S pending=~D~%"
                         device-id since effective-since pending-count)
                 (force-output)
                 (llog:info "Sync init" :device-id device-id :since since
                            :effective-since effective-since :pending pending-count)
                 (when *sync-debug* (format t "~&[SYNC-DEBUG] Pending changes to send: ~D~%" pending-count))

                 ;; Send SyncAck
                 (let ((ack-msg (make-sync-ack-message (now-iso) pending-count)))
                   (when *sync-debug*
                     (format t "~&[SYNC-DEBUG] Sending ACK (server-time=~A, pending=~D)~%"
                             (now-iso) pending-count)
                     (let ((ack-bytes (ag-proto:serialize-to-bytes ack-msg)))
                       (format t "~&[SYNC-DEBUG] ACK serialized: ~D bytes~%"
                               (length ack-bytes))))
                   (handler-case
                       (progn
                         (ag-grpc:stream-send stream ack-msg)
                         (when *sync-debug* (format t "~&[SYNC-DEBUG] ACK sent successfully~%")))
                     (error (e)
                       (llog:error "Failed to send ACK" :error (princ-to-string e))
                       (return-from sync-handler))))

                 ;; Send all pending changes (using the row's actual valid_from timestamp)
                 (dolist (row rows)
                   (let* ((todo (db-row-to-todo row))
                          (valid-from (gethash "valid_from" row))
                          (change-msg (make-sync-upsert-message-with-timestamp
                                       (get-device-id) todo valid-from)))
                     (handler-case
                         (ag-grpc:stream-send stream change-msg)
                       (error (e)
                         (llog:error "Failed to send change" :error (princ-to-string e))
                         (return-from sync-handler)))))

                 ;; Send all settings
                 (let ((settings-hash (db-load-all-settings :user-id username)))
                   (when (> (hash-table-count settings-hash) 0)
                     (let ((settings-msg (make-sync-settings-message (get-device-id) settings-hash)))
                       (when *sync-debug*
                         (format t "~&[SYNC-DEBUG] Sending ~D settings~%"
                                 (hash-table-count settings-hash)))
                       (handler-case
                           (ag-grpc:stream-send stream settings-msg)
                         (error (e)
                           (llog:error "Failed to send settings" :error (princ-to-string e))
                           (return-from sync-handler))))))

                 ;; Send list definitions and items if client supports lists
                 ;; Always send ALL current lists (not filtered by since) because:
                 ;; 1. Lists are a small dataset
                 ;; 2. Avoids missed lists when capability is newly added
                 ;; 3. The client handler is idempotent (supersedes existing rows)
                 (when client-has-lists
                   ;; Ensure the built-in Groceries list exists for this user
                   (ensure-default-lists :user-id username)
                   (let ((list-def-rows (db-load-current-list-rows-since "1970-01-01T00:00:00Z" :user-id username)))
                     (when *sync-debug*
                       (format t "~&[SYNC-DEBUG] Sending ~D list definitions~%" (length list-def-rows)))
                     ;; Send definitions first (items reference list_id)
                     (dolist (row list-def-rows)
                       (handler-case
                           (let* ((list-def (row-to-list-definition
                                             (list (gethash "row_id" row 0)
                                                   (gethash "id" row)
                                                   (gethash "name" row)
                                                   (gethash "description" row)
                                                   (gethash "sections" row)
                                                   (gethash "created_at" row)
                                                   (gethash "device_id" row)
                                                   (gethash "valid_from" row)
                                                   (gethash "valid_to" row))))
                                  (change-msg (make-sync-list-upsert-message
                                               (get-device-id) list-def)))
                             (ag-grpc:stream-send stream change-msg))
                         (error (e)
                           (llog:error "Failed to send list definition" :error (princ-to-string e))
                           (return-from sync-handler)))))
                   ;; Then send list items (also send ALL, same reasoning as definitions)
                   (let ((item-rows (db-load-current-list-item-rows-since "1970-01-01T00:00:00Z" :user-id username)))
                     (when *sync-debug*
                       (format t "~&[SYNC-DEBUG] Sending ~D list items~%" (length item-rows)))
                     (dolist (row item-rows)
                       (let* ((item (row-to-list-item
                                     (list (gethash "row_id" row 0)
                                           (gethash "id" row)
                                           (gethash "list_id" row)
                                           (gethash "title" row)
                                           (gethash "section" row)
                                           (if (gethash "checked" row) 1 0)
                                           (gethash "notes" row)
                                           (gethash "created_at" row)
                                           (gethash "device_id" row)
                                           (gethash "valid_from" row)
                                           (gethash "valid_to" row))))
                              (change-msg (make-sync-list-item-upsert-message
                                           (get-device-id) item)))
                         (handler-case
                             (ag-grpc:stream-send stream change-msg)
                           (error (e)
                             (llog:error "Failed to send list item" :error (princ-to-string e))
                             (return-from sync-handler))))))) ;; closes when client-has-lists

                 (when *sync-debug*
                   (format t "~&[SYNC-DEBUG] All pending changes sent. Entering receive loop.~%")
                   (force-output))

                 ;; Register this client for receiving broadcasts
                 (register-sync-client stream device-id username)

                 ;; Main receive loop - process incoming changes from this client
                 (loop
                   (when *sync-debug*
                     (format t "~&[SYNC-DEBUG] Calling stream-recv...~%")
                     (force-output))
                   (let ((msg (handler-case
                                  (ag-grpc:stream-recv stream)
                                (error (e)
                                  (llog:error "stream-recv error" :error (princ-to-string e)
                                              :type (princ-to-string (type-of e)))
                                  nil))))
                     (unless msg
                       ;; Client disconnected
                       (when *sync-debug*
                         (format t "~&[SYNC-DEBUG] stream-recv returned nil (client disconnected)~%")
                         (force-output))
                       (return-from sync-handler))

                     (when (eql (proto-msg-case msg) :change)
                       (let ((change (proto-msg-change msg)))
                         (case (change-case change)
                           (:upsert
                            ;; Client is sending an upsert
                            (let* ((proto-data (proto-todo-change-upsert change))
                                   (origin-device-id (proto-todo-change-device-id change))
                                   (change-timestamp (proto-todo-change-timestamp change))
                                   (todo (proto-to-todo proto-data))
                                   (enriched nil))
                              ;; Set device-id from the change envelope (not from local server)
                              (setf (todo-device-id todo) origin-device-id)

                              ;; Try to enrich the TODO only if it's marked as needing enrichment
                              (let ((redirected-to-list nil))
                              (when (and *enrichment-enabled* (todo-enriching-p todo))
                                (handler-case
                                    (let ((enriched-data (enrich-todo-input
                                                          (todo-title todo)
                                                          (todo-description todo))))
                                      (when enriched-data
                                        ;; Check if LLM says this belongs on a list
                                        (let ((list-name (getf enriched-data :list-name))
                                              (list-section (getf enriched-data :list-section))
                                              (list-items (getf enriched-data :list-items)))
                                          (if (and list-name (stringp list-name) (> (length list-name) 0))
                                              ;; Redirect to list: create list item(s), delete TODO
                                              (let ((list-def (db-find-list-by-name list-name)))
                                                (if list-def
                                                    (let ((items-to-create
                                                            (if list-items
                                                                ;; Multiple items from LLM
                                                                (mapcar (lambda (li)
                                                                          (list :title (or (getf li :title) "")
                                                                                :section (getf li :section)))
                                                                        list-items)
                                                                ;; Single item
                                                                (list (list :title (or (getf enriched-data :title)
                                                                                      (todo-title todo))
                                                                            :section list-section)))))
                                                      (dolist (item-spec items-to-create)
                                                        (let ((item (make-list-item
                                                                      (list-def-id list-def)
                                                                      (getf item-spec :title)
                                                                      :section (getf item-spec :section)
                                                                      :device-id origin-device-id)))
                                                          (db-save-list-item item :valid-from change-timestamp :user-id username)
                                                          (let ((item-msg (make-sync-list-item-upsert-message
                                                                            origin-device-id item)))
                                                            (broadcast-list-change item-msg nil username))
                                                          (llog:info "Created list item from TODO"
                                                                     :title (getf item-spec :title)
                                                                     :list-name list-name
                                                                     :section (getf item-spec :section))))
                                                      ;; Delete the original TODO from originator
                                                      (let ((delete-msg (make-sync-delete-message
                                                                          origin-device-id (todo-id todo))))
                                                        (broadcast-change delete-msg nil username))
                                                      (setf redirected-to-list t)
                                                      (llog:info "Redirected TODO to list"
                                                                 :todo-id (todo-id todo)
                                                                 :list-name list-name
                                                                 :item-count (length items-to-create)))
                                                    ;; List not found - fall through to normal enrichment
                                                    (llog:warn "List not found for redirect, treating as normal TODO"
                                                               :list-name list-name)))
                                              ;; Normal enrichment (no list redirect)
                                              (progn
                                                (setf enriched t)
                                                (when (getf enriched-data :title)
                                                  (setf (todo-title todo) (getf enriched-data :title)))
                                                (when (getf enriched-data :description)
                                                  (setf (todo-description todo) (getf enriched-data :description)))
                                                (when (getf enriched-data :priority)
                                                  (setf (todo-priority todo) (getf enriched-data :priority)))
                                                (when (getf enriched-data :category)
                                                  (let ((tag (category-to-tag (getf enriched-data :category))))
                                                    (when tag
                                                      (pushnew tag (todo-tags todo) :test #'string-equal))))
                                                (when (getf enriched-data :scheduled-date)
                                                  (setf (todo-scheduled-date todo) (getf enriched-data :scheduled-date)))
                                                (when (getf enriched-data :due-date)
                                                  (setf (todo-due-date todo) (getf enriched-data :due-date)))
                                                (when (getf enriched-data :location-info)
                                                  (setf (todo-location-info todo) (getf enriched-data :location-info)))
                                                (when (and (getf enriched-data :url) (not (todo-url todo)))
                                                  (setf (todo-url todo) (getf enriched-data :url)))
                                                (llog:info "Enriched TODO from sync" :id (todo-id todo)))))))
                                  (error (e)
                                    (llog:warn "Enrichment failed, using original TODO"
                                              :id (todo-id todo)
                                              :error (princ-to-string e)))))

                              (unless redirected-to-list
                                ;; Clear enriching-p flag since enrichment is complete
                                (setf (todo-enriching-p todo) nil)

                                ;; Save to local database with original timestamp
                                (db-save-todo todo :valid-from change-timestamp :user-id username)

                                ;; If enriched, create a new change message with enriched data and broadcast
                                ;; When enriched, send to ALL clients including the originator so they get the enrichment
                                ;; When not enriched, exclude the originator (they already have this data)
                                (if enriched
                                    (let ((enriched-msg (make-sync-upsert-message-with-timestamp
                                                         origin-device-id todo change-timestamp)))
                                      (when enriched-msg
                                        (broadcast-change enriched-msg nil username)))
                                    (broadcast-change msg device-id username))))))
                           (:delete-id
                            ;; Client is requesting a delete
                            (let ((todo-id (proto-todo-change-delete-id change)))
                              ;; Delete locally
                              (db-delete-todo todo-id :user-id username)
                              ;; Broadcast to other clients
                              (broadcast-change msg device-id username))))))

                    (when (eql (proto-msg-case msg) :settings-change)
                      (handler-case
                          (let* ((settings-change (proto-msg-settings-change msg))
                                 (origin-device-id (proto-settings-change-device-id settings-change))
                                 (settings-list (proto-settings-change-settings settings-change)))
                            (when *sync-debug*
                              (format t "~&[SYNC-DEBUG] Received settings change from ~A with ~D settings~%"
                                      origin-device-id (length settings-list)))
                            ;; Process each setting with last-write-wins conflict resolution
                            (dolist (setting-data settings-list)
                              (let* ((key (slot-value setting-data 'key))
                                     (value (slot-value setting-data 'value))
                                     (updated-at (slot-value setting-data 'updated-at)))
                                (multiple-value-bind (current-value current-timestamp)
                                    (db-load-setting-with-timestamp key :user-id username)
                                  ;; Only update if incoming timestamp is newer or setting doesn't exist
                                  (when (or (null current-value)
                                           (string< current-timestamp updated-at))
                                    (when *sync-debug*
                                      (format t "~&[SYNC-DEBUG] Updating setting ~A~%" key))
                                    ;; Save with the incoming timestamp to preserve causality
                                    (db-save-setting key value :user-id username :updated-at updated-at)))))
                            ;; Broadcast to other clients (excluding sender)
                            (broadcast-change msg origin-device-id username))
                        (error (e)
                          (llog:error "Settings change processing failed"
                                     :error (princ-to-string e)))))

                    ;; Handle list changes
                    (when (eql (proto-msg-case msg) :list-change)
                      (handler-case
                          (let* ((list-change (proto-msg-list-change msg))
                                 (origin-device-id (proto-list-change-device-id list-change))
                                 (change-timestamp (proto-list-change-timestamp list-change)))
                            (when *sync-debug*
                              (format t "~&[SYNC-DEBUG] Received list change from ~A, case=~A~%"
                                      origin-device-id (slot-value list-change 'change-case)))
                            (case (slot-value list-change 'change-case)
                              (:upsert-list
                               (let* ((proto-data (proto-list-change-upsert-list list-change))
                                      (list-def (proto-to-list-definition proto-data)))
                                 (setf (list-def-device-id list-def) origin-device-id)
                                 (let* ((*suppress-change-notifications* t)
                                        (saved (db-save-list-definition list-def :valid-from change-timestamp :user-id username)))
                                   (if saved
                                       ;; Broadcast to other clients
                                       (broadcast-list-change msg device-id username)
                                       ;; Stale upsert rejected (list was deleted more recently).
                                       ;; Send a delete back to the sender so it learns the list is gone.
                                       (handler-case
                                           (let ((delete-msg (make-sync-list-delete-message
                                                               (get-device-id)
                                                               (list-def-id list-def))))
                                             (ag-grpc:stream-send stream delete-msg)
                                             (llog:info "Sent corrective delete to client"
                                                        :list-id (list-def-id list-def)
                                                        :device device-id))
                                         (error (e)
                                           (llog:warn "Failed to send corrective delete"
                                                      :error (princ-to-string e))))))))
                          (:delete-list-id
                           (let ((list-id (proto-list-change-delete-list-id list-change)))
                             (let ((*suppress-change-notifications* t))
                               (db-delete-list-definition list-id :user-id username))
                             (broadcast-list-change msg device-id username)))
                          (:upsert-item
                           (let* ((proto-data (proto-list-change-upsert-item list-change))
                                  (item (proto-to-list-item proto-data)))
                             (setf (list-item-device-id item) origin-device-id)
                             (let ((*suppress-change-notifications* t))
                               (db-save-list-item item :valid-from change-timestamp :user-id username))
                             (broadcast-list-change msg device-id username)))
                          (:delete-item-id
                           (let ((item-id (proto-list-change-delete-item-id list-change)))
                             (let ((*suppress-change-notifications* t))
                               (db-delete-list-item item-id :user-id username))
                             (broadcast-list-change msg device-id username)))))
                        (error (e)
                          (llog:error "List change processing failed"
                                     :error (princ-to-string e)))))

                    (when (eql (proto-msg-case msg) :reset)
                      (let* ((reset (proto-msg-reset msg))
                             (origin-device-id (proto-sync-reset-device-id reset))
                             (reset-to (proto-sync-reset-reset-to reset)))
                        (llog:info "Sync reset requested" :device-id origin-device-id
                                   :reset-to reset-to)
                        (when *sync-debug*
                          (format t "~&[SYNC-DEBUG] Received reset request from ~A (reset-to: ~A)~%"
                                  origin-device-id reset-to))
                        ;; Broadcast reset to all connected clients (including sender)
                        ;; This tells all clients to clear their last-sync timestamps
                        (broadcast-change msg nil username))))))))) ; close loop, let (pending changes), block

      ;; Cleanup on exit
      (when *sync-debug*
        (format t "~&[SYNC-DEBUG] Handler exiting, cleanup. device-id=~A~%" client-device-id)
        (force-output))
      (when client-device-id
        (unregister-sync-client-by-stream stream client-device-id)))))

;;── Helper: Convert DB row hash table to todo ─────────────────────────────────

(defun db-row-to-todo (ht)
  "Convert a DB row hash table (from db-load-rows-since) back to a todo object.
   Note: This differs from storage.lisp's hash-table-to-todo because DB rows
   store tags/location_info as JSON strings, not parsed structures."
  (let ((tags-json (gethash "tags" ht)))
    (make-instance 'todo
                   :id (gethash "id" ht)
                   :title (gethash "title" ht)
                   :description (let ((d (gethash "description" ht)))
                                  (unless (or (null d) (eq d 'null)) d))
                   :priority (intern (string-upcase (gethash "priority" ht)) :keyword)
                   :status (intern (string-upcase (gethash "status" ht)) :keyword)
                   :scheduled-date (parse-timestamp (gethash "scheduled_date" ht))
                   :due-date (parse-timestamp (gethash "due_date" ht))
                   :tags (when (and tags-json (not (eq tags-json 'null)) (stringp tags-json))
                           (coerce (jzon:parse tags-json) 'list))
                   :location-info (let ((loc (gethash "location_info" ht)))
                                    (when (and loc (not (eq loc 'null)) (stringp loc))
                                      (let ((ht (jzon:parse loc)))
                                        (when (hash-table-p ht)
                                          (list :name (gethash "name" ht)
                                                :address (gethash "address" ht)
                                                :phone (gethash "phone" ht)
                                                :map-url (gethash "map_url" ht)
                                                :website (gethash "website" ht))))))
                   :url (let ((u (gethash "url" ht)))
                          (unless (or (null u) (eq u 'null)) u))
                   :device-id (gethash "device_id" ht)
                   :repeat-interval (let ((i (gethash "repeat_interval" ht)))
                                      (unless (or (null i) (eq i 'null)) i))
                   :repeat-unit (let ((u (gethash "repeat_unit" ht)))
                                  (when (and u (not (eq u 'null)) (stringp u))
                                    (intern (string-upcase u) :keyword)))
                   :attachment-hashes (let ((ah (gethash "attachment_hashes" ht)))
                                        (when (and ah (not (eq ah 'null)) (stringp ah))
                                          (coerce (jzon:parse ah) 'list)))
                   :created-at (lt:parse-timestring (gethash "created_at" ht))
                   :completed-at (parse-timestamp (gethash "completed_at" ht)))))

;;── Server Control ────────────────────────────────────────────────────────────

(defun start-grpc-sync-server (&key (port *grpc-port*) (host "0.0.0.0") (require-tls t))
  "Start the gRPC sync server on the specified port.
   When REQUIRE-TLS is true (default), mTLS is required and certificates must be initialized.
   Returns the server instance."
  (when *grpc-server*
    (stop-grpc-sync-server))

  ;; Check for mTLS certificates
  (let ((use-tls (and require-tls (ca-initialized-p) (server-cert-initialized-p))))
    (when (and require-tls (not use-tls))
      (error "mTLS certificates not initialized but require-tls is true.~%~
              Run 'cloodoo cert init' to set up secure sync.~%~
              Pass :require-tls nil to explicitly start without TLS (NOT recommended)."))

    ;; Disable ENABLE_PUSH (deprecated in RFC 9113, gRPC doesn't use server push)
    (let ((push-setting (assoc ag-http2::+settings-enable-push+
                               ag-http2::*default-settings*)))
      (when push-setting
        (setf (rest push-setting) 0)))

    (setf *grpc-server*
          (if use-tls
              (ag-grpc:make-grpc-server port
                                        :host host
                                        :tls t
                                        :tls-certificate (namestring (server-cert-file))
                                        :tls-key (namestring (server-key-file))
                                        :tls-verify-client t
                                        :tls-ca-certificate (pure-tls::make-trust-store-from-sources
                                                             (namestring (ca-cert-file)) nil))
              (ag-grpc:make-grpc-server port :host host)))

    ;; Register the SyncStream handler
    (ag-grpc:server-register-handler
     *grpc-server*
     "/cloodoo.TodoSync/SyncStream"
     #'handle-sync-stream
     :request-type 'proto-sync-message
     :response-type 'proto-sync-message
     :client-streaming t
     :server-streaming t)

    ;; Register the AttachmentService handlers
    (register-attachment-service *grpc-server*)

    ;; Register change hooks so local TUI changes are broadcast to sync clients
    (setf *todo-change-hook* #'notify-todo-changed)
    (setf *todo-delete-hook* #'notify-todo-deleted)
    (setf *settings-change-hook* #'notify-settings-changed)

    ;; Initialize enrichment for automatic TODO enrichment
    (when *enrichment-enabled*
      (handler-case
          (progn
            (init-enrichment)
            (llog:info "Enrichment initialized for sync server"))
        (error (e)
          (llog:warn "Failed to initialize enrichment, will continue without it"
                    :error (princ-to-string e)))))

    (llog:info "Starting gRPC sync server" :host host :port port)
    (format t "~&gRPC sync server starting on ~A:~A~%" host port)
    (format t "~&Device ID: ~A~%" (get-device-id))
    (if use-tls
        (format t "~&mTLS: enabled (client certificates required)~%")
        (format t "~&mTLS: disabled (plaintext mode)~%"))

    ;; Start server in a background thread
    (bt:make-thread
     (lambda ()
       (handler-case
           (ag-grpc:server-start *grpc-server*)
         (error (e)
           (llog:error "gRPC server error" :error (princ-to-string e)))))
     :name "grpc-sync-server")

    *grpc-server*))

(defun stop-grpc-sync-server ()
  "Stop the gRPC sync server."
  (when *grpc-server*
    (llog:info "Stopping gRPC sync server")
    ;; Unregister change hooks
    (setf *todo-change-hook* nil)
    (setf *todo-delete-hook* nil)
    (setf *settings-change-hook* nil)
    ;; Stop the server
    (handler-case
        (ag-grpc:server-stop *grpc-server*)
      (error (e)
        (llog:warn "Error stopping gRPC server" :error (princ-to-string e))))
    (setf *grpc-server* nil)
    ;; Clear connected clients
    (bt:with-lock-held (*clients-lock*)
      (setf *connected-clients* nil))
    (format t "~&gRPC sync server stopped.~%")))

;;── Notify Connected Clients of Local Changes ─────────────────────────────────

(defun notify-todo-changed (todo)
  "Notify all connected sync clients about a todo change.
   Call this when a todo is created or modified locally."
  (when *connected-clients*
    (let ((msg (make-sync-upsert-message (get-device-id) todo)))
      (broadcast-change msg))))

(defun notify-todo-deleted (todo-id)
  "Notify all connected sync clients about a todo deletion.
   Call this when a todo is deleted locally."
  (when *connected-clients*
    (let ((msg (make-sync-delete-message (get-device-id) todo-id)))
      (broadcast-change msg))))

(defun notify-settings-changed (key value)
  "Notify all connected sync clients about a setting change.
   Call this when a setting is modified locally."
  (declare (ignore key))  ; We send all settings for simplicity
  (declare (ignore value))
  (when *connected-clients*
    (let* ((settings-hash (db-load-all-settings))
           (msg (make-sync-settings-message (get-device-id) settings-hash)))
      (broadcast-change msg))))

;;══════════════════════════════════════════════════════════════════════════════
;;  SYNC CLIENT - Connect to a remote sync server
;;══════════════════════════════════════════════════════════════════════════════

(defvar *sync-client-channel* nil
  "The gRPC channel for the sync client connection.")

(defvar *sync-client-stream* nil
  "The bidirectional stream for sync client.")

(defvar *sync-client-thread* nil
  "Background thread for receiving sync messages.")

(defvar *sync-client-running* nil
  "Flag to control the client receive loop.")

(defvar *sync-client-status* :disconnected
  "Current sync client status: :disconnected, :connecting, :connected, :error.")

(defvar *sync-client-error* nil
  "Last error message from sync client.")

(defvar *sync-model-ref* nil
  "Reference to the app model for updating sync status.")

(defvar *sync-program-ref* nil
  "Reference to the TUI program for triggering redraws after sync updates.")

(defvar *sync-pending-count* 0
  "Number of pending changes expected from server during initial sync.")

(defvar *sync-received-count* 0
  "Number of changes received so far during initial sync.")

(defun notify-tui-refresh ()
  "Send a sync-refresh message to the TUI program to trigger a redraw."
  (when *sync-program-ref*
    (tui:send *sync-program-ref* (make-instance 'sync-refresh-msg))))

(defun notify-tui-reload ()
  "Send a sync-reload message to the TUI program to reload todos from database."
  (when *sync-program-ref*
    (tui:send *sync-program-ref* (make-instance 'sync-reload-msg))))

;;── Sync Client Status ────────────────────────────────────────────────────────

(defun sync-client-connected-p ()
  "Check if the sync client is connected."
  (eql *sync-client-status* :connected))

(defun update-sync-status (status &optional error-msg)
  "Update the sync client status and optionally the model."
  (setf *sync-client-status* status)
  (setf *sync-client-error* error-msg)
  (when *sync-model-ref*
    (setf (model-sync-status *sync-model-ref*) status)
    (when error-msg
      (setf (model-sync-error-message *sync-model-ref*) error-msg))
    (notify-tui-refresh)))

;;── Sync Client Connection ────────────────────────────────────────────────────

(defvar *sync-connector-host* nil
  "Host for the sync connector thread.")
(defvar *sync-connector-port* nil
  "Port for the sync connector thread.")
(defvar *sync-connector-cert* nil
  "Client certificate path for the sync connector thread.")
(defvar *sync-connector-key* nil
  "Client key path for the sync connector thread.")

(defun start-sync-client (host port &key model program client-certificate client-key)
  "Connect to a remote sync server as a client.
   Returns immediately — all connection logic runs in a background thread
   that handles connect, receive, and reconnect with exponential backoff.
   HOST is the server address, PORT is the gRPC port.
   MODEL is optional app-model to update with sync status.
   PROGRAM is optional TUI program for triggering redraws.
   CLIENT-CERTIFICATE - Path to client certificate for mTLS.
   CLIENT-KEY - Path to client private key for mTLS."
  (when *sync-client-channel*
    (stop-sync-client))

  (setf *sync-model-ref* model)
  (setf *sync-program-ref* program)
  (setf *sync-client-running* t)
  (setf *sync-connector-host* host)
  (setf *sync-connector-port* port)
  (setf *sync-connector-cert* client-certificate)
  (setf *sync-connector-key* client-key)
  (update-sync-status :connecting)

  (when model
    (setf (model-sync-server-address model) (format nil "~A:~A" host port)))

  ;; Register hooks so local changes are sent even while reconnecting
  (setf *todo-change-hook* #'notify-sync-todo-changed)
  (setf *todo-delete-hook* #'notify-sync-todo-deleted)
  (setf *settings-change-hook* #'notify-sync-settings-changed)

  ;; All blocking work happens in the connector thread
  (setf *sync-client-thread*
        (bt:make-thread #'sync-connector-loop :name "sync-connector"))

  (llog:info "Sync client started (non-blocking)" :host host :port port)
  t)

(defun sync-connector-loop ()
  "Background thread that connects, receives, and reconnects with backoff.
   Runs until *sync-client-running* is set to nil."
  (let ((host *sync-connector-host*)
        (port *sync-connector-port*)
        (client-certificate *sync-connector-cert*)
        (client-key *sync-connector-key*)
        (backoff-delay 1)
        (max-backoff 30))
    (let ((use-tls (and client-certificate client-key
                        (probe-file client-certificate)
                        (probe-file client-key))))
      (loop while *sync-client-running* do
        (handler-case
            (progn
              (update-sync-status :connecting)
              (llog:info "Connecting to sync server" :host host :port port :tls use-tls)

              ;; Create gRPC channel with no timeout (bidirectional stream is long-lived)
              ;; and keepalive PINGs every 20s to prevent NAT/firewall idle drops
              (setf *sync-client-channel*
                    (let ((ka (ag-grpc:make-keepalive-config
                               :ping-interval 20
                               :permit-without-calls t)))
                      (if use-tls
                          (ag-grpc:make-channel host port
                                                :tls t
                                                :timeout nil
                                                :keepalive ka
                                                :tls-client-certificate (namestring client-certificate)
                                                :tls-client-key (namestring client-key))
                          (ag-grpc:make-channel host port :tls nil :timeout nil
                                                :keepalive ka))))

              ;; Create stub and stream
              (let ((stub (make-todo-sync-stub *sync-client-channel*)))
                (setf *sync-client-stream* (todo-sync-sync-stream stub)))

              ;; Send init message with last known sync timestamp
              (let* ((client-time (now-iso))
                     (since (load-last-sync-timestamp))
                     (init-msg (make-sync-init-message (get-device-id) since client-time)))
                (ag-grpc:stream-send *sync-client-stream* init-msg)
                (llog:info "Sent sync init" :device-id (get-device-id)
                           :since (if (zerop (length since)) "(full sync)" since)
                           :client-time client-time))

              ;; Reset backoff on successful connection
              (setf backoff-delay 1)

              ;; Mark connection as having a reader thread so flow control
              ;; uses CV-wait instead of inline reads (prevents read contention)
              (setf (ag-http2:connection-reader-thread-active-p
                     (ag-grpc:channel-connection *sync-client-channel*)) t)

              ;; Receive loop — blocks until stream ends or error
              (loop while *sync-client-running*
                    do (handler-case
                           (let ((msg (ag-grpc:stream-read-message *sync-client-stream*)))
                             (cond (msg
                                    (handle-sync-client-message msg))
                                   (t
                                    (llog:info "Sync stream ended (nil message)")
                                    (return))))
                         (error (e)
                           (llog:error "Sync receive error" :error (princ-to-string e))
                           (return)))))
          (error (e)
            (let ((error-msg (princ-to-string e)))
              ;; Check if this is a deliberate shutdown, not an actual error
              (cond ((search "sync client shutting down" error-msg)
                     (llog:info "Sync client shutdown signal received")
                     (return))
                    (t
                     (llog:error "Sync connection error" :error error-msg)
                     (update-sync-status :error error-msg)
                     (notify-tui-refresh))))))

        ;; Cleanup before retry
        (cleanup-sync-client)

        ;; Sleep with backoff before retrying (unless told to stop).
        ;; Sleep in short increments so stop-sync-client isn't blocked.
        (when *sync-client-running*
          (llog:info "Reconnecting in ~Ds" :delay backoff-delay)
          (let ((remaining backoff-delay))
            (loop while (and *sync-client-running* (> remaining 0))
                  do (let ((step (min 0.5 remaining)))
                       (sleep step)
                       (decf remaining step))))
          (setf backoff-delay (min (* backoff-delay 2) max-backoff))))))
  (llog:info "Sync connector loop exited"))

(defun stop-sync-client ()
  "Disconnect from the sync server."
  (llog:info "Stopping sync client")
  (setf *sync-client-running* nil)

  ;; Unregister hooks
  (setf *todo-change-hook* nil)
  (setf *todo-delete-hook* nil)
  (setf *settings-change-hook* nil)

  ;; Close channel to unblock stream-read-message
  ;; This will cause stream-read-message to return nil, exiting the loop
  (cleanup-sync-client)

  ;; Wait for the thread to finish (with timeout to prevent hanging on exit)
  (when (and *sync-client-thread* (bt:thread-alive-p *sync-client-thread*))
    (handler-case
        (let ((timeout 2)  ; 2 second timeout
              (elapsed 0)
              (step 0.1))  ; Check every 100ms
          (loop while (and (< elapsed timeout) (bt:thread-alive-p *sync-client-thread*))
                do (sleep step)
                   (incf elapsed step))
          (when (bt:thread-alive-p *sync-client-thread*)
            (llog:warn "Sync thread did not exit cleanly within ~Ds, destroying thread" :timeout timeout)
            (bt:destroy-thread *sync-client-thread*)))
      (error (e)
        (llog:warn "Error waiting for sync thread" :error (princ-to-string e)))))
  (setf *sync-client-thread* nil)

  (update-sync-status :disconnected)
  (setf *sync-model-ref* nil)
  (setf *sync-program-ref* nil))

(defun cleanup-sync-client ()
  "Clean up sync client resources (channel and stream)."
  (when *sync-client-stream*
    (handler-case
        (ag-grpc:stream-close-send *sync-client-stream*)
      (error (e)
        (llog:warn "Error closing stream" :error (princ-to-string e)))))
  (setf *sync-client-stream* nil)

  (when *sync-client-channel*
    (handler-case
        (ag-grpc:channel-close *sync-client-channel*)
      (error (e)
        (llog:warn "Error closing channel" :error (princ-to-string e)))))
  (setf *sync-client-channel* nil))

(defun download-attachment-from-server (hash)
  "Download an attachment from the sync server by hash.
   Returns T if successful, NIL if failed or not connected."
  (unless *sync-client-channel*
    (llog:warn "Cannot download attachment: not connected to sync server")
    (return-from download-attachment-from-server nil))

  (handler-case
      (let* ((request (make-instance 'proto-attachment-download-request :hash hash))
             ;; Use call-server-stream directly (the generated stub uses wrong function name)
             (stream (ag-grpc:call-server-stream
                      *sync-client-channel*
                      "/cloodoo.AttachmentService/DownloadAttachment"
                      request
                      :response-type 'proto-attachment-download-response))
             (metadata nil)
             (chunks nil))
        ;; Read all responses from the stream
        (loop
          (let ((response (ag-grpc:stream-receive-message stream)))
            (unless response
              (return))  ; End of stream
            ;; Check for error
            (let ((error-msg (proto-error response)))
              (when (and error-msg (plusp (length error-msg)))
                (llog:warn "Attachment download error" :hash hash :error error-msg)
                (return-from download-attachment-from-server nil)))
            ;; Collect metadata or chunk based on response-case
            (case (slot-value response 'response-case)
              (:metadata
               (setf metadata (proto-metadata response)))
              (:chunk
               (let ((chunk (proto-chunk response)))
                 (when (and chunk (plusp (length chunk)))
                   (push chunk chunks)))))))
        ;; Assemble and store the attachment
        (when (and metadata chunks)
          (let* ((content (apply #'concatenate '(vector (unsigned-byte 8))
                                 (nreverse chunks)))
                 (filename (proto-filename metadata))
                 (mime-type (proto-mime-type metadata))
                 (size (length content)))
            ;; Store in local database
            (with-db (db)
              (sqlite:execute-non-query db
                "INSERT OR IGNORE INTO attachments (hash, content, filename, mime_type, size, created_at)
                 VALUES (?, ?, ?, ?, ?, ?)"
                hash content filename mime-type size (lt:format-timestring nil (lt:now))))
            (llog:info "Downloaded attachment" :hash hash :size size)
            t)))
    (error (e)
      (llog:error "Failed to download attachment" :hash hash :error (princ-to-string e))
      nil)))

(defun make-upload-channel ()
  "Create a dedicated gRPC channel for attachment uploads.
   Uses the same TLS credentials as the sync client but establishes a separate
   HTTP/2 connection, avoiding reader-thread conflicts with the bidi sync stream."
  (if (and *sync-connector-cert* *sync-connector-key*)
      (ag-grpc:make-channel *sync-connector-host* *sync-connector-port*
                            :tls t
                            :tls-client-certificate (namestring *sync-connector-cert*)
                            :tls-client-key (namestring *sync-connector-key*)
                            :timeout 60)
      (ag-grpc:make-channel *sync-connector-host* *sync-connector-port*
                            :tls nil
                            :timeout 60)))

(defun upload-all-attachments-to-server ()
  "Upload all attachments from the local database to the sync server.
   Useful for syncing attachments that were created before automatic upload was implemented.
   Creates a dedicated gRPC channel for uploads to avoid conflicts with the sync stream."
  (unless *sync-client-channel*
    (llog:warn "Cannot upload attachments: not connected to sync server")
    (return-from upload-all-attachments-to-server nil))

  (let ((upload-channel (make-upload-channel)))
    (unwind-protect
         (with-db (db)
           (let* ((stmt (sqlite:prepare-statement db "SELECT hash, filename, size FROM attachments"))
                  (uploaded 0)
                  (failed 0))
             (sqlite:step-statement stmt)
             (loop while (sqlite:statement-column-value stmt 0)
                   do (let ((hash (sqlite:statement-column-value stmt 0))
                            (filename (sqlite:statement-column-value stmt 1))
                            (size (sqlite:statement-column-value stmt 2)))
                        (llog:info "Uploading attachment" :hash hash :filename filename :size size)
                        (if (upload-attachment-to-server hash db upload-channel)
                            (incf uploaded)
                            (incf failed))
                        (sqlite:step-statement stmt)))
             (sqlite:finalize-statement stmt)
             (llog:info "Attachment upload complete" :uploaded uploaded :failed failed)
             (list :uploaded uploaded :failed failed)))
      (handler-case (ag-grpc:channel-close upload-channel)
        (error () nil)))))

(defun upload-attachment-to-server (hash &optional existing-db upload-channel)
  "Upload an attachment to the sync server by hash.
   EXISTING-DB, if provided, is used instead of opening a new db connection.
   UPLOAD-CHANNEL, if provided, is used instead of creating a new dedicated channel.
   Returns T if successful, NIL if failed or not connected."
  (unless (or *sync-client-channel* *sync-connector-host*)
    (llog:warn "Cannot upload attachment: not connected to sync server")
    (return-from upload-attachment-to-server nil))

  (let ((channel (or upload-channel (make-upload-channel)))
        (own-channel-p (null upload-channel)))
    (unwind-protect
         (handler-case
             (flet ((do-upload (db)
                      ;; Load attachment from local database
                      (multiple-value-bind (stored-hash content filename mime-type size created-at)
                          (resolve-attachment db hash)
                        (declare (ignore created-at))
                        (unless stored-hash
                          (llog:warn "Attachment not found in local database" :hash hash)
                          (return-from upload-attachment-to-server nil))

                        (let* ((metadata (make-instance 'proto-attachment-meta
                                                        :hash hash
                                                        :filename filename
                                                        :mime-type mime-type
                                                        :size size))
                               ;; Scale timeout by size: 60s base + 30s per MB
                               (upload-timeout (+ 60 (* 30 (ceiling size (* 1024 1024)))))
                               (stream (ag-grpc:call-client-streaming
                                        channel
                                        "/cloodoo.AttachmentService/UploadAttachment"
                                        :response-type 'proto-attachment-upload-response
                                        :timeout upload-timeout)))

                          ;; Verify content hash matches before sending
                          (let ((verify-hash (ironclad:byte-array-to-hex-string
                                             (ironclad:digest-sequence :sha256 content))))
                            (llog:info "Starting attachment upload"
                                       :hash hash :size size
                                       :content-type (type-of content)
                                       :content-length (length content)
                                       :verify-hash verify-hash
                                       :hash-match (string-equal hash verify-hash)))

                          ;; Send metadata as first message
                          (let ((meta-msg (make-instance 'proto-attachment-upload-request)))
                            (setf (slot-value meta-msg 'metadata) metadata)
                            (setf (slot-value meta-msg 'request-case) :metadata)
                            (ag-grpc:stream-send stream meta-msg))

                          ;; Send content in chunks (16KB each)
                          (let ((chunk-size 16384)
                                (offset 0)
                                (total-size (length content)))
                            (loop while (< offset total-size)
                                  do (let* ((remaining (- total-size offset))
                                            (current-chunk-size (min chunk-size remaining))
                                            (chunk (subseq content offset (+ offset current-chunk-size)))
                                            (chunk-msg (make-instance 'proto-attachment-upload-request)))
                                       (setf (slot-value chunk-msg 'chunk) chunk)
                                       (setf (slot-value chunk-msg 'request-case) :chunk)
                                       (ag-grpc:stream-send stream chunk-msg)
                                       (incf offset current-chunk-size))))

                          ;; Close send and receive response
                          (let ((response (ag-grpc:stream-close-and-recv stream)))
                            (unless response
                              (llog:error "No response from server for attachment upload" :hash hash)
                              (return-from upload-attachment-to-server nil))

                            ;; Check for error in response
                            (let ((error-msg (proto-attachment-upload-response-error response)))
                              (when (and error-msg (plusp (length error-msg)))
                                (llog:error "Attachment upload failed" :hash hash :error error-msg)
                                (return-from upload-attachment-to-server nil)))

                            (llog:info "Attachment uploaded successfully" :hash hash)
                            t)))))
               (if existing-db
                   (do-upload existing-db)
                   (with-db (db)
                     (do-upload db))))
           (error (e)
             (llog:error "Failed to upload attachment" :hash hash :error (princ-to-string e))
             nil))
      ;; Cleanup: close own channel even on non-local exit (return-from)
      (when own-channel-p
        (handler-case (ag-grpc:channel-close channel)
          (error () nil))))))

(defun handle-sync-client-message (msg)
  "Handle a message received from the sync server."
  (case (proto-msg-case msg)
    (:ack
     (let* ((ack (proto-msg-ack msg))
            (server-time (proto-sync-ack-server-time ack))
            (pending (proto-sync-ack-pending-changes ack))
            (error-msg (proto-sync-ack-error ack)))
       (cond ((and error-msg (plusp (length error-msg)))
              (llog:error "Server rejected connection" :error error-msg)
              (update-sync-status :error error-msg)
              (setf *sync-client-running* nil))
             (t
              (llog:info "Sync connected" :server-time server-time :pending pending)
              (update-sync-status :connected)
             ;; Load old last-sync timestamp before updating it
             (let ((old-last-sync (load-last-sync-timestamp)))
               ;; Save server time for next reconnect (avoid full resync)
               (when (and server-time (plusp (length server-time)))
                 (save-last-sync-timestamp server-time))

               (setf *sync-pending-count* pending)
               (setf *sync-received-count* 0)
               (llog:info "Expecting pending changes" :count pending)
               (when *sync-model-ref*
                 (setf (model-sync-pending-count *sync-model-ref*) pending))

               ;; Send local changes that occurred since last sync
               ;; This handles changes that failed to send due to connection errors
               ;; Also handles full resync when old-last-sync is empty
               (handler-case
                   (let* ((effective-since (if (plusp (length old-last-sync))
                                               old-last-sync
                                               "1970-01-01T00:00:00Z"))
                          (local-changes (db-load-current-rows-since effective-since))
                          (local-count (length local-changes)))
                     (when (plusp local-count)
                       (llog:info "Sending local changes" :count local-count :since effective-since)
                       (dolist (row local-changes)
                         (let* ((todo (db-row-to-todo row))
                                (valid-from (gethash "valid_from" row))
                                (change-msg (make-sync-upsert-message-with-timestamp
                                             (get-device-id) todo valid-from)))
                           (handler-case
                               (ag-grpc:stream-send *sync-client-stream* change-msg)
                             (error (e)
                               (llog:error "Failed to send local change"
                                          :id (todo-id todo)
                                          :error (princ-to-string e))))))))
                 (error (e)
                   (llog:warn "Failed to load/send local changes" :error (princ-to-string e))))

               ;; Send local settings to server on connect
               (let ((settings-hash (db-load-all-settings)))
                 (when (> (hash-table-count settings-hash) 0)
                   (sync-client-send-settings nil nil)))

               ;; Send local list changes to server on connect
               (handler-case
                   (let* ((effective-since-2 (if (plusp (length old-last-sync))
                                                  old-last-sync
                                                  "1970-01-01T00:00:00Z"))
                          ;; Send list definitions first, then items
                          (list-def-rows (db-load-current-list-rows-since effective-since-2))
                          (list-item-rows (db-load-current-list-item-rows-since effective-since-2)))
                     (when (plusp (length list-def-rows))
                       (llog:info "Sending local list definitions" :count (length list-def-rows))
                       (dolist (row list-def-rows)
                         (let* ((list-def (row-to-list-definition
                                           (list 0
                                                 (gethash "id" row)
                                                 (gethash "name" row)
                                                 (gethash "description" row)
                                                 (gethash "sections" row)
                                                 (gethash "created_at" row)
                                                 (gethash "device_id" row)
                                                 (gethash "valid_from" row)
                                                 (gethash "valid_to" row))))
                                (change-msg (make-sync-list-upsert-message
                                             (get-device-id) list-def)))
                           (handler-case
                               (ag-grpc:stream-send *sync-client-stream* change-msg)
                             (error (e)
                               (llog:error "Failed to send local list definition"
                                          :error (princ-to-string e)))))))
                     (when (plusp (length list-item-rows))
                       (llog:info "Sending local list items" :count (length list-item-rows))
                       (dolist (row list-item-rows)
                         (let* ((item (row-to-list-item
                                       (list 0
                                             (gethash "id" row)
                                             (gethash "list_id" row)
                                             (gethash "title" row)
                                             (gethash "section" row)
                                             (if (gethash "checked" row) 1 0)
                                             (gethash "notes" row)
                                             (gethash "created_at" row)
                                             (gethash "device_id" row)
                                             (gethash "valid_from" row)
                                             (gethash "valid_to" row))))
                                (change-msg (make-sync-list-item-upsert-message
                                             (get-device-id) item)))
                           (handler-case
                               (ag-grpc:stream-send *sync-client-stream* change-msg)
                             (error (e)
                               (llog:error "Failed to send local list item"
                                          :error (princ-to-string e))))))))
                 (error (e)
                   (llog:warn "Failed to send local list changes" :error (princ-to-string e)))))))))

    (:change
     (let ((change (proto-msg-change msg)))
       (case (change-case change)
         (:upsert
          (let* ((proto-data (proto-todo-change-upsert change))
                 (todo (proto-to-todo proto-data)))
            (llog:info "Received upsert from server" :id (todo-id todo))
            ;; Server always clears enriching-p after processing, so no need to check
            ;; Suppress notifications to avoid sending the change back
            (let ((*suppress-change-notifications* t))
              (db-save-todo todo))
            ;; Track progress and only refresh when done with initial batch
            (when (> *sync-pending-count* 0)
              (incf *sync-received-count*)
              ;; Yield CPU every 10 writes to keep UI responsive
              (when (zerop (mod *sync-received-count* 10))
                (sleep 0.001))
              (when (>= *sync-received-count* *sync-pending-count*)
                (llog:info "Initial sync complete" :count *sync-received-count*)
                (notify-tui-reload)
                (setf *sync-pending-count* 0 *sync-received-count* 0)))
            ;; For single updates outside initial sync, ask main thread to reload
            (when (zerop *sync-pending-count*)
              (notify-tui-reload))))
         (:delete-id
          (let ((todo-id (proto-todo-change-delete-id change)))
            (llog:info "Received delete from server" :id todo-id)
            ;; Suppress notifications
            (let ((*suppress-change-notifications* t))
              (db-delete-todo todo-id))
            ;; Ask main thread to reload
            (notify-tui-reload))))))

    (:settings-change
     (let* ((settings-change (proto-msg-settings-change msg))
            (settings-list (proto-settings-change-settings settings-change)))
       (llog:info "Received settings change from server" :count (length settings-list))
       ;; Process each setting with last-write-wins conflict resolution
       (dolist (setting-data settings-list)
         (let* ((key (slot-value setting-data 'key))
                (value (slot-value setting-data 'value))
                (updated-at (slot-value setting-data 'updated-at)))
           (multiple-value-bind (current-value current-timestamp)
               (db-load-setting-with-timestamp key)
             ;; Only update if incoming timestamp is newer or setting doesn't exist
             (when (or (null current-value)
                       (string< current-timestamp updated-at))
               (let ((*suppress-change-notifications* t))
                 ;; Save with incoming timestamp (bypass db-save-setting which auto-timestamps)
                 (with-db (db)
                   (sqlite:execute-non-query db
                     "INSERT OR REPLACE INTO app_settings (key, value, updated_at)
                      VALUES (?, ?, ?)"
                     key value updated-at)))
               (llog:info "Applied settings update from server" :key key)))))))

    (:list-change
     (let* ((list-change (proto-msg-list-change msg)))
       (when *sync-debug*
         (format t "~&[SYNC-DEBUG] Client received list change, case=~A~%"
                 (slot-value list-change 'change-case)))
       (case (slot-value list-change 'change-case)
         (:upsert-list
          (let* ((proto-data (proto-list-change-upsert-list list-change))
                 (list-def (proto-to-list-definition proto-data))
                 (change-timestamp (proto-list-change-timestamp list-change)))
            (setf (list-def-device-id list-def)
                  (proto-list-change-device-id list-change))
            (llog:info "Received list definition from server" :name (list-def-name list-def))
            (let ((*suppress-change-notifications* t))
              (db-save-list-definition list-def :valid-from change-timestamp))))
         (:delete-list-id
          (let ((list-id (proto-list-change-delete-list-id list-change)))
            (llog:info "Received list delete from server" :id list-id)
            (let ((*suppress-change-notifications* t))
              (db-delete-list-definition list-id))))
         (:upsert-item
          (let* ((proto-data (proto-list-change-upsert-item list-change))
                 (item (proto-to-list-item proto-data))
                 (change-timestamp (proto-list-change-timestamp list-change)))
            (setf (list-item-device-id item)
                  (proto-list-change-device-id list-change))
            (llog:info "Received list item from server" :title (list-item-title item))
            (let ((*suppress-change-notifications* t))
              (db-save-list-item item :valid-from change-timestamp))))
         (:delete-item-id
          (let ((item-id (proto-list-change-delete-item-id list-change)))
            (llog:info "Received list item delete from server" :id item-id)
            (let ((*suppress-change-notifications* t))
              (db-delete-list-item item-id)))))
       ;; Trigger TUI refresh for list views
       (notify-tui-reload)))

    (:reset
     (let* ((reset (proto-msg-reset msg))
            (origin-device-id (proto-sync-reset-device-id reset))
            (reset-to (proto-sync-reset-reset-to reset)))
       (llog:info "Received sync reset from server" :origin-device-id origin-device-id
                  :reset-to reset-to)
       ;; Clear or set last-sync timestamp as requested
       (if (or (null reset-to) (zerop (length reset-to)))
           (progn
             ;; Full reset - delete last-sync file
             (let ((file (last-sync-file)))
               (when (probe-file file)
                 (delete-file file))
               (llog:info "Last sync timestamp cleared for full resync")))
           (progn
             ;; Partial reset - set to specific timestamp
             (save-last-sync-timestamp reset-to)
             (llog:info "Last sync timestamp reset" :to reset-to)))
       ;; Trigger reconnect to perform the resync
       (llog:info "Disconnecting to trigger resync...")
       (setf *sync-client-running* nil)))

    (otherwise
     (llog:warn "Unknown message from server" :case (proto-msg-case msg)))))

(defun refresh-model-todos (model)
  "Refresh the model's todo list from the database."
  (setf (model-todos model) (load-todos))
  (setf (model-visible-todos-dirty model) t)
  (notify-tui-refresh))

;;── Send Changes to Server ────────────────────────────────────────────────────

(defun sync-client-send-upsert (todo)
  "Send a todo upsert to the sync server.
   Also uploads any attachments that the server doesn't have."
  (when (and *sync-client-stream* (sync-client-connected-p))
    (let ((msg (make-sync-upsert-message (get-device-id) todo)))
      (handler-case
          (progn
            ;; Send the TODO update
            (ag-grpc:stream-send *sync-client-stream* msg)

            ;; Upload attachments if the TODO has any
            (when (todo-attachment-hashes todo)
              (dolist (hash (todo-attachment-hashes todo))
                (llog:info "Uploading attachment for TODO" :todo-id (todo-id todo) :hash hash)
                (upload-attachment-to-server hash))))
        (error (e)
          (llog:error "Failed to send upsert" :error (princ-to-string e)))))))

(defun sync-client-send-delete (todo-id)
  "Send a todo deletion to the sync server."
  (when (and *sync-client-stream* (sync-client-connected-p))
    (let ((msg (make-sync-delete-message (get-device-id) todo-id)))
      (handler-case
          (ag-grpc:stream-send *sync-client-stream* msg)
        (error (e)
          (llog:error "Failed to send delete" :error (princ-to-string e)))))))

;;── Combined Notify Function (Server + Client) ────────────────────────────────

(defun notify-sync-todo-changed (todo)
  "Notify about a todo change - sends to connected sync clients AND to server if connected as client."
  ;; Notify connected clients (if we're running as server)
  (notify-todo-changed todo)
  ;; Send to server (if we're connected as client)
  (sync-client-send-upsert todo))

(defun notify-sync-todo-deleted (todo-id)
  "Notify about a todo deletion - sends to connected sync clients AND to server if connected as client."
  ;; Notify connected clients (if we're running as server)
  (notify-todo-deleted todo-id)
  ;; Send to server (if we're connected as client)
  (sync-client-send-delete todo-id))

(defun sync-client-send-settings (key value)
  "Send settings change to the sync server."
  (declare (ignore key value))  ; We send all settings for simplicity
  (when (and *sync-client-stream* (sync-client-connected-p))
    (let* ((settings-hash (db-load-all-settings))
           (msg (make-sync-settings-message (get-device-id) settings-hash)))
      (handler-case
          (ag-grpc:stream-send *sync-client-stream* msg)
        (error (e)
          (llog:error "Failed to send settings" :error (princ-to-string e)))))))

(defun notify-sync-settings-changed (key value)
  "Notify about a settings change - sends to connected sync clients AND to server if connected as client."
  ;; Notify connected clients (if we're running as server)
  (notify-settings-changed key value)
  ;; Send to server (if we're connected as client)
  (sync-client-send-settings key value))

;;── List Change Sync Functions ────────────────────────────────────────────────

(defun sync-client-send-list-upsert (list-def)
  "Send a list definition upsert to the sync server."
  (when (and *sync-client-stream* (sync-client-connected-p))
    (let ((msg (make-sync-list-upsert-message (get-device-id) list-def)))
      (handler-case
          (ag-grpc:stream-send *sync-client-stream* msg)
        (error (e)
          (llog:error "Failed to send list upsert" :error (princ-to-string e)))))))

(defun sync-client-send-list-delete (list-def-id)
  "Send a list definition deletion to the sync server."
  (when (and *sync-client-stream* (sync-client-connected-p))
    (let ((msg (make-sync-list-delete-message (get-device-id) list-def-id)))
      (handler-case
          (ag-grpc:stream-send *sync-client-stream* msg)
        (error (e)
          (llog:error "Failed to send list delete" :error (princ-to-string e)))))))

(defun sync-client-send-list-item-upsert (item)
  "Send a list item upsert to the sync server."
  (when (and *sync-client-stream* (sync-client-connected-p))
    (let ((msg (make-sync-list-item-upsert-message (get-device-id) item)))
      (handler-case
          (ag-grpc:stream-send *sync-client-stream* msg)
        (error (e)
          (llog:error "Failed to send list item upsert" :error (princ-to-string e)))))))

(defun sync-client-send-list-item-delete (item-id)
  "Send a list item deletion to the sync server."
  (when (and *sync-client-stream* (sync-client-connected-p))
    (let ((msg (make-sync-list-item-delete-message (get-device-id) item-id)))
      (handler-case
          (ag-grpc:stream-send *sync-client-stream* msg)
        (error (e)
          (llog:error "Failed to send list item delete" :error (princ-to-string e)))))))

(defun notify-sync-list-changed (list-def)
  "Notify about a list definition change - broadcasts and sends to server."
  (broadcast-list-change (make-sync-list-upsert-message (get-device-id) list-def) nil)
  (sync-client-send-list-upsert list-def))

(defun notify-sync-list-deleted (list-def-id)
  "Notify about a list deletion - broadcasts and sends to server."
  (broadcast-list-change (make-sync-list-delete-message (get-device-id) list-def-id) nil)
  (sync-client-send-list-delete list-def-id))

(defun notify-sync-list-item-changed (item)
  "Notify about a list item change - broadcasts and sends to server."
  (broadcast-list-change (make-sync-list-item-upsert-message (get-device-id) item) nil)
  (sync-client-send-list-item-upsert item))

(defun notify-sync-list-item-deleted (item-id)
  "Notify about a list item deletion - broadcasts and sends to server."
  (broadcast-list-change (make-sync-list-item-delete-message (get-device-id) item-id) nil)
  (sync-client-send-list-item-delete item-id))

;;── One-Shot Sync (for CLI commands) ──────────────────────────────────────────

(defun cli-sync-todo (todo)
  "Sync a single TODO to the server from a CLI context (no running sync client).
   Opens a temporary bidi-stream, sends init + upsert, then closes.
   Also uploads any attachments. Returns T on success, NIL on failure."
  (handler-case
      (multiple-value-bind (host port server-id)
          (find-paired-sync-config)
        (unless server-id (return-from cli-sync-todo nil))
        (let ((cert (namestring (paired-client-cert-file server-id)))
              (key (namestring (paired-client-key-file server-id))))
          (unless (and (probe-file cert) (probe-file key))
            (return-from cli-sync-todo nil))
          ;; Set connector vars for make-upload-channel
          (setf *sync-connector-host* host
                *sync-connector-port* port
                *sync-connector-cert* cert
                *sync-connector-key* key)
          (let* ((channel (ag-grpc:make-channel host port
                                                :tls t
                                                :tls-client-certificate cert
                                                :tls-client-key key
                                                :timeout 10))
                 (stub (make-todo-sync-stub channel))
                 (stream (todo-sync-sync-stream stub)))
            (unwind-protect
                 (progn
                   ;; Send init
                   (let ((init-msg (make-sync-init-message (get-device-id)
                                                           (load-last-sync-timestamp)
                                                           (now-iso))))
                     (ag-grpc:stream-send stream init-msg))
                   ;; Read ACK
                   (let* ((ack-msg (ag-grpc:stream-read-message stream))
                          (pending (when (and ack-msg (eql (proto-msg-case ack-msg) :ack))
                                    (let* ((ack (proto-msg-ack ack-msg))
                                           (server-time (proto-sync-ack-server-time ack)))
                                      ;; Save server time for future syncs
                                      (when (plusp (length server-time))
                                        (save-last-sync-timestamp server-time))
                                      (proto-sync-ack-pending-changes ack)))))
                     (unless pending
                       (return-from cli-sync-todo nil))
                     ;; Drain exactly pending-count messages from server
                     (dotimes (_ pending)
                       (let ((msg (ag-grpc:stream-read-message stream)))
                         (when msg
                           ;; Apply server changes to local DB silently
                           (let ((*suppress-change-notifications* t))
                             (case (proto-msg-case msg)
                               (:change
                                (let* ((change (proto-msg-change msg))
                                       (change-case (proto-todo-change-change-case change)))
                                  (case change-case
                                    (:upsert
                                     (let ((todo-data (proto-todo-change-upsert change)))
                                       (db-save-todo (proto-todo-to-todo todo-data))))
                                    (:delete-id
                                     (db-delete-todo (proto-todo-change-delete-id change))))))
                               (t nil)))))))
                   ;; Send the TODO upsert
                   (let ((upsert-msg (make-sync-upsert-message (get-device-id) todo)))
                     (ag-grpc:stream-send stream upsert-msg))
                   ;; Upload attachments
                   (when (todo-attachment-hashes todo)
                     (dolist (hash (todo-attachment-hashes todo))
                       (upload-attachment-to-server hash)))
                   ;; Brief pause for server to process
                   (sleep 0.2)
                   t)
              (ignore-errors
                (ag-grpc:stream-close-send stream)
                (ag-grpc:channel-close channel))))))
    (error (e)
      (llog:error "CLI sync failed" :error (princ-to-string e))
      nil)))

;;══════════════════════════════════════════════════════════════════════════════
;;  ATTACHMENT SERVICE - Upload/download attachments via gRPC
;;══════════════════════════════════════════════════════════════════════════════

(defvar *attachment-chunk-size* 8192
  "Size of chunks for streaming attachment transfers (8KB).
Keep small to avoid exhausting HTTP/2 flow control window.")

(defun handle-upload-attachment (ctx stream)
  "Handler for AttachmentService.UploadAttachment RPC.
   Receives streaming chunks from client, stores in attachments table."
  (declare (ignore ctx))
  (let ((metadata nil)
        (content-buffer nil)
        (total-size 0))
    (handler-case
        (block upload-handler
          ;; First message should be metadata
          (let ((first-msg (ag-grpc:stream-recv stream)))
            (unless first-msg
              (let ((resp (make-instance 'proto-attachment-upload-response
                                         :error "No metadata received")))
                (return-from upload-handler resp)))

            ;; Check if it's metadata
            (unless (eql (request-case first-msg) :metadata)
              (let ((resp (make-instance 'proto-attachment-upload-response
                                         :error "First message must be metadata")))
                (return-from upload-handler resp)))

            (setf metadata (proto-attachment-upload-request-metadata first-msg))
            (llog:info "Attachment upload started"
                       :hash (proto-attachment-meta-hash metadata)
                       :filename (proto-attachment-meta-filename metadata)
                       :size (proto-attachment-meta-size metadata)))

          ;; Receive content chunks
          (setf content-buffer (make-array 0 :element-type '(unsigned-byte 8)
                                            :adjustable t :fill-pointer 0))
          (loop
            (let ((msg (ag-grpc:stream-recv stream)))
              (unless msg
                ;; End of stream
                (return))

              (when (eql (request-case msg) :chunk)
                (let ((chunk (proto-attachment-upload-request-chunk msg)))
                  (loop for byte across chunk
                        do (vector-push-extend byte content-buffer))
                  (incf total-size (length chunk))))))

          ;; Coerce adjustable content-buffer to simple array for ironclad
          (let ((simple-content (make-array (length content-buffer)
                                           :element-type '(unsigned-byte 8))))
            (replace simple-content content-buffer)

          ;; Verify hash
          (let* ((received-hash (proto-attachment-meta-hash metadata))
                 (computed-hash (ironclad:byte-array-to-hex-string
                                 (ironclad:digest-sequence :sha256 simple-content))))
            (format t "~&[UPLOAD-DEBUG] total-size=~A buffer-len=~A expected=~A computed=~A~%"
                    total-size (length content-buffer) received-hash computed-hash)
            (force-output)
            (unless (string-equal received-hash computed-hash)
              (format t "~&[UPLOAD-DEBUG] HASH MISMATCH! First 16 bytes: ~A~%"
                      (subseq content-buffer 0 (min 16 (length content-buffer))))
              (force-output)
              (llog:warn "Attachment hash mismatch"
                         :expected received-hash
                         :computed computed-hash)
              (let ((resp (make-instance 'proto-attachment-upload-response
                                         :error "Hash mismatch")))
                (return-from upload-handler resp)))

            ;; Store in database
            (with-db (db)
              (sqlite:execute-non-query db
                "INSERT OR IGNORE INTO attachments (hash, content, filename, mime_type, size, created_at)
                 VALUES (?, ?, ?, ?, ?, ?)"
                received-hash
                simple-content
                (proto-attachment-meta-filename metadata)
                (proto-attachment-meta-mime-type metadata)
                (proto-attachment-meta-size metadata)
                (now-iso)))

            (llog:info "Attachment stored" :hash received-hash :size total-size)

            ;; Return success response
            (make-instance 'proto-attachment-upload-response
                           :hash received-hash))))

      (error (e)
        (llog:error "Attachment upload failed" :error (princ-to-string e))
        (make-instance 'proto-attachment-upload-response
                       :error (princ-to-string e))))))

(defun handle-download-attachment (request ctx stream)
  "Handler for AttachmentService.DownloadAttachment RPC.
   Streams attachment content to client in chunks."
  (declare (ignore request ctx))

  ;; For server-streaming, ag-grpc passes nil as request parameter
  ;; We need to read the request from the stream
  (let* ((req-msg (ag-grpc:stream-recv stream))
         (hash (when req-msg (slot-value req-msg 'hash))))
    (llog:info "ATTACHMENT DOWNLOAD REQUEST" :hash hash)
    (format t "~&[ATTACHMENT-DOWNLOAD] Request for hash: ~A~%" hash)
    (force-output)
    (handler-case
        (with-db (db)
          (multiple-value-bind (stored-hash content filename mime-type size created-at)
              (resolve-attachment db hash)
            (declare (ignore created-at))

            (unless stored-hash
              (llog:warn "Attachment not found" :hash hash)
              (format t "~&[ATTACHMENT-DOWNLOAD] Attachment not found: ~A~%" hash)
              (force-output)
              (let ((resp (make-instance 'proto-attachment-download-response
                                         :error "Attachment not found")))
                (ag-grpc:stream-send stream resp))
              (return-from handle-download-attachment))

            ;; Send metadata first
            (format t "~&[ATTACHMENT-DOWNLOAD] Sending metadata: size=~A~%" size)
            (force-output)
            (let* ((meta (make-instance 'proto-attachment-meta
                                        :hash stored-hash
                                        :filename filename
                                        :mime-type mime-type
                                        :size size))
                   (resp (make-instance 'proto-attachment-download-response
                                        :metadata meta)))
              (setf (slot-value resp 'response-case) :metadata)
              (ag-grpc:stream-send stream resp))

            ;; Send content in chunks
            (format t "~&[ATTACHMENT-DOWNLOAD] Sending content in chunks~%")
            (force-output)
            (let ((content-vec (if (vectorp content)
                                   content
                                   (flexi-streams:string-to-octets content :external-format :utf-8)))
                  (chunk-count 0))
              (loop with len = (length content-vec)
                    for offset from 0 below len by *attachment-chunk-size*
                    for end = (min (+ offset *attachment-chunk-size*) len)
                    for chunk = (subseq content-vec offset end)
                    do (progn
                         (incf chunk-count)
                         (format t "~&[ATTACHMENT-DOWNLOAD] Sending chunk ~A: offset=~A len=~A~%"
                                 chunk-count offset (length chunk))
                         (force-output)
                         (let ((resp (make-instance 'proto-attachment-download-response
                                                    :chunk chunk)))
                           (setf (slot-value resp 'response-case) :chunk)
                           (ag-grpc:stream-send stream resp))))
              (format t "~&[ATTACHMENT-DOWNLOAD] Sent ~A chunks total~%" chunk-count)
              (force-output))

            (llog:info "Attachment sent" :hash hash :size size)
            (format t "~&[ATTACHMENT-DOWNLOAD] Download complete~%")
            (force-output)))

      (error (e)
        (llog:error "Attachment download failed" :hash hash :error (princ-to-string e))
        (let ((resp (make-instance 'proto-attachment-download-response
                                   :error (princ-to-string e))))
          (ag-grpc:stream-send stream resp))))))

(defun register-attachment-service (server)
  "Register the AttachmentService handlers with the gRPC server."
  ;; Upload handler (client streaming)
  (ag-grpc:server-register-handler
   server
   "/cloodoo.AttachmentService/UploadAttachment"
   #'handle-upload-attachment
   :request-type 'proto-attachment-upload-request
   :response-type 'proto-attachment-upload-response
   :client-streaming t
   :server-streaming nil)

  ;; Download handler (server streaming)
  (ag-grpc:server-register-handler
   server
   "/cloodoo.AttachmentService/DownloadAttachment"
   #'handle-download-attachment
   :request-type 'proto-attachment-download-request
   :response-type 'proto-attachment-download-response
   :client-streaming nil
   :server-streaming t))
