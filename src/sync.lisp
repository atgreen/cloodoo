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

;;── Client Registration ───────────────────────────────────────────────────────

(defun register-sync-client (stream device-id)
  "Register a connected sync client for broadcasting."
  (bt:with-lock-held (*clients-lock*)
    (push (cons device-id stream) *connected-clients*)
    (llog:info "Sync client connected" :device-id device-id)))

(defun unregister-sync-client (device-id)
  "Unregister a disconnected sync client."
  (bt:with-lock-held (*clients-lock*)
    (setf *connected-clients*
          (remove-if (lambda (entry) (string= (car entry) device-id))
                     *connected-clients*))
    (llog:info "Sync client disconnected" :device-id device-id)))

(defun broadcast-change (change-msg &optional exclude-device-id)
  "Broadcast a change to all connected clients except the one specified.
   CHANGE-MSG should be a proto-sync-message.
   Snapshots the client list under lock, then sends outside the lock
   to avoid holding the lock during network I/O."
  (let ((clients-snapshot
          (bt:with-lock-held (*clients-lock*)
            (copy-list *connected-clients*))))
    (dolist (entry clients-snapshot)
      (destructuring-bind (device-id . stream) entry
        (when (or (null exclude-device-id)
                  (not (string= device-id exclude-device-id)))
          (handler-case
              (ag-grpc:stream-send stream change-msg)
            (error (e)
              (llog:warn "Failed to send to client" :device-id device-id :error (princ-to-string e)))))))))

;;── SyncStream Handler ────────────────────────────────────────────────────────

(defun handle-sync-stream (ctx stream)
  "Handler for bidirectional TodoSync.SyncStream RPC.
   CTX is the call context, STREAM is for sending/receiving messages."
  (declare (ignore ctx))
  (when *sync-debug* (format t "~&[SYNC-DEBUG] handle-sync-stream entered~%"))
  (let ((client-device-id nil))
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
             (unless (eq (proto-msg-case init-msg) :init)
               (llog:warn "Expected init message" :got (proto-msg-case init-msg))
               (return-from sync-handler))

             (let* ((sync-init (proto-msg-init init-msg))
                    (device-id (proto-init-device-id sync-init))
                    (since (proto-init-since sync-init))
                    (client-time-str (proto-init-client-time sync-init)))
               (setf client-device-id device-id)
               (when *sync-debug*
                 (format t "~&[SYNC-DEBUG] Init: device-id=~A since=~A client-time=~A~%"
                         device-id since client-time-str))

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
               (let* ((rows (db-load-current-rows-since (if (plusp (length since))
                                                            since
                                                            "1970-01-01T00:00:00Z")))
                      (pending-count (length rows)))
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
                   (let* ((todo (hash-table-to-todo row))
                          (valid-from (gethash "valid_from" row))
                          (change-msg (make-sync-upsert-message-with-timestamp
                                       (get-device-id) todo valid-from)))
                     (handler-case
                         (ag-grpc:stream-send stream change-msg)
                       (error (e)
                         (llog:error "Failed to send change" :error (princ-to-string e))
                         (return-from sync-handler)))))
                 (when *sync-debug*
                   (format t "~&[SYNC-DEBUG] All pending changes sent. Entering receive loop.~%")
                   (force-output))

                 ;; Register this client for receiving broadcasts
                 (register-sync-client stream device-id)

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

                     (when (eq (proto-msg-case msg) :change)
                       (let ((change (proto-msg-change msg)))
                         (case (proto-change-case change)
                           (:upsert
                            ;; Client is sending an upsert
                            (let* ((proto-data (proto-change-upsert change))
                                   (todo (proto-to-todo proto-data)))
                              ;; Save to local database
                              (db-save-todo todo)
                              ;; Broadcast to other connected clients
                              (broadcast-change msg device-id)))
                           (:delete-id
                            ;; Client is requesting a delete
                            (let ((todo-id (proto-change-delete-id change)))
                              ;; Delete locally
                              (db-delete-todo todo-id)
                              ;; Broadcast to other clients
                              (broadcast-change msg device-id))))))))))))

      ;; Cleanup on exit
      (when *sync-debug*
        (format t "~&[SYNC-DEBUG] Handler exiting, cleanup. device-id=~A~%" client-device-id)
        (force-output))
      (when client-device-id
        (unregister-sync-client client-device-id)))))

;;── Helper: Convert sync hash table to todo ───────────────────────────────────

(defun hash-table-to-todo (ht)
  "Convert a sync hash table (from db-load-rows-since) back to a todo object."
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
                   :estimated-minutes (let ((m (gethash "estimated_minutes" ht)))
                                        (unless (or (null m) (eq m 'null)) m))
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
        (setf (cdr push-setting) 0)))

    (setf *grpc-server*
          (if use-tls
              (ag-grpc:make-grpc-server port
                                        :host host
                                        :tls t
                                        :tls-certificate (namestring (server-cert-file))
                                        :tls-key (namestring (server-key-file)))
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

    ;; Register change hooks so local TUI changes are broadcast to sync clients
    (setf *todo-change-hook* #'notify-todo-changed)
    (setf *todo-delete-hook* #'notify-todo-deleted)

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

(defun notify-tui-refresh ()
  "Send a sync-refresh message to the TUI program to trigger a redraw."
  (when *sync-program-ref*
    (tui:send *sync-program-ref* (make-instance 'sync-refresh-msg))))

;;── Sync Client Status ────────────────────────────────────────────────────────

(defun sync-client-connected-p ()
  "Check if the sync client is connected."
  (eq *sync-client-status* :connected))

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

              ;; Create gRPC channel (blocks if server is unreachable)
              (setf *sync-client-channel*
                    (if use-tls
                        (ag-grpc:make-channel host port
                                              :tls t
                                              :tls-client-certificate (namestring client-certificate)
                                              :tls-client-key (namestring client-key))
                        (ag-grpc:make-channel host port :tls nil)))

              ;; Create stub and stream
              (let ((stub (make-todo-sync-stub *sync-client-channel*)))
                (setf *sync-client-stream* (todo-sync-sync-stream stub)))

              ;; Send init message
              (let* ((client-time (now-iso))
                     (init-msg (make-sync-init-message (get-device-id) "" client-time)))
                (ag-grpc:stream-send *sync-client-stream* init-msg)
                (llog:info "Sent sync init" :device-id (get-device-id) :client-time client-time))

              ;; Reset backoff on successful connection
              (setf backoff-delay 1)

              ;; Receive loop — blocks until stream ends or error
              (loop while *sync-client-running*
                    do (let ((msg (ag-grpc:stream-read-message *sync-client-stream*)))
                         (if msg
                             (handle-sync-client-message msg)
                             (progn
                               (llog:info "Sync stream ended")
                               (return))))))
          (error (e)
            (let ((error-msg (princ-to-string e)))
              ;; Check if this is a deliberate shutdown, not an actual error
              (if (search "sync client shutting down" error-msg)
                  (progn
                    (llog:info "Sync client shutdown signal received")
                    (return))
                  (progn
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

  ;; Close channel to unblock stream-read-message
  (cleanup-sync-client)

  ;; Interrupt the thread to break out of sleep or make-channel
  (when (and *sync-client-thread* (bt:thread-alive-p *sync-client-thread*))
    (handler-case
        (bt:interrupt-thread *sync-client-thread*
                             (lambda () (error "sync client shutting down")))
      (error (e)
        (llog:warn "Error interrupting sync thread" :error (princ-to-string e)))))

  ;; Wait for the thread to finish
  (when (and *sync-client-thread* (bt:thread-alive-p *sync-client-thread*))
    (handler-case
        (bt:join-thread *sync-client-thread*)
      (error (e)
        (llog:warn "Error joining sync thread" :error (princ-to-string e)))))
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

(defun handle-sync-client-message (msg)
  "Handle a message received from the sync server."
  (case (proto-msg-case msg)
    (:ack
     (let* ((ack (proto-msg-ack msg))
            (server-time (proto-ack-server-time ack))
            (pending (proto-ack-pending-changes ack))
            (error-msg (proto-ack-error ack)))
       (if (and error-msg (plusp (length error-msg)))
           ;; Server rejected connection
           (progn
             (llog:error "Server rejected connection" :error error-msg)
             (update-sync-status :error error-msg)
             (setf *sync-client-running* nil))
           ;; Connection accepted
           (progn
             (llog:info "Sync connected" :server-time server-time :pending pending)
             (update-sync-status :connected)
             (when *sync-model-ref*
               (setf (model-sync-pending-count *sync-model-ref*) pending))))))

    (:change
     (let ((change (proto-msg-change msg)))
       (case (proto-change-case change)
         (:upsert
          (let* ((proto-data (proto-change-upsert change))
                 (todo (proto-to-todo proto-data)))
            (llog:info "Received upsert from server" :id (todo-id todo))
            ;; Suppress notifications to avoid sending the change back
            (let ((*suppress-change-notifications* t))
              (db-save-todo todo))
            ;; Refresh the model's todo list if we have a reference
            (when *sync-model-ref*
              (refresh-model-todos *sync-model-ref*))))
         (:delete-id
          (let ((todo-id (proto-change-delete-id change)))
            (llog:info "Received delete from server" :id todo-id)
            ;; Suppress notifications
            (let ((*suppress-change-notifications* t))
              (db-delete-todo todo-id))
            ;; Refresh the model
            (when *sync-model-ref*
              (refresh-model-todos *sync-model-ref*)))))))

    (otherwise
     (llog:warn "Unknown message from server" :case (proto-msg-case msg)))))

(defun refresh-model-todos (model)
  "Refresh the model's todo list from the database."
  (setf (model-todos model) (load-todos))
  (setf (model-visible-todos-dirty model) t)
  (notify-tui-refresh))

;;── Send Changes to Server ────────────────────────────────────────────────────

(defun sync-client-send-upsert (todo)
  "Send a todo upsert to the sync server."
  (when (and *sync-client-stream* (sync-client-connected-p))
    (let ((msg (make-sync-upsert-message (get-device-id) todo)))
      (handler-case
          (ag-grpc:stream-send *sync-client-stream* msg)
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
