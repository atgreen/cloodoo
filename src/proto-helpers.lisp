;;; proto-helpers.lisp
;;;
;;; Helper functions for working with generated protobuf classes
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cloodoo)

;;── Message Constructors ──────────────────────────────────────────────────────

(defun make-sync-init-message (device-id since client-time)
  "Create a SyncMessage with SyncInit payload."
  (let ((init (make-instance 'proto-sync-init
                             :device-id device-id
                             :since since
                             :client-time client-time))
        (msg (make-instance 'proto-sync-message)))
    (setf (proto-sync-message-init msg) init)
    (setf (msg-case msg) :init)
    msg))

(defun make-sync-ack-message (server-time pending-count &optional error)
  "Create a SyncMessage with SyncAck payload."
  (let ((ack (make-instance 'proto-sync-ack
                            :server-time server-time
                            :pending-changes pending-count
                            :error (or error "")))
        (msg (make-instance 'proto-sync-message)))
    (setf (proto-sync-message-ack msg) ack)
    (setf (msg-case msg) :ack)
    msg))

(defun make-sync-upsert-message (device-id todo)
  "Create a SyncMessage with TodoChange(upsert) payload."
  (make-sync-upsert-message-with-timestamp device-id todo (now-iso)))

(defun timestamp-to-string (ts)
  "Convert a local-time timestamp to RFC3339 string, or return empty string if nil."
  (if ts
      (lt:format-rfc3339-timestring nil ts)
      ""))

(defun make-sync-upsert-message-with-timestamp (device-id todo timestamp)
  "Create a SyncMessage with TodoChange(upsert) payload and explicit timestamp."
  (let* ((todo-data (make-instance 'proto-todo-data
                                   :id (todo-id todo)
                                   :title (todo-title todo)
                                   :description (or (todo-description todo) "")
                                   :priority (string-downcase (symbol-name (todo-priority todo)))
                                   :status (string-downcase (symbol-name (todo-status todo)))
                                   :scheduled-date (timestamp-to-string (todo-scheduled-date todo))
                                   :due-date (timestamp-to-string (todo-due-date todo))
                                   :tags (or (todo-tags todo) '())
                                   :estimated-minutes (or (todo-estimated-minutes todo) 0)
                                   :url (or (todo-url todo) "")
                                   :created-at (timestamp-to-string (todo-created-at todo))
                                   :completed-at (timestamp-to-string (todo-completed-at todo))
                                   :parent-id ""
                                   :repeat-interval (or (todo-repeat-interval todo) 0)
                                   :repeat-unit (let ((ru (todo-repeat-unit todo)))
                                                     (if ru (string-downcase (symbol-name ru)) ""))
                                   :attachment-hashes (or (todo-attachment-hashes todo) '())
                                   :enriching-p (todo-enriching-p todo)))
         (change (make-instance 'proto-todo-change
                                :device-id device-id
                                :timestamp timestamp))
         (msg (make-instance 'proto-sync-message)))
    (setf (proto-todo-change-upsert change) todo-data)
    (setf (change-case change) :upsert)
    (setf (proto-sync-message-change msg) change)
    (setf (msg-case msg) :change)
    msg))

(defun make-sync-delete-message (device-id todo-id)
  "Create a SyncMessage with TodoChange(delete) payload."
  (let* ((change (make-instance 'proto-todo-change
                                :device-id device-id
                                :timestamp (now-iso)))
         (msg (make-instance 'proto-sync-message)))
    (setf (proto-todo-change-delete-id change) todo-id)
    (setf (change-case change) :delete-id)
    (setf (proto-sync-message-change msg) change)
    (setf (msg-case msg) :change)
    msg))

(defun make-sync-settings-message (device-id settings-hash)
  "Create a SyncMessage with SettingsChange payload.
   SETTINGS-HASH is a hash table with keys mapped to (:value val :updated-at timestamp)."
  (let* ((settings-data-list
           (loop for key being the hash-keys of settings-hash
                   using (hash-value data)
                 collect (make-instance 'proto-settings-data
                                       :key key
                                       :value (getf data :value)
                                       :updated-at (getf data :updated-at))))
         (settings-change (make-instance 'proto-settings-change
                                        :device-id device-id
                                        :timestamp (now-iso)
                                        :settings settings-data-list))
         (msg (make-instance 'proto-sync-message)))
    (setf (proto-sync-message-settings-change msg) settings-change)
    (setf (msg-case msg) :settings-change)
    msg))

;;── Message Type Detection ────────────────────────────────────────────────────

(defun proto-msg-case (sync-message)
  "Return which variant is set in a SyncMessage (:init, :ack, :change, or :settings-change)."
  (msg-case sync-message))

;;── Message Field Accessors ───────────────────────────────────────────────────

(defun proto-msg-ack (sync-message)
  "Extract the SyncAck from a SyncMessage."
  (proto-sync-message-ack sync-message))

(defun proto-msg-init (sync-message)
  "Extract the SyncInit from a SyncMessage."
  (proto-sync-message-init sync-message))

(defun proto-msg-change (sync-message)
  "Extract the TodoChange from a SyncMessage."
  (proto-sync-message-change sync-message))

;;── SyncAck Accessors ──────────────────────────────────────────────────────────

(defun proto-ack-server-time (sync-ack)
  "Get server-time from SyncAck."
  (proto-sync-ack-server-time sync-ack))

(defun proto-ack-pending-changes (sync-ack)
  "Get pending-changes from SyncAck."
  (proto-sync-ack-pending-changes sync-ack))

(defun proto-ack-error (sync-ack)
  "Get error from SyncAck."
  (proto-sync-ack-error sync-ack))

;;── TodoChange Accessors ───────────────────────────────────────────────────────

(defun proto-change-case (todo-change)
  "Return which variant is set in a TodoChange (:upsert or :delete-id)."
  (change-case todo-change))

(defun proto-change-device-id (todo-change)
  "Get device-id from TodoChange."
  (proto-todo-change-device-id todo-change))

(defun proto-change-timestamp (todo-change)
  "Get timestamp from TodoChange."
  (proto-todo-change-timestamp todo-change))

(defun proto-change-upsert (todo-change)
  "Get upsert (TodoData) from TodoChange."
  (proto-todo-change-upsert todo-change))

(defun proto-change-delete-id (todo-change)
  "Get delete-id from TodoChange."
  (proto-todo-change-delete-id todo-change))

;;── SyncInit Accessors ─────────────────────────────────────────────────────────

(defun proto-init-device-id (sync-init)
  "Get device-id from SyncInit."
  (proto-sync-init-device-id sync-init))

(defun proto-init-since (sync-init)
  "Get since from SyncInit."
  (proto-sync-init-since sync-init))

(defun proto-init-client-time (sync-init)
  "Get client-time from SyncInit."
  (proto-sync-init-client-time sync-init))

;;── SettingsChange Accessors ──────────────────────────────────────────────────

(defun proto-msg-settings-change (sync-message)
  "Extract the SettingsChange from a SyncMessage."
  (proto-sync-message-settings-change sync-message))

(defun proto-settings-change-device-id (settings-change)
  "Get device-id from SettingsChange."
  (proto-settings-change-device-id settings-change))

(defun proto-settings-change-timestamp (settings-change)
  "Get timestamp from SettingsChange."
  (proto-settings-change-timestamp settings-change))

(defun proto-settings-change-settings (settings-change)
  "Get settings list from SettingsChange."
  (proto-settings-change-settings settings-change))

;;── TodoData Conversion ────────────────────────────────────────────────────────

(defun parse-timestamp (str)
  "Parse an ISO 8601 timestamp string, returning NIL if invalid."
  (when (and str (stringp str) (> (length str) 0))
    (handler-case (lt:parse-timestring str)
      (error () nil))))

(defun proto-to-todo (proto-data)
  "Convert a PROTO-TODO-DATA to a todo class instance."
  (make-instance 'todo
                 :id (proto-id proto-data)
                 :title (proto-title proto-data)
                 :description (let ((desc (proto-description proto-data)))
                                (when (and desc (> (length desc) 0)) desc))
                 :priority (let ((p (proto-priority proto-data)))
                             (if (and p (> (length p) 0))
                                 (intern (string-upcase p) :keyword)
                                 :medium))
                 :status (let ((s (proto-status proto-data)))
                           (if (and s (> (length s) 0))
                               (intern (string-upcase s) :keyword)
                               :pending))
                 :scheduled-date (parse-timestamp (proto-scheduled-date proto-data))
                 :due-date (parse-timestamp (proto-due-date proto-data))
                 :tags (proto-tags proto-data)
                 :estimated-minutes (let ((em (proto-estimated-minutes proto-data)))
                                      (when (> em 0) em))
                 :location-info (let ((loc (proto-location-info proto-data)))
                                  (when loc
                                    (list :name (proto-name loc)
                                          :address (proto-address loc)
                                          :phone (proto-phone loc)
                                          :map-url (proto-map-url loc)
                                          :website (proto-website loc))))
                 :url (let ((u (proto-url proto-data)))
                        (when (and u (> (length u) 0)) u))
                 :repeat-interval (let ((ri (proto-repeat-interval proto-data)))
                                    (when (> ri 0) ri))
                 :repeat-unit (let ((ru (proto-repeat-unit proto-data)))
                                (when (and ru (> (length ru) 0))
                                  (intern (string-upcase ru) :keyword)))
                 :attachment-hashes (proto-attachment-hashes proto-data)
                 :created-at (lt:parse-timestring (proto-created-at proto-data))
                 :completed-at (parse-timestamp (proto-completed-at proto-data))
                 :enriching-p (proto-enriching-p proto-data)))
