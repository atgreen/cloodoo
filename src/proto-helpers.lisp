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
    (setf (slot-value msg 'init) init)
    (setf (slot-value msg 'msg-case) :init)
    msg))

(defun make-sync-ack-message (server-time pending-count &optional error)
  "Create a SyncMessage with SyncAck payload."
  (let ((ack (make-instance 'proto-sync-ack
                            :server-time server-time
                            :pending-changes pending-count
                            :error (or error "")))
        (msg (make-instance 'proto-sync-message)))
    (setf (slot-value msg 'ack) ack)
    (setf (slot-value msg 'msg-case) :ack)
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
    (setf (slot-value change 'upsert) todo-data)
    (setf (slot-value change 'change-case) :upsert)
    (setf (slot-value msg 'change) change)
    (setf (slot-value msg 'msg-case) :change)
    msg))

(defun make-sync-delete-message (device-id todo-id)
  "Create a SyncMessage with TodoChange(delete) payload."
  (let* ((change (make-instance 'proto-todo-change
                                :device-id device-id
                                :timestamp (now-iso)))
         (msg (make-instance 'proto-sync-message)))
    (setf (slot-value change 'delete-id) todo-id)
    (setf (slot-value change 'change-case) :delete-id)
    (setf (slot-value msg 'change) change)
    (setf (slot-value msg 'msg-case) :change)
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
    (setf (slot-value msg 'settings-change) settings-change)
    (setf (slot-value msg 'msg-case) :settings-change)
    msg))

(defun make-sync-reset-message (device-id &optional reset-to)
  "Create a SyncMessage with SyncReset payload.
   RESET-TO is an optional ISO 8601 timestamp; if nil, performs a full reset."
  (let ((reset (make-instance 'proto-sync-reset
                              :device-id device-id
                              :timestamp (now-iso)
                              :reset-to (or reset-to "")))
        (msg (make-instance 'proto-sync-message)))
    (setf (slot-value msg 'reset) reset)
    (setf (slot-value msg 'msg-case) :reset)
    msg))

;;── Message Type Detection ────────────────────────────────────────────────────

(defun proto-msg-case (sync-message)
  "Return which variant is set in a SyncMessage (:init, :ack, :change, or :settings-change)."
  (slot-value sync-message 'msg-case))

;;── Message Field Accessors ───────────────────────────────────────────────────

(defun proto-msg-ack (sync-message)
  "Extract the SyncAck from a SyncMessage."
  (slot-value sync-message 'ack))

(defun proto-msg-init (sync-message)
  "Extract the SyncInit from a SyncMessage."
  (slot-value sync-message 'init))

(defun proto-msg-change (sync-message)
  "Extract the TodoChange from a SyncMessage."
  (slot-value sync-message 'change))

;;── SyncAck Accessors ──────────────────────────────────────────────────────────

(defun proto-ack-server-time (sync-ack)
  "Get server-time from SyncAck."
  (slot-value sync-ack 'server-time))

(defun proto-ack-pending-changes (sync-ack)
  "Get pending-changes from SyncAck."
  (slot-value sync-ack 'pending-changes))

(defun proto-ack-error (sync-ack)
  "Get error from SyncAck."
  (slot-value sync-ack 'proto-error))

;;── TodoChange Accessors ───────────────────────────────────────────────────────

(defun proto-change-case (todo-change)
  "Return which variant is set in a TodoChange (:upsert or :delete-id)."
  (slot-value todo-change 'change-case))

(defun proto-change-device-id (todo-change)
  "Get device-id from TodoChange."
  (slot-value todo-change 'device-id))

(defun proto-change-timestamp (todo-change)
  "Get timestamp from TodoChange."
  (slot-value todo-change 'timestamp))

(defun proto-change-upsert (todo-change)
  "Get upsert (TodoData) from TodoChange."
  (slot-value todo-change 'upsert))

(defun proto-change-delete-id (todo-change)
  "Get delete-id from TodoChange."
  (slot-value todo-change 'delete-id))

;;── SyncInit Accessors ─────────────────────────────────────────────────────────

(defun proto-init-device-id (sync-init)
  "Get device-id from SyncInit."
  (slot-value sync-init 'device-id))

(defun proto-init-since (sync-init)
  "Get since from SyncInit."
  (slot-value sync-init 'since))

(defun proto-init-client-time (sync-init)
  "Get client-time from SyncInit."
  (slot-value sync-init 'client-time))

;;── SettingsChange Accessors ──────────────────────────────────────────────────

(defun proto-msg-settings-change (sync-message)
  "Extract the SettingsChange from a SyncMessage."
  (slot-value sync-message 'settings-change))

(defun proto-settings-change-device-id (settings-change)
  "Get device-id from SettingsChange."
  (slot-value settings-change 'device-id))

(defun proto-settings-change-timestamp (settings-change)
  "Get timestamp from SettingsChange."
  (slot-value settings-change 'timestamp))

(defun proto-settings-change-settings (settings-change)
  "Get settings list from SettingsChange."
  (slot-value settings-change 'settings))

;;── SyncReset Accessors ────────────────────────────────────────────────────────

(defun proto-msg-reset (sync-message)
  "Extract the SyncReset from a SyncMessage."
  (slot-value sync-message 'reset))

(defun proto-reset-device-id (sync-reset)
  "Get device-id from SyncReset."
  (slot-value sync-reset 'device-id))

(defun proto-reset-timestamp (sync-reset)
  "Get timestamp from SyncReset."
  (slot-value sync-reset 'timestamp))

(defun proto-reset-reset-to (sync-reset)
  "Get reset-to timestamp from SyncReset."
  (slot-value sync-reset 'reset-to))

;;── TodoData Conversion ────────────────────────────────────────────────────────

(defun parse-timestamp (str)
  "Parse an ISO 8601 timestamp string, returning NIL if invalid."
  (when (and str (stringp str) (> (length str) 0))
    (handler-case (lt:parse-timestring str)
      (error () nil))))

(defun proto-to-todo (proto-data)
  "Convert a PROTO-TODO-DATA to a todo class instance."
  (make-instance 'todo
                 :id (slot-value proto-data 'id)
                 :title (slot-value proto-data 'title)
                 :description (let ((desc (slot-value proto-data 'description)))
                                (when (and desc (> (length desc) 0)) desc))
                 :priority (let ((p (slot-value proto-data 'priority)))
                             (if (and p (> (length p) 0))
                                 (intern (string-upcase p) :keyword)
                                 :medium))
                 :status (let ((s (slot-value proto-data 'status)))
                           (if (and s (> (length s) 0))
                               (intern (string-upcase s) :keyword)
                               :pending))
                 :scheduled-date (parse-timestamp (slot-value proto-data 'scheduled-date))
                 :due-date (parse-timestamp (slot-value proto-data 'due-date))
                 :tags (slot-value proto-data 'tags)
                 :estimated-minutes (let ((em (slot-value proto-data 'estimated-minutes)))
                                      (when (> em 0) em))
                 :location-info (let ((loc (slot-value proto-data 'location-info)))
                                  (when loc
                                    (list :name (slot-value loc 'name)
                                          :address (slot-value loc 'address)
                                          :phone (slot-value loc 'phone)
                                          :map-url (slot-value loc 'map-url)
                                          :website (slot-value loc 'website))))
                 :url (let ((u (slot-value proto-data 'url)))
                        (when (and u (> (length u) 0)) u))
                 :repeat-interval (let ((ri (slot-value proto-data 'repeat-interval)))
                                    (when (> ri 0) ri))
                 :repeat-unit (let ((ru (slot-value proto-data 'repeat-unit)))
                                (when (and ru (> (length ru) 0))
                                  (intern (string-upcase ru) :keyword)))
                 :attachment-hashes (slot-value proto-data 'attachment-hashes)
                 :created-at (lt:parse-timestring (slot-value proto-data 'created-at))
                 :completed-at (parse-timestamp (slot-value proto-data 'completed-at))
                 :enriching-p (slot-value proto-data 'enriching-p)))
