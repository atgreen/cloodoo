;;; proto-helpers.lisp
;;;
;;; Helper functions for working with generated protobuf classes
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cloodoo)

;;── Message Constructors ──────────────────────────────────────────────────────

(defun make-sync-init-message (device-id since client-time &key capabilities)
  "Create a SyncMessage with SyncInit payload.
   CAPABILITIES is an optional list of capability strings (e.g., '(\"lists\"))."
  (let ((init (make-instance 'proto-sync-init
                             :device-id device-id
                             :since since
                             :client-time client-time
                             :capabilities (or capabilities '("lists"))))
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
                                   :estimated-minutes 0
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

(defun proto-msg-settings-change (sync-message)
  "Extract the SettingsChange from a SyncMessage."
  (slot-value sync-message 'settings-change))

(defun proto-msg-reset (sync-message)
  "Extract the SyncReset from a SyncMessage."
  (slot-value sync-message 'reset))

;;── Note: Field accessors (proto-*-device-id, proto-*-timestamp, etc.) are now
;; auto-generated by ag-proto via :ACCESSOR in DEFCLASS. Only custom accessors
;; like proto-msg-case, proto-msg-ack, etc. remain here.

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

;;── ListDefinitionData Conversion ────────────────────────────────────────────

(defun proto-to-list-definition (proto-data)
  "Convert a PROTO-LIST-DEFINITION-DATA to a list-definition instance."
  (make-instance 'list-definition
                 :id (slot-value proto-data 'id)
                 :name (slot-value proto-data 'name)
                 :description (let ((d (slot-value proto-data 'description)))
                                (when (and d (> (length d) 0)) d))
                 :sections (slot-value proto-data 'sections)
                 :created-at (lt:parse-timestring (slot-value proto-data 'created-at))))

(defun list-definition-to-proto (list-def)
  "Convert a list-definition to a PROTO-LIST-DEFINITION-DATA."
  (make-instance 'proto-list-definition-data
                 :id (list-def-id list-def)
                 :name (list-def-name list-def)
                 :description (or (list-def-description list-def) "")
                 :sections (or (list-def-sections list-def) '())
                 :created-at (timestamp-to-string (list-def-created-at list-def))))

;;── ListItemData Conversion ──────────────────────────────────────────────────

(defun proto-to-list-item (proto-data)
  "Convert a PROTO-LIST-ITEM-DATA to a list-item instance."
  (make-instance 'list-item
                 :id (slot-value proto-data 'id)
                 :list-id (slot-value proto-data 'list-id)
                 :title (slot-value proto-data 'title)
                 :section (let ((s (slot-value proto-data 'section)))
                            (when (and s (> (length s) 0)) s))
                 :checked (slot-value proto-data 'checked)
                 :notes (let ((n (slot-value proto-data 'notes)))
                          (when (and n (> (length n) 0)) n))
                 :created-at (lt:parse-timestring (slot-value proto-data 'created-at))))

(defun list-item-to-proto (item)
  "Convert a list-item to a PROTO-LIST-ITEM-DATA."
  (make-instance 'proto-list-item-data
                 :id (list-item-id item)
                 :list-id (list-item-list-id item)
                 :title (list-item-title item)
                 :section (or (list-item-section item) "")
                 :checked (list-item-checked item)
                 :notes (or (list-item-notes item) "")
                 :created-at (timestamp-to-string (list-item-created-at item))))

;;── ListChange Message Constructors ──────────────────────────────────────────

(defun make-sync-list-upsert-message (device-id list-def)
  "Create a SyncMessage with ListChange(upsert_list) payload."
  (let* ((proto-data (list-definition-to-proto list-def))
         (change (make-instance 'proto-list-change
                                :device-id device-id
                                :timestamp (now-iso)))
         (msg (make-instance 'proto-sync-message)))
    (setf (slot-value change 'upsert-list) proto-data)
    (setf (slot-value change 'change-case) :upsert-list)
    (setf (slot-value msg 'list-change) change)
    (setf (slot-value msg 'msg-case) :list-change)
    msg))

(defun make-sync-list-delete-message (device-id list-def-id)
  "Create a SyncMessage with ListChange(delete_list_id) payload."
  (let* ((change (make-instance 'proto-list-change
                                :device-id device-id
                                :timestamp (now-iso)))
         (msg (make-instance 'proto-sync-message)))
    (setf (slot-value change 'delete-list-id) list-def-id)
    (setf (slot-value change 'change-case) :delete-list-id)
    (setf (slot-value msg 'list-change) change)
    (setf (slot-value msg 'msg-case) :list-change)
    msg))

(defun make-sync-list-item-upsert-message (device-id item)
  "Create a SyncMessage with ListChange(upsert_item) payload."
  (let* ((proto-data (list-item-to-proto item))
         (change (make-instance 'proto-list-change
                                :device-id device-id
                                :timestamp (now-iso)))
         (msg (make-instance 'proto-sync-message)))
    (setf (slot-value change 'upsert-item) proto-data)
    (setf (slot-value change 'change-case) :upsert-item)
    (setf (slot-value msg 'list-change) change)
    (setf (slot-value msg 'msg-case) :list-change)
    msg))

(defun make-sync-list-item-delete-message (device-id item-id)
  "Create a SyncMessage with ListChange(delete_item_id) payload."
  (let* ((change (make-instance 'proto-list-change
                                :device-id device-id
                                :timestamp (now-iso)))
         (msg (make-instance 'proto-sync-message)))
    (setf (slot-value change 'delete-item-id) item-id)
    (setf (slot-value change 'change-case) :delete-item-id)
    (setf (slot-value msg 'list-change) change)
    (setf (slot-value msg 'msg-case) :list-change)
    msg))

;;── ListChange Field Accessors ───────────────────────────────────────────────

(defun proto-msg-list-change (sync-message)
  "Extract the ListChange from a SyncMessage."
  (slot-value sync-message 'list-change))
