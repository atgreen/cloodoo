;;; db.lisp - SQLite storage with temporal tables for time-travel
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cloodoo)

;;── Database Path ─────────────────────────────────────────────────────────────

(defun db-file ()
  "Return the path to the SQLite database file."
  (merge-pathnames "cloodoo.db" (data-directory)))

;;── Device ID Management ─────────────────────────────────────────────────────

(defvar *device-id* nil
  "Cached device ID for this machine.")

(defun device-id-file ()
  "Return the path to the device ID file."
  (merge-pathnames "device-id" (data-directory)))

(defun generate-device-id ()
  "Generate a new UUID for this device."
  (format nil "~8,'0X-~4,'0X-~4,'0X-~4,'0X-~12,'0X"
          (random #xFFFFFFFF)
          (random #xFFFF)
          (logior #x4000 (random #x0FFF))  ; Version 4 UUID
          (logior #x8000 (random #x3FFF))  ; Variant 1
          (random #xFFFFFFFFFFFF)))

(defun load-device-id ()
  "Load or create the device ID for this machine."
  (ensure-data-directory)
  (let ((file (device-id-file)))
    (if (probe-file file)
        (string-trim '(#\Space #\Newline #\Tab #\Return)
                     (uiop:read-file-string file))
        ;; Generate and save new ID
        (let ((id (generate-device-id)))
          (with-open-file (stream file
                                  :direction :output
                                  :if-does-not-exist :create)
            (write-string id stream))
          id))))

(defun get-device-id ()
  "Get the device ID, loading it if necessary."
  (or *device-id*
      (setf *device-id* (load-device-id))))

;;── Database Connection ───────────────────────────────────────────────────────

(defvar *db* nil
  "Current database connection.")

(defvar *db-lock* (bt:make-lock "db-lock")
  "Lock for serializing database access within a process.")

(defun open-db ()
  "Open the database connection with proper settings for concurrent access."
  (ensure-data-directory)
  (let ((db (sqlite:connect (namestring (db-file)))))
    ;; Enable WAL mode for better concurrent access
    (sqlite:execute-non-query db "PRAGMA journal_mode=WAL")
    ;; Set busy timeout to 5 seconds
    (sqlite:execute-non-query db "PRAGMA busy_timeout=5000")
    ;; Enable foreign keys
    (sqlite:execute-non-query db "PRAGMA foreign_keys=ON")
    db))

(defun close-db (db)
  "Close the database connection."
  (when db
    (sqlite:disconnect db)))

(defmacro with-db ((db-var) &body body)
  "Execute BODY with a database connection bound to DB-VAR.
   Uses connection pooling within the same process."
  `(bt:with-lock-held (*db-lock*)
     (let ((,db-var (or *db* (open-db))))
       (unwind-protect
            (progn ,@body)
         (unless *db*
           (close-db ,db-var))))))

(defun init-db ()
  "Initialize the database, creating tables if needed."
  (with-db (db)
    ;; Main todos table with temporal columns
    (sqlite:execute-non-query db "
      CREATE TABLE IF NOT EXISTS todos (
        row_id INTEGER PRIMARY KEY AUTOINCREMENT,
        id TEXT NOT NULL,
        title TEXT NOT NULL,
        description TEXT,
        priority TEXT DEFAULT 'medium',
        status TEXT DEFAULT 'pending',
        scheduled_date TEXT,
        due_date TEXT,
        tags TEXT,
        estimated_minutes INTEGER,
        location_info TEXT,
        url TEXT,
        parent_id TEXT,
        created_at TEXT NOT NULL,
        completed_at TEXT,
        valid_from TEXT NOT NULL,
        valid_to TEXT,
        device_id TEXT NOT NULL DEFAULT 'unknown'
      )")

    ;; Migration: add device_id column if it doesn't exist (for existing databases)
    (handler-case
        (sqlite:execute-non-query db "
          ALTER TABLE todos ADD COLUMN device_id TEXT NOT NULL DEFAULT 'unknown'")
      (error () nil))  ; Column already exists

    ;; Migration: add repeat_interval column if it doesn't exist
    (handler-case
        (sqlite:execute-non-query db "
          ALTER TABLE todos ADD COLUMN repeat_interval INTEGER")
      (error () nil))  ; Column already exists

    ;; Migration: add repeat_unit column if it doesn't exist
    (handler-case
        (sqlite:execute-non-query db "
          ALTER TABLE todos ADD COLUMN repeat_unit TEXT")
      (error () nil))  ; Column already exists

    ;; Migration: add enriching_p column if it doesn't exist
    (handler-case
        (sqlite:execute-non-query db "
          ALTER TABLE todos ADD COLUMN enriching_p INTEGER DEFAULT 0")
      (error () nil))  ; Column already exists

    ;; Index for efficient current state queries
    (sqlite:execute-non-query db "
      CREATE INDEX IF NOT EXISTS idx_todos_current
      ON todos(id, valid_to) WHERE valid_to IS NULL")

    ;; Index for time-travel queries
    (sqlite:execute-non-query db "
      CREATE INDEX IF NOT EXISTS idx_todos_temporal
      ON todos(valid_from, valid_to)")

    ;; Index for sync queries (rows since timestamp)
    (sqlite:execute-non-query db "
      CREATE INDEX IF NOT EXISTS idx_todos_sync
      ON todos(valid_from)")

    ;; Tag presets table (simple key-value)
    (sqlite:execute-non-query db "
      CREATE TABLE IF NOT EXISTS tag_presets (
        slot INTEGER PRIMARY KEY,
        tags TEXT
      )")

    ;; Initialize presets if empty
    (let ((count (sqlite:execute-single db "SELECT COUNT(*) FROM tag_presets")))
      (when (zerop count)
        (dotimes (i 10)
          (sqlite:execute-non-query db
            "INSERT INTO tag_presets (slot, tags) VALUES (?, NULL)" i)))))
  t)

;;── Timestamp Helpers ─────────────────────────────────────────────────────────

(defun now-iso ()
  "Return current timestamp as ISO 8601 string."
  (lt:format-rfc3339-timestring nil (lt:now)))

(defun parse-timestamp (str)
  "Parse an ISO 8601 timestamp string, returning NIL if invalid."
  (when (and str (stringp str) (> (length str) 0))
    (handler-case (lt:parse-timestring str)
      (error () nil))))

;;── Row to TODO Conversion ────────────────────────────────────────────────────

(defun row-to-todo (row)
  "Convert a database row to a TODO object.
   ROW is a list: (row_id id title description priority status scheduled_date
                  due_date tags estimated_minutes location_info url parent_id
                  created_at completed_at valid_from valid_to device_id
                  repeat_interval repeat_unit enriching_p)"
  (destructuring-bind (row-id id title description priority status
                       scheduled-date due-date tags estimated-minutes
                       location-info url parent-id created-at completed-at
                       valid-from valid-to device-id
                       &optional repeat-interval repeat-unit enriching-p) row
    (declare (ignore row-id valid-from valid-to))
    (make-instance 'todo
                   :id id
                   :title title
                   :description (unless (eq description :null) description)
                   :priority (intern (string-upcase priority) :keyword)
                   :status (intern (string-upcase status) :keyword)
                   :scheduled-date (parse-timestamp scheduled-date)
                   :due-date (parse-timestamp due-date)
                   :tags (when (and tags (not (eq tags :null)))
                           (coerce (jzon:parse tags) 'list))
                   :estimated-minutes (unless (eq estimated-minutes :null)
                                        estimated-minutes)
                   :location-info (when (and location-info (not (eq location-info :null)))
                                    (let ((ht (jzon:parse location-info)))
                                      (when (hash-table-p ht)
                                        (list :name (gethash "name" ht)
                                              :address (gethash "address" ht)
                                              :phone (gethash "phone" ht)
                                              :map-url (gethash "map_url" ht)
                                              :website (gethash "website" ht)))))
                   :url (unless (eq url :null) url)
                   :parent-id (unless (eq parent-id :null) parent-id)
                   :device-id (unless (eq device-id :null) device-id)
                   :repeat-interval (unless (or (null repeat-interval) (eq repeat-interval :null))
                                      repeat-interval)
                   :repeat-unit (when (and repeat-unit (not (eq repeat-unit :null)) (stringp repeat-unit))
                                  (intern (string-upcase repeat-unit) :keyword))
                   :enriching-p (and enriching-p (not (eq enriching-p :null)) (= enriching-p 1))
                   :created-at (lt:parse-timestring created-at)
                   :completed-at (parse-timestamp completed-at))))

;;── Load TODOs ────────────────────────────────────────────────────────────────

(defun db-load-todos ()
  "Load current (non-superseded) TODOs from the database."
  (with-db (db)
    (let ((rows (sqlite:execute-to-list db "
      SELECT row_id, id, title, description, priority, status,
             scheduled_date, due_date, tags, estimated_minutes,
             location_info, url, parent_id, created_at, completed_at,
             valid_from, valid_to, device_id, repeat_interval, repeat_unit,
             enriching_p
      FROM todos
      WHERE valid_to IS NULL
      ORDER BY created_at DESC")))
      (mapcar #'row-to-todo rows))))

(defun db-load-todos-at (timestamp)
  "Load TODOs as they existed at TIMESTAMP (time-travel query).
   TIMESTAMP should be a local-time timestamp or ISO string."
  (let ((ts (if (stringp timestamp)
                timestamp
                (lt:format-rfc3339-timestring nil timestamp))))
    (with-db (db)
      (let ((rows (sqlite:execute-to-list db "
        SELECT row_id, id, title, description, priority, status,
               scheduled_date, due_date, tags, estimated_minutes,
               location_info, url, parent_id, created_at, completed_at,
               valid_from, valid_to, device_id, repeat_interval, repeat_unit,
               enriching_p
        FROM todos
        WHERE valid_from <= ?
          AND (valid_to IS NULL OR valid_to > ?)
        ORDER BY created_at DESC" ts ts)))
        (mapcar #'row-to-todo rows)))))

;;── Save TODO (Insert or Update) ──────────────────────────────────────────────

(defun todo-to-db-values (todo)
  "Convert a TODO to a list of values for database insertion."
  (list (todo-id todo)
        (todo-title todo)
        (todo-description todo)
        (string-downcase (symbol-name (todo-priority todo)))
        (string-downcase (symbol-name (todo-status todo)))
        (when (todo-scheduled-date todo)
          (lt:format-rfc3339-timestring nil (todo-scheduled-date todo)))
        (when (todo-due-date todo)
          (lt:format-rfc3339-timestring nil (todo-due-date todo)))
        (when (todo-tags todo)
          (jzon:stringify (coerce (todo-tags todo) 'vector)))
        (todo-estimated-minutes todo)
        (when (todo-location-info todo)
          (let ((loc (todo-location-info todo)))
            (jzon:stringify
             (alexandria:plist-hash-table
              (list "name" (getf loc :name)
                    "address" (getf loc :address)
                    "phone" (getf loc :phone)
                    "map_url" (getf loc :map-url)
                    "website" (getf loc :website))
              :test #'equal))))
        (todo-url todo)
        (todo-parent-id todo)
        (lt:format-rfc3339-timestring nil (todo-created-at todo))
        (when (todo-completed-at todo)
          (lt:format-rfc3339-timestring nil (todo-completed-at todo)))
        ;; Use the todo's device-id if set, otherwise use this device's ID
        (or (todo-device-id todo) (get-device-id))
        ;; Repeat fields
        (todo-repeat-interval todo)
        (when (todo-repeat-unit todo)
          (string-downcase (symbol-name (todo-repeat-unit todo))))
        ;; Enrichment flag
        (if (todo-enriching-p todo) 1 0)))

(defun db-save-todo (todo)
  "Save a TODO to the database using append-only semantics.
   If the TODO already exists (by ID), the old version is marked as superseded."
  (with-db (db)
    (let ((now (now-iso))
          (values (todo-to-db-values todo))
          (committed nil))
      ;; Start a transaction for atomicity
      (sqlite:execute-non-query db "BEGIN IMMEDIATE")
      (unwind-protect
           (progn
             ;; Mark any existing current version as superseded
             (sqlite:execute-non-query db "
               UPDATE todos SET valid_to = ?
               WHERE id = ? AND valid_to IS NULL"
               now (todo-id todo))
             ;; Insert the new version
             (sqlite:execute-non-query db "
               INSERT INTO todos (id, title, description, priority, status,
                                  scheduled_date, due_date, tags, estimated_minutes,
                                  location_info, url, parent_id, created_at,
                                  completed_at, valid_from, valid_to, device_id,
                                  repeat_interval, repeat_unit, enriching_p)
               VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULL, ?, ?, ?, ?)"
               (first values) (second values) (third values) (fourth values)
               (fifth values) (sixth values) (seventh values) (eighth values)
               (ninth values) (tenth values) (nth 10 values) (nth 11 values)
               (nth 12 values) (nth 13 values) now (nth 14 values)
               (nth 15 values) (nth 16 values) (nth 17 values))
             (sqlite:execute-non-query db "COMMIT")
             (setf committed t))
        (unless committed
          (ignore-errors (sqlite:execute-non-query db "ROLLBACK")))))))

(defun db-save-todos (todos)
  "Save all TODOs to the database.
   Only updates todos that are in the incoming list, preserving externally added ones.
   Deduplicates by ID, keeping the first occurrence (most recent since lists are built with push)."
  (with-db (db)
    ;; Deduplicate todos by ID - keep first occurrence of each ID
    (let* ((seen-ids (make-hash-table :test #'equal))
           (unique-todos (loop for todo in todos
                               for id = (todo-id todo)
                               unless (gethash id seen-ids)
                                 collect todo
                                 and do (setf (gethash id seen-ids) t)))
           (now (now-iso))
           (committed nil)
           (incoming-ids (mapcar #'todo-id unique-todos)))
      (sqlite:execute-non-query db "BEGIN IMMEDIATE")
      (unwind-protect
           (progn
             ;; Only invalidate todos that are in our incoming list
             ;; This preserves externally added todos
             (dolist (id incoming-ids)
               (sqlite:execute-non-query db "
                 UPDATE todos SET valid_to = ?
                 WHERE id = ? AND valid_to IS NULL" now id))
             ;; Insert all TODOs as new versions
             (dolist (todo unique-todos)
               (let ((values (todo-to-db-values todo)))
                 (sqlite:execute-non-query db "
                   INSERT INTO todos (id, title, description, priority, status,
                                      scheduled_date, due_date, tags, estimated_minutes,
                                      location_info, url, parent_id, created_at,
                                      completed_at, valid_from, valid_to, device_id,
                                      repeat_interval, repeat_unit, enriching_p)
                   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULL, ?, ?, ?, ?)"
                   (first values) (second values) (third values) (fourth values)
                   (fifth values) (sixth values) (seventh values) (eighth values)
                   (ninth values) (tenth values) (nth 10 values) (nth 11 values)
                   (nth 12 values) (nth 13 values) now (nth 14 values)
                   (nth 15 values) (nth 16 values) (nth 17 values))))
             (sqlite:execute-non-query db "COMMIT")
             (setf committed t))
        (unless committed
          (ignore-errors (sqlite:execute-non-query db "ROLLBACK")))))))

(defun db-delete-todo (todo-id)
  "Delete a TODO by marking all its versions as superseded.
   The data remains for time-travel queries."
  (with-db (db)
    (let ((now (now-iso)))
      (sqlite:execute-non-query db "
        UPDATE todos SET valid_to = ?
        WHERE id = ? AND valid_to IS NULL"
        now todo-id))))

;;── Tag Presets ───────────────────────────────────────────────────────────────

(defun db-load-presets ()
  "Load tag presets from the database."
  (with-db (db)
    (let ((presets (make-array 10 :initial-element nil)))
      (dolist (row (sqlite:execute-to-list db "SELECT slot, tags FROM tag_presets"))
        (destructuring-bind (slot tags) row
          (when (and tags (not (eq tags :null)) (< slot 10))
            (let ((parsed (jzon:parse tags)))
              ;; Only store if parsed result is an actual sequence (not null symbol)
              (when (and parsed (not (eq parsed 'null)) (typep parsed 'sequence))
                (setf (aref presets slot) (coerce parsed 'list)))))))
      presets)))

(defun db-save-presets (presets)
  "Save tag presets to the database."
  (with-db (db)
    (dotimes (i 10)
      (let ((tags (aref presets i)))
        (sqlite:execute-non-query db "
          UPDATE tag_presets SET tags = ? WHERE slot = ?"
          (when (and tags (listp tags) (not (eq tags 'null)) (not (eq tags :null)))
            (jzon:stringify (coerce tags 'vector)))
          i)))))

;;── Migration ─────────────────────────────────────────────────────────────────

(defun migrate-json-to-db ()
  "Migrate existing todos.json data to the SQLite database."
  (let ((json-file (todos-file)))
    (when (probe-file json-file)
      (format t "Migrating data from ~A to SQLite...~%" json-file)
      ;; Load from JSON directly (since load-todos will be redefined later)
      (let* ((data (with-open-file (stream json-file :direction :input)
                     (jzon:parse stream)))
             (todos-array (gethash "todos" data))
             (presets-data (gethash "tag_presets" data))
             (todos (when todos-array
                      (map 'list #'hash-table-to-todo todos-array)))
             (presets (if (and presets-data (typep presets-data 'sequence))
                          (let ((p (make-array 10 :initial-element nil)))
                            (loop for i from 0 below (min 10 (length presets-data))
                                  for tags = (aref presets-data i)
                                  do (when (and tags (not (eq tags 'null)) (typep tags 'sequence))
                                       (setf (aref p i) (coerce tags 'list))))
                            p)
                          (make-array 10 :initial-element nil))))
        (init-db)
        ;; Save all TODOs
        (with-db (db)
          (let ((now (now-iso))
                (committed nil))
            (sqlite:execute-non-query db "BEGIN IMMEDIATE")
            (unwind-protect
                 (progn
                   (dolist (todo todos)
                     (let ((values (todo-to-db-values todo)))
                       (sqlite:execute-non-query db "
                         INSERT INTO todos (id, title, description, priority, status,
                                            scheduled_date, due_date, tags, estimated_minutes,
                                            location_info, url, parent_id, created_at,
                                            completed_at, valid_from, valid_to, device_id,
                                            repeat_interval, repeat_unit, enriching_p)
                         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULL, ?, ?, ?, ?)"
                         (first values) (second values) (third values) (fourth values)
                         (fifth values) (sixth values) (seventh values) (eighth values)
                         (ninth values) (tenth values) (nth 10 values) (nth 11 values)
                         (nth 12 values) (nth 13 values) now (nth 14 values)
                         (nth 15 values) (nth 16 values) (nth 17 values))))
                   (sqlite:execute-non-query db "COMMIT")
                   (setf committed t))
              (unless committed
                (ignore-errors (sqlite:execute-non-query db "ROLLBACK"))))))
        ;; Save presets
        (db-save-presets presets)
        ;; Rename old JSON file
        (let ((backup-file (merge-pathnames "todos.json.bak" (data-directory))))
          (rename-file json-file backup-file)
          (format t "Migration complete. Old file backed up to ~A~%" backup-file))
        t))))

;;── Storage Backend Selection ─────────────────────────────────────────────────

(defvar *use-sqlite* t
  "When T, use SQLite storage. When NIL, use JSON storage.")

(defun ensure-db-initialized ()
  "Ensure the database is initialized, migrating from JSON if needed."
  (when *use-sqlite*
    (let ((db-exists (probe-file (db-file)))
          (json-exists (probe-file (todos-file))))
      (cond
        ;; Database exists - just ensure tables exist
        (db-exists
         (init-db))
        ;; JSON exists but no database - migrate
        ((and json-exists (not db-exists))
         (migrate-json-to-db))
        ;; Neither exists - create fresh database
        (t
         (init-db))))))

;;── Redefine Storage Functions to Use SQLite ──────────────────────────────────

;; Redefine to use SQLite (these override the JSON versions from storage.lisp)
(defun load-todos ()
  "Load current TODOs from SQLite database."
  (ensure-db-initialized)
  (db-load-todos))

(defun save-todos (todos &optional presets)
  "Save TODOs to SQLite database."
  (ensure-db-initialized)
  (db-save-todos todos)
  (when presets
    (db-save-presets presets)))

;;── Change Notification Hooks ─────────────────────────────────────────────────

(defvar *todo-change-hook* nil
  "Function to call when a todo is saved. Called with (todo).
   Used by sync server to broadcast changes to connected clients.")

(defvar *todo-delete-hook* nil
  "Function to call when a todo is deleted. Called with (todo-id).
   Used by sync server to broadcast deletions to connected clients.")

(defvar *suppress-change-notifications* nil
  "When T, don't call change hooks. Used when processing incoming sync changes.")

(defun save-todo (todo)
  "Save a single TODO (more efficient for single updates)."
  (ensure-db-initialized)
  (db-save-todo todo)
  ;; Notify sync clients unless suppressed
  (when (and *todo-change-hook* (not *suppress-change-notifications*))
    (handler-case
        (funcall *todo-change-hook* todo)
      (error (e)
        (llog:warn "Change notification failed: ~A" e)))))

(defun load-presets ()
  "Load tag presets from SQLite database."
  (ensure-db-initialized)
  (db-load-presets))

(defun save-presets (presets)
  "Save tag presets to SQLite database."
  (ensure-db-initialized)
  (db-save-presets presets))

;; Time-travel API
(defun load-todos-at (timestamp)
  "Load TODOs as they existed at TIMESTAMP.
   TIMESTAMP can be a local-time timestamp or ISO 8601 string."
  (ensure-db-initialized)
  (db-load-todos-at timestamp))

;;── Sync API ─────────────────────────────────────────────────────────────────

(defun db-null-to-json-null (value)
  "Convert a database value to JSON-safe value. :null and nil become 'null."
  (if (or (eq value :null) (null value))
      'null
      value))

(defun row-to-sync-hash-table (row)
  "Convert a database row to a hash table for JSON sync response.
   Includes all fields including row_id, valid_from, valid_to for sync purposes."
  (destructuring-bind (row-id id title description priority status
                       scheduled-date due-date tags estimated-minutes
                       location-info url parent-id created-at completed-at
                       valid-from valid-to device-id
                       &optional repeat-interval repeat-unit enriching-p) row
    (let ((ht (make-hash-table :test #'equal)))
      (setf (gethash "row_id" ht) row-id)
      (setf (gethash "id" ht) id)
      (setf (gethash "title" ht) title)
      (setf (gethash "description" ht) (db-null-to-json-null description))
      (setf (gethash "priority" ht) priority)
      (setf (gethash "status" ht) status)
      (setf (gethash "scheduled_date" ht) (db-null-to-json-null scheduled-date))
      (setf (gethash "due_date" ht) (db-null-to-json-null due-date))
      (setf (gethash "tags" ht) (db-null-to-json-null tags))
      (setf (gethash "estimated_minutes" ht) (db-null-to-json-null estimated-minutes))
      (setf (gethash "location_info" ht) (db-null-to-json-null location-info))
      (setf (gethash "url" ht) (db-null-to-json-null url))
      (setf (gethash "parent_id" ht) (db-null-to-json-null parent-id))
      (setf (gethash "created_at" ht) created-at)
      (setf (gethash "completed_at" ht) (db-null-to-json-null completed-at))
      (setf (gethash "valid_from" ht) valid-from)
      (setf (gethash "valid_to" ht) (db-null-to-json-null valid-to))
      (setf (gethash "device_id" ht) device-id)
      (setf (gethash "repeat_interval" ht) (db-null-to-json-null repeat-interval))
      (setf (gethash "repeat_unit" ht) (db-null-to-json-null repeat-unit))
      (setf (gethash "enriching_p" ht) (db-null-to-json-null enriching-p))
      ht)))

(defun db-load-rows-since (timestamp)
  "Load all rows (including superseded) where valid_from > timestamp.
   Returns raw hash tables suitable for JSON serialization."
  (ensure-db-initialized)
  (let ((ts (if (stringp timestamp)
                timestamp
                (lt:format-rfc3339-timestring nil timestamp))))
    (with-db (db)
      (let ((rows (sqlite:execute-to-list db "
        SELECT row_id, id, title, description, priority, status,
               scheduled_date, due_date, tags, estimated_minutes,
               location_info, url, parent_id, created_at, completed_at,
               valid_from, valid_to, device_id, repeat_interval, repeat_unit,
               enriching_p
        FROM todos
        WHERE valid_from > ?
        ORDER BY valid_from ASC" ts)))
        (mapcar #'row-to-sync-hash-table rows)))))

(defun db-load-current-rows-since (timestamp)
  "Load only CURRENT (non-superseded) rows where valid_from > timestamp.
   This is the correct function for sync - it sends the latest state of each todo
   without replaying historical versions that could overwrite newer data.
   Returns raw hash tables suitable for JSON serialization."
  (ensure-db-initialized)
  (let ((ts (if (stringp timestamp)
                timestamp
                (lt:format-rfc3339-timestring nil timestamp))))
    (with-db (db)
      (let ((rows (sqlite:execute-to-list db "
        SELECT row_id, id, title, description, priority, status,
               scheduled_date, due_date, tags, estimated_minutes,
               location_info, url, parent_id, created_at, completed_at,
               valid_from, valid_to, device_id, repeat_interval, repeat_unit,
               enriching_p
        FROM todos
        WHERE valid_from > ? AND valid_to IS NULL
        ORDER BY valid_from ASC" ts)))
        (mapcar #'row-to-sync-hash-table rows)))))

(defun db-merge-rows (rows source-device-id)
  "Merge rows from another device into the local database.
   ROWS is a list of hash tables with all row data.
   Returns (accepted rejected) counts.
   Conflict resolution: Last-writer-wins based on valid_from timestamp."
  (ensure-db-initialized)
  (let ((accepted 0)
        (rejected 0))
    (with-db (db)
      (let ((committed nil))
        (sqlite:execute-non-query db "BEGIN IMMEDIATE")
        (unwind-protect
             (progn
               (dolist (row rows)
                 (let* ((id (gethash "id" row))
                        (valid-from (gethash "valid_from" row))
                        (valid-to (gethash "valid_to" row))
                        (device-id (or (gethash "device_id" row) source-device-id))
                        ;; Check if we already have this exact row (by valid_from and device_id)
                        (exists (sqlite:execute-single db "
                          SELECT 1 FROM todos
                          WHERE id = ? AND valid_from = ? AND device_id = ?"
                          id valid-from device-id)))
                   (if exists
                       (incf rejected)
                       (progn
                         ;; Insert the row with its original timestamps
                         (sqlite:execute-non-query db "
                           INSERT INTO todos (id, title, description, priority, status,
                                              scheduled_date, due_date, tags, estimated_minutes,
                                              location_info, url, parent_id, created_at,
                                              completed_at, valid_from, valid_to, device_id,
                                              repeat_interval, repeat_unit, enriching_p)
                           VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                           id
                           (gethash "title" row)
                           (gethash "description" row)
                           (gethash "priority" row)
                           (gethash "status" row)
                           (gethash "scheduled_date" row)
                           (gethash "due_date" row)
                           (gethash "tags" row)
                           (gethash "estimated_minutes" row)
                           (gethash "location_info" row)
                           (gethash "url" row)
                           (gethash "parent_id" row)
                           (gethash "created_at" row)
                           (gethash "completed_at" row)
                           valid-from
                           valid-to
                           device-id
                           (gethash "repeat_interval" row)
                           (gethash "repeat_unit" row)
                           (gethash "enriching_p" row))
                         (incf accepted)))))
               (sqlite:execute-non-query db "COMMIT")
               (setf committed t))
          (unless committed
            (ignore-errors (sqlite:execute-non-query db "ROLLBACK"))))))
    (values accepted rejected)))
