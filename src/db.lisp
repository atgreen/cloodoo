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

;;── Cryptographically Secure Random ──────────────────────────────────────────

(defun secure-random-bytes (n)
  "Return N cryptographically random bytes using ironclad's portable CSPRNG."
  (ironclad:random-data n))

(defun secure-random (limit)
  "Return a cryptographically random non-negative integer below LIMIT.
   Uses /dev/urandom. Applies rejection sampling to avoid modulo bias."
  (let* ((byte-count (max 1 (ceiling (integer-length limit) 8)))
         (mask (1- (ash 1 (* byte-count 8)))))
    (loop
      (let* ((bytes (secure-random-bytes byte-count))
             (value (loop for byte across bytes
                          for shift from 0 by 8
                          sum (ash byte shift))))
        (let ((candidate (logand value mask)))
          (when (< candidate limit)
            (return candidate)))))))

;;── Device ID Management ─────────────────────────────────────────────────────

(defvar *device-id* nil
  "Cached device ID for this machine.")

(defun device-id-file ()
  "Return the path to the device ID file."
  (merge-pathnames "device-id" (data-directory)))

(defun generate-device-id ()
  "Generate a new UUID v4 for this device using CSPRNG."
  (format nil "~8,'0X-~4,'0X-~4,'0X-~4,'0X-~12,'0X"
          (secure-random #xFFFFFFFF)
          (secure-random #xFFFF)
          (logior #x4000 (secure-random #x0FFF))  ; Version 4 UUID
          (logior #x8000 (secure-random #x3FFF))  ; Variant 1
          (secure-random #xFFFFFFFFFFFF)))

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

;;── Last Sync Timestamp Management ────────────────────────────────────────────

(defun last-sync-file ()
  "Return the path to the last sync timestamp file."
  (merge-pathnames "last-sync" (data-directory)))

(defun load-last-sync-timestamp ()
  "Load the last sync timestamp, or return empty string if none."
  (let ((file (last-sync-file)))
    (if (probe-file file)
        (string-trim '(#\Space #\Newline #\Tab #\Return)
                     (uiop:read-file-string file))
        "")))

(defun save-last-sync-timestamp (timestamp)
  "Save the last sync timestamp."
  (ensure-data-directory)
  (with-open-file (stream (last-sync-file)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string timestamp stream)))

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
     (let ((,db-var (or *db* (open-db)))) ; lint:suppress malformed-let
       (unwind-protect
            (progn ,@body) ; lint:suppress redundant-progn
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

    ;; Content-addressed blob storage for large text fields
    (sqlite:execute-non-query db "
      CREATE TABLE IF NOT EXISTS blobs (
        hash TEXT PRIMARY KEY,
        content TEXT NOT NULL
      )")

    ;; Content-addressed attachment storage for binary files (screenshots, etc.)
    (sqlite:execute-non-query db "
      CREATE TABLE IF NOT EXISTS attachments (
        hash TEXT PRIMARY KEY,
        content BLOB NOT NULL,
        filename TEXT NOT NULL,
        mime_type TEXT NOT NULL,
        size INTEGER NOT NULL,
        created_at TEXT NOT NULL
      )")

    ;; Migration: add description_hash column
    (handler-case
        (sqlite:execute-non-query db "
          ALTER TABLE todos ADD COLUMN description_hash TEXT")
      (error () nil))

    ;; Migration: add location_info_hash column
    (handler-case
        (sqlite:execute-non-query db "
          ALTER TABLE todos ADD COLUMN location_info_hash TEXT")
      (error () nil))

    ;; Migration: add attachment_hashes column
    (handler-case
        (sqlite:execute-non-query db "
          ALTER TABLE todos ADD COLUMN attachment_hashes TEXT")
      (error () nil))

    ;; Migrate existing inline descriptions to blobs
    (migrate-inline-to-blobs db)

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
            "INSERT INTO tag_presets (slot, tags) VALUES (?, NULL)" i))))

    ;; App settings table for user context and other settings
    (sqlite:execute-non-query db
      "CREATE TABLE IF NOT EXISTS app_settings (
         key TEXT PRIMARY KEY,
         value TEXT,
         updated_at TEXT NOT NULL
       )"))
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

;;── Content-Addressed Blob Storage ────────────────────────────────────────────

(defun content-hash (text)
  "Compute SHA-256 hash of TEXT as a hex string."
  (when (and text (stringp text) (> (length text) 0))
    (let* ((bytes (ironclad:digest-sequence
                   :sha256
                   (flexi-streams:string-to-octets text :external-format :utf-8)))
           (hex (make-string 64)))
      (loop for byte across bytes
            for i from 0 by 2
            do (let ((hi (ash byte -4))
                     (lo (logand byte #xf)))
                 (setf (char hex i) (char "0123456789abcdef" hi))
                 (setf (char hex (1+ i)) (char "0123456789abcdef" lo))))
      hex)))

(defun store-blob (db text)
  "Store TEXT in the blobs table, returning its hash. No-op if already stored."
  (when text
    (let ((hash (content-hash text)))
      (when hash
        (sqlite:execute-non-query db
          "INSERT OR IGNORE INTO blobs (hash, content) VALUES (?, ?)"
          hash text)
        hash))))

(defun resolve-blob (db hash)
  "Look up a blob by hash, returning its content or NIL."
  (when hash
    (sqlite:execute-single db
      "SELECT content FROM blobs WHERE hash = ?" hash)))

(defun migrate-inline-to-blobs (db)
  "Migrate existing inline descriptions and location_info to the blobs table.
   Only runs if there are rows with inline content but no hash."
  (let ((rows (sqlite:execute-to-list db "
    SELECT row_id, description, location_info
    FROM todos
    WHERE (description IS NOT NULL AND description_hash IS NULL)
       OR (location_info IS NOT NULL AND location_info_hash IS NULL)
    LIMIT 1000")))
    (when rows
      (dolist (row rows)
        (destructuring-bind (row-id description location-info) row
          (when (and description (not (eql description :null)))
            (let ((hash (store-blob db description)))
              (when hash
                (sqlite:execute-non-query db
                  "UPDATE todos SET description = NULL, description_hash = ? WHERE row_id = ?"
                  hash row-id))))
          (when (and location-info (not (eql location-info :null)))
            (let ((hash (store-blob db location-info)))
              (when hash
                (sqlite:execute-non-query db
                  "UPDATE todos SET location_info = NULL, location_info_hash = ? WHERE row_id = ?"
                  hash row-id))))))
      ;; Recurse if there are more rows (batched to avoid huge transactions)
      (when (= (length rows) 1000)
        (migrate-inline-to-blobs db)))))

;;── Content-Addressed Attachment Storage ──────────────────────────────────────

(defun content-hash-file (filepath)
  "Compute SHA256 hash of file contents."
  (let ((hash (ironclad:digest-file :sha256 (pathname filepath))))
    (ironclad:byte-array-to-hex-string hash)))

(defun guess-mime-type (filepath)
  "Guess MIME type from file extension."
  (let ((ext (pathname-type filepath)))
    (cond
      ((and ext (string-equal ext "png")) "image/png")
      ((and ext (or (string-equal ext "jpg") (string-equal ext "jpeg"))) "image/jpeg")
      ((and ext (string-equal ext "gif")) "image/gif")
      ((and ext (string-equal ext "webp")) "image/webp")
      ((and ext (string-equal ext "bmp")) "image/bmp")
      (t "application/octet-stream"))))

(defun store-attachment (db filepath)
  "Store file in attachments table, returning its hash. Deduplicates by content hash."
  (when (probe-file filepath)
    (let* ((hash (content-hash-file filepath))
           (content (alexandria:read-file-into-byte-vector filepath))
           (filename (file-namestring filepath))
           (mime-type (guess-mime-type filepath))
           (size (length content))
           (created-at (now-iso)))
      ;; Insert or ignore (hash is PK, handles deduplication)
      (sqlite:execute-non-query db
        "INSERT OR IGNORE INTO attachments (hash, content, filename, mime_type, size, created_at)
         VALUES (?, ?, ?, ?, ?, ?)"
        hash content filename mime-type size created-at)
      hash)))

(defun resolve-attachment (db hash)
  "Retrieve attachment metadata and content by hash.
   Returns (values hash content filename mime-type size created-at) or NIL."
  (when hash
    (let ((row (sqlite:execute-one-row-m-v db
                 "SELECT hash, content, filename, mime_type, size, created_at
                  FROM attachments WHERE hash = ?" hash)))
      (when row
        (values-list row)))))

;;── App Settings Storage ──────────────────────────────────────────────────────

(defun db-load-setting (key)
  "Load a setting by KEY from the database, returning its value or NIL."
  (with-db (db)
    (sqlite:execute-single db
      "SELECT value FROM app_settings WHERE key = ?" key)))

(defun db-load-setting-with-timestamp (key)
  "Load a setting by KEY with its timestamp, returning (values value updated-at) or NIL."
  (with-db (db)
    (let ((row (sqlite:execute-to-list db
                 "SELECT value, updated_at FROM app_settings WHERE key = ?" key)))
      (when row
        (values (first (first row)) (second (first row)))))))

(defun db-save-setting (key value)
  "Save a SETTING with KEY and VALUE, auto-timestamping."
  (with-db (db)
    (sqlite:execute-non-query db
      "INSERT OR REPLACE INTO app_settings (key, value, updated_at)
       VALUES (?, ?, ?)"
      key value (now-iso))))

(defun db-load-all-settings ()
  "Return all settings as a hash table."
  (with-db (db)
    (let ((ht (make-hash-table :test 'equal))
          (rows (sqlite:execute-to-list db
                  "SELECT key, value, updated_at FROM app_settings")))
      (dolist (row rows)
        (destructuring-bind (key value updated-at) row
          (setf (gethash key ht)
                (list :value value :updated-at updated-at))))
      ht)))

(defun migrate-user-context-to-db ()
  "One-time migration of context.txt from file to database.
   Renames the file to context.txt.migrated after successful migration."
  (let ((context-file (merge-pathnames "context.txt" (config-directory))))
    (when (probe-file context-file)
      ;; Check if already migrated
      (unless (db-load-setting "user_context")
        (let ((content (uiop:read-file-string context-file)))
          (when (and content (> (length content) 0))
            ;; Save to database
            (db-save-setting "user_context" content)
            ;; Rename file
            (let ((backup-file (merge-pathnames "context.txt.migrated"
                                               (config-directory))))
              (rename-file context-file backup-file)
              (format t "~&Migrated user context to database (backup: ~A)~%"
                      backup-file))))))))

;;── Row to TODO Conversion ────────────────────────────────────────────────────

(defun row-to-todo (row)
  "Convert a database row to a TODO object.
   ROW is a list: (row_id id title description priority status scheduled_date
                  due_date tags estimated_minutes location_info url parent_id
                  created_at completed_at valid_from valid_to device_id
                  repeat_interval repeat_unit enriching_p attachment_hashes)"
  (destructuring-bind (row-id id title description priority status
                       scheduled-date due-date tags estimated-minutes
                       location-info url parent-id created-at completed-at
                       valid-from valid-to device-id
                       &optional repeat-interval repeat-unit enriching-p attachment-hashes) row
    (declare (ignore row-id valid-from valid-to parent-id))
    (make-instance 'todo
                   :id id
                   :title title
                   :description (unless (eql description :null) description)
                   :priority (intern (string-upcase priority) :keyword)
                   :status (intern (string-upcase status) :keyword)
                   :scheduled-date (parse-timestamp scheduled-date)
                   :due-date (parse-timestamp due-date)
                   :tags (when (and tags (not (eql tags :null)))
                           (coerce (jzon:parse tags) 'list))
                   :estimated-minutes (unless (eql estimated-minutes :null)
                                        estimated-minutes)
                   :location-info (when (and location-info (not (eql location-info :null)))
                                    (let ((ht (jzon:parse location-info)))
                                      (when (hash-table-p ht)
                                        (list :name (gethash "name" ht)
                                              :address (gethash "address" ht)
                                              :phone (gethash "phone" ht)
                                              :map-url (gethash "map_url" ht)
                                              :website (gethash "website" ht)))))
                   :url (unless (eql url :null) url)
                   :attachment-hashes (when (and attachment-hashes (not (eql attachment-hashes :null)))
                                        (coerce (jzon:parse attachment-hashes) 'list))
                   :device-id (unless (eql device-id :null) device-id)
                   :repeat-interval (unless (or (null repeat-interval) (eql repeat-interval :null))
                                      repeat-interval)
                   :repeat-unit (when (and repeat-unit (not (eql repeat-unit :null)) (stringp repeat-unit))
                                  (intern (string-upcase repeat-unit) :keyword))
                   :enriching-p (and enriching-p (not (eql enriching-p :null)) (= enriching-p 1))
                   :created-at (lt:parse-timestring created-at)
                   :completed-at (parse-timestamp completed-at))))

;;── Load TODOs ────────────────────────────────────────────────────────────────

(defun db-load-todos ()
  "Load current (non-superseded) TODOs from the database."
  (with-db (db)
    (let ((rows (sqlite:execute-to-list db "
      SELECT t.row_id, t.id, t.title,
             COALESCE(b1.content, t.description) as description,
             t.priority, t.status,
             t.scheduled_date, t.due_date, t.tags, t.estimated_minutes,
             COALESCE(b2.content, t.location_info) as location_info,
             t.url, t.parent_id, t.created_at, t.completed_at,
             t.valid_from, t.valid_to, t.device_id, t.repeat_interval,
             t.repeat_unit, t.enriching_p, t.attachment_hashes
      FROM todos t
      LEFT JOIN blobs b1 ON t.description_hash = b1.hash
      LEFT JOIN blobs b2 ON t.location_info_hash = b2.hash
      WHERE t.valid_to IS NULL
      ORDER BY t.created_at DESC")))
      (mapcar #'row-to-todo rows))))

(defun db-load-todos-at (timestamp)
  "Load TODOs as they existed at TIMESTAMP (time-travel query).
   TIMESTAMP should be a local-time timestamp or ISO string."
  (let ((ts (if (stringp timestamp)
                timestamp
                (lt:format-rfc3339-timestring nil timestamp))))
    (with-db (db)
      (let ((rows (sqlite:execute-to-list db "
        SELECT t.row_id, t.id, t.title,
               COALESCE(b1.content, t.description) as description,
               t.priority, t.status,
               t.scheduled_date, t.due_date, t.tags, t.estimated_minutes,
               COALESCE(b2.content, t.location_info) as location_info,
               t.url, t.parent_id, t.created_at, t.completed_at,
               t.valid_from, t.valid_to, t.device_id, t.repeat_interval,
               t.repeat_unit, t.enriching_p, t.attachment_hashes
        FROM todos t
        LEFT JOIN blobs b1 ON t.description_hash = b1.hash
        LEFT JOIN blobs b2 ON t.location_info_hash = b2.hash
        WHERE t.valid_from <= ?
          AND (t.valid_to IS NULL OR t.valid_to > ?)
        ORDER BY t.created_at DESC" ts ts)))
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
        nil  ; parent-id removed - nested TODOs no longer supported
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
        (if (todo-enriching-p todo) 1 0)
        ;; Attachment hashes
        (when (todo-attachment-hashes todo)
          (jzon:stringify (coerce (todo-attachment-hashes todo) 'vector)))))

(defun db-save-todo (todo &key valid-from)
  "Save a TODO to the database using append-only semantics.
   If the TODO already exists (by ID), the old version is marked as superseded.
   Large text fields (description, location_info) are stored in the blobs table.
   VALID-FROM can be specified for sync operations to preserve original timestamp.
   Returns T if saved, NIL if rejected due to conflict (incoming timestamp older than current)."
  (with-db (db)
    (let ((now (or valid-from (now-iso)))
          (values (todo-to-db-values todo))
          (committed nil))
      ;; Check for timestamp conflict - reject if incoming is older than current
      (let ((current-timestamp (sqlite:execute-single db
                                  "SELECT valid_from FROM todos WHERE id = ? AND valid_to IS NULL"
                                  (todo-id todo))))
        (when (and current-timestamp valid-from
                   (string< valid-from current-timestamp))
          (llog:warn "Rejecting stale update" :id (todo-id todo)
                     :incoming valid-from :current current-timestamp)
          (return-from db-save-todo nil)))
      ;; Store large text fields as blobs
      (let ((desc-hash (store-blob db (third values)))
            (loc-hash (store-blob db (tenth values))))
        ;; Start a transaction for atomicity
        (sqlite:execute-non-query db "BEGIN IMMEDIATE")
        (unwind-protect
             (progn
               ;; Mark any existing current version as superseded
               (sqlite:execute-non-query db "
                 UPDATE todos SET valid_to = ?
                 WHERE id = ? AND valid_to IS NULL"
                 now (todo-id todo))
               ;; Insert the new version (description/location_info as hashes)
               (sqlite:execute-non-query db "
                 INSERT INTO todos (id, title, description_hash, priority, status,
                                    scheduled_date, due_date, tags, estimated_minutes,
                                    location_info_hash, url, parent_id, created_at,
                                    completed_at, valid_from, valid_to, device_id,
                                    repeat_interval, repeat_unit, enriching_p, attachment_hashes)
                 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULL, ?, ?, ?, ?, ?)"
                 (first values) (second values) desc-hash (fourth values)
                 (fifth values) (sixth values) (seventh values) (eighth values)
                 (ninth values) loc-hash (nth 10 values) (nth 11 values)
                 (nth 12 values) (nth 13 values) now (nth 14 values)
                 (nth 15 values) (nth 16 values) (nth 17 values) (nth 18 values))
               (sqlite:execute-non-query db "COMMIT")
               (setf committed t)
               t)  ; Return T on success
          (unless committed
            (ignore-errors (sqlite:execute-non-query db "ROLLBACK"))))))))

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
             ;; Insert all TODOs as new versions (with blob storage)
             (dolist (todo unique-todos)
               (let* ((values (todo-to-db-values todo))
                      (desc-hash (store-blob db (third values)))
                      (loc-hash (store-blob db (tenth values))))
                 (sqlite:execute-non-query db "
                   INSERT INTO todos (id, title, description_hash, priority, status,
                                      scheduled_date, due_date, tags, estimated_minutes,
                                      location_info_hash, url, parent_id, created_at,
                                      completed_at, valid_from, valid_to, device_id,
                                      repeat_interval, repeat_unit, enriching_p, attachment_hashes)
                   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULL, ?, ?, ?, ?, ?)"
                   (first values) (second values) desc-hash (fourth values)
                   (fifth values) (sixth values) (seventh values) (eighth values)
                   (ninth values) loc-hash (nth 10 values) (nth 11 values)
                   (nth 12 values) (nth 13 values) now (nth 14 values)
                   (nth 15 values) (nth 16 values) (nth 17 values) (nth 18 values))))
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
          (when (and tags (not (eql tags :null)) (< slot 10))
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
          (when (and tags (listp tags) (not (eq tags 'null)) (not (eql tags :null)))
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
                     (let* ((values (todo-to-db-values todo))
                            (desc-hash (store-blob db (third values)))
                            (loc-hash (store-blob db (tenth values))))
                       (sqlite:execute-non-query db "
                         INSERT INTO todos (id, title, description_hash, priority, status,
                                            scheduled_date, due_date, tags, estimated_minutes,
                                            location_info_hash, url, parent_id, created_at,
                                            completed_at, valid_from, valid_to, device_id,
                                            repeat_interval, repeat_unit, enriching_p, attachment_hashes)
                         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULL, ?, ?, ?, ?, ?)"
                         (first values) (second values) desc-hash (fourth values)
                         (fifth values) (sixth values) (seventh values) (eighth values)
                         (ninth values) loc-hash (nth 10 values) (nth 11 values)
                         (nth 12 values) (nth 13 values) now (nth 14 values)
                         (nth 15 values) (nth 16 values) (nth 17 values) (nth 18 values))))
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
         (init-db))))
    ;; Always attempt user context migration (idempotent)
    (migrate-user-context-to-db)))

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

;; User context - redefine to use database
(defvar *settings-change-hook* nil
  "Function to call when a setting changes. Called with (key value).
   Used by sync server to broadcast settings changes to connected clients.")

(defun load-user-context ()
  "Load the user context from the database.
   Falls back to default template if not present."
  (ensure-db-initialized)
  (or (db-load-setting "user_context")
      *default-user-context*))

(defun save-user-context (content)
  "Save user context to database and notify sync clients."
  (ensure-db-initialized)
  (db-save-setting "user_context" content)
  ;; Notify sync clients
  (when (and *settings-change-hook* (not *suppress-change-notifications*))
    (handler-case
        (funcall *settings-change-hook* "user_context" content)
      (error (e)
        (llog:warn "Settings change notification failed: ~A" e)))))

;; Time-travel API
(defun load-todos-at (timestamp)
  "Load TODOs as they existed at TIMESTAMP.
   TIMESTAMP can be a local-time timestamp or ISO 8601 string."
  (ensure-db-initialized)
  (db-load-todos-at timestamp))

;;── Sync API ─────────────────────────────────────────────────────────────────

(defun db-null-to-json-null (value)
  "Convert a database value to JSON-safe value. :null and nil become 'null."
  (if (or (eql value :null) (null value))
      'null
      value))

(defun row-to-sync-hash-table (row)
  "Convert a database row to a hash table for JSON sync response.
   Includes all fields including row_id, valid_from, valid_to for sync purposes."
  (destructuring-bind (row-id id title description priority status
                       scheduled-date due-date tags estimated-minutes
                       location-info url parent-id created-at completed-at
                       valid-from valid-to device-id
                       &optional repeat-interval repeat-unit enriching-p attachment-hashes) row
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
      (setf (gethash "parent_id" ht) 'null)  ; parent-id no longer used
      (setf (gethash "created_at" ht) created-at)
      (setf (gethash "completed_at" ht) (db-null-to-json-null completed-at))
      (setf (gethash "valid_from" ht) valid-from)
      (setf (gethash "valid_to" ht) (db-null-to-json-null valid-to))
      (setf (gethash "device_id" ht) device-id)
      (setf (gethash "repeat_interval" ht) (db-null-to-json-null repeat-interval))
      (setf (gethash "repeat_unit" ht) (db-null-to-json-null repeat-unit))
      (setf (gethash "enriching_p" ht) (db-null-to-json-null enriching-p))
      (setf (gethash "attachment_hashes" ht) (db-null-to-json-null attachment-hashes))
      ht)))

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
        SELECT t.row_id, t.id, t.title,
               COALESCE(b1.content, t.description) as description,
               t.priority, t.status,
               t.scheduled_date, t.due_date, t.tags, t.estimated_minutes,
               COALESCE(b2.content, t.location_info) as location_info,
               t.url, t.parent_id, t.created_at, t.completed_at,
               t.valid_from, t.valid_to, t.device_id, t.repeat_interval,
               t.repeat_unit, t.enriching_p, t.attachment_hashes
        FROM todos t
        LEFT JOIN blobs b1 ON t.description_hash = b1.hash
        LEFT JOIN blobs b2 ON t.location_info_hash = b2.hash
        WHERE t.valid_from > ? AND t.valid_to IS NULL
        ORDER BY t.valid_from ASC" ts)))
        (mapcar #'row-to-sync-hash-table rows)))))

