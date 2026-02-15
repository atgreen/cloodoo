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
    ;; Uses composite PK (key, user_id) for multi-user isolation.
    (sqlite:execute-non-query db
      "CREATE TABLE IF NOT EXISTS app_settings (
         key TEXT NOT NULL,
         value TEXT,
         updated_at TEXT NOT NULL,
         user_id TEXT NOT NULL DEFAULT 'default',
         PRIMARY KEY (key, user_id)
       )")

    ;;── List Management Tables ──────────────────────────────────────────────

    ;; List definitions (temporal)
    (sqlite:execute-non-query db "
      CREATE TABLE IF NOT EXISTS list_definitions (
        row_id INTEGER PRIMARY KEY AUTOINCREMENT,
        id TEXT NOT NULL,
        name TEXT NOT NULL,
        description TEXT,
        sections TEXT,
        created_at TEXT NOT NULL,
        device_id TEXT NOT NULL DEFAULT 'unknown',
        valid_from TEXT NOT NULL,
        valid_to TEXT
      )")

    ;; Indexes for list definitions
    (sqlite:execute-non-query db "
      CREATE INDEX IF NOT EXISTS idx_list_defs_current
      ON list_definitions(id, valid_to) WHERE valid_to IS NULL")

    ;; Case-insensitive unique name index for current definitions
    (handler-case
        (sqlite:execute-non-query db "
          CREATE UNIQUE INDEX IF NOT EXISTS idx_list_defs_unique_name
          ON list_definitions(LOWER(name)) WHERE valid_to IS NULL")
      (error () nil))  ; May fail if index exists with different definition

    ;; List items (temporal, no SQL FK — app-level integrity)
    (sqlite:execute-non-query db "
      CREATE TABLE IF NOT EXISTS list_items (
        row_id INTEGER PRIMARY KEY AUTOINCREMENT,
        id TEXT NOT NULL,
        list_id TEXT NOT NULL,
        title TEXT NOT NULL,
        section TEXT,
        checked INTEGER DEFAULT 0,
        notes TEXT,
        created_at TEXT NOT NULL,
        device_id TEXT NOT NULL DEFAULT 'unknown',
        valid_from TEXT NOT NULL,
        valid_to TEXT
      )")

    ;; Indexes for list items
    (sqlite:execute-non-query db "
      CREATE INDEX IF NOT EXISTS idx_list_items_current
      ON list_items(id, valid_to) WHERE valid_to IS NULL")

    (sqlite:execute-non-query db "
      CREATE INDEX IF NOT EXISTS idx_list_items_list_id
      ON list_items(list_id) WHERE valid_to IS NULL")

    ;; Device capabilities table for sync capability negotiation
    (sqlite:execute-non-query db "
      CREATE TABLE IF NOT EXISTS device_capabilities (
        device_id TEXT PRIMARY KEY,
        capabilities TEXT,
        last_seen TEXT,
        device_name TEXT
      )")

    ;;── Multi-User Migrations ──────────────────────────────────────────────

    ;; Migration: add user_id column to todos
    (handler-case
        (sqlite:execute-non-query db "
          ALTER TABLE todos ADD COLUMN user_id TEXT NOT NULL DEFAULT 'default'")
      (error () nil))

    ;; Migration: add user_id column to list_definitions
    (handler-case
        (sqlite:execute-non-query db "
          ALTER TABLE list_definitions ADD COLUMN user_id TEXT NOT NULL DEFAULT 'default'")
      (error () nil))

    ;; Migration: add user_id column to list_items
    (handler-case
        (sqlite:execute-non-query db "
          ALTER TABLE list_items ADD COLUMN user_id TEXT NOT NULL DEFAULT 'default'")
      (error () nil))

    ;; Migration: add user_id column to app_settings and recreate with composite PK.
    ;; app_settings originally had key TEXT PRIMARY KEY; multi-user needs (key, user_id).
    (handler-case
        (sqlite:execute-non-query db "
          ALTER TABLE app_settings ADD COLUMN user_id TEXT NOT NULL DEFAULT 'default'")
      (error () nil))

    ;; Migrate app_settings to composite PK (key, user_id) if needed.
    ;; Check if the table still has single-column PK by trying to insert two rows
    ;; with the same key but different user_id. If that fails, recreate the table.
    (handler-case
        (progn
          ;; Test if composite PK is already in place
          (sqlite:execute-non-query db "
            INSERT INTO app_settings (key, value, updated_at, user_id)
            VALUES ('__pk_test__', '', '', '__a__')")
          (sqlite:execute-non-query db "
            INSERT INTO app_settings (key, value, updated_at, user_id)
            VALUES ('__pk_test__', '', '', '__b__')")
          ;; If we get here, composite PK is already in place. Clean up.
          (sqlite:execute-non-query db
            "DELETE FROM app_settings WHERE key = '__pk_test__'"))
      (error ()
        ;; Composite PK not in place. Recreate the table.
        (ignore-errors
          (sqlite:execute-non-query db
            "DELETE FROM app_settings WHERE key = '__pk_test__'"))
        (handler-case
            (progn
              (sqlite:execute-non-query db "
                CREATE TABLE app_settings_new (
                  key TEXT NOT NULL,
                  value TEXT,
                  updated_at TEXT NOT NULL,
                  user_id TEXT NOT NULL DEFAULT 'default',
                  PRIMARY KEY (key, user_id)
                )")
              (sqlite:execute-non-query db "
                INSERT INTO app_settings_new (key, value, updated_at, user_id)
                SELECT key, value, updated_at, user_id FROM app_settings")
              (sqlite:execute-non-query db "DROP TABLE app_settings")
              (sqlite:execute-non-query db "ALTER TABLE app_settings_new RENAME TO app_settings"))
          (error () nil))))

    ;; Indexes for user-scoped queries
    (sqlite:execute-non-query db "
      CREATE INDEX IF NOT EXISTS idx_todos_user_current
      ON todos(user_id, valid_to) WHERE valid_to IS NULL")

    (sqlite:execute-non-query db "
      CREATE INDEX IF NOT EXISTS idx_list_defs_user_current
      ON list_definitions(user_id, valid_to) WHERE valid_to IS NULL")

    (sqlite:execute-non-query db "
      CREATE INDEX IF NOT EXISTS idx_list_items_user_current
      ON list_items(user_id, valid_to) WHERE valid_to IS NULL"))
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
    (let ((result (multiple-value-list
                   (sqlite:execute-one-row-m-v db
                     "SELECT hash, content, filename, mime_type, size, created_at
                      FROM attachments WHERE hash = ?" hash))))
      (cond
        ((null result) nil)
        ;; Some sqlite versions return a single LIST row.
        ((and (= (length result) 1) (listp (first result)))
         (destructuring-bind (stored-hash content filename mime-type size created-at)
             (first result)
           (values stored-hash content filename mime-type size created-at)))
        ;; Otherwise, treat the multiple values as the row.
        (t (values-list result))))))

;;── App Settings Storage ──────────────────────────────────────────────────────

(defun db-load-setting (key)
  "Load a setting by KEY from the database, returning its value or NIL."
  (with-db (db)
    (sqlite:execute-single db
      "SELECT value FROM app_settings WHERE key = ?" key)))

(defun db-load-setting-with-timestamp (key &key user-id)
  "Load a setting by KEY with its timestamp, returning (values value updated-at) or NIL.
   USER-ID scopes the query for multi-user sync; NIL queries without user filtering."
  (with-db (db)
    (let ((row (if user-id
                   (sqlite:execute-to-list db
                     "SELECT value, updated_at FROM app_settings WHERE key = ? AND user_id = ?"
                     key user-id)
                   (sqlite:execute-to-list db
                     "SELECT value, updated_at FROM app_settings WHERE key = ?" key))))
      (when row
        (values (first (first row)) (second (first row)))))))

(defun db-save-setting (key value &key user-id updated-at)
  "Save a SETTING with KEY and VALUE.
   UPDATED-AT overrides the timestamp (for sync); defaults to now.
   USER-ID scopes the setting for multi-user sync; NIL for standalone mode."
  (with-db (db)
    (let ((ts (or updated-at (now-iso))))
      (if user-id
          (sqlite:execute-non-query db
            "INSERT OR REPLACE INTO app_settings (key, value, updated_at, user_id)
             VALUES (?, ?, ?, ?)"
            key value ts user-id)
          (sqlite:execute-non-query db
            "INSERT OR REPLACE INTO app_settings (key, value, updated_at, user_id)
             VALUES (?, ?, ?, 'default')"
            key value ts)))))

(defun db-load-all-settings (&key user-id)
  "Return all settings as a hash table.
   USER-ID scopes the query for multi-user sync; NIL returns all settings."
  (with-db (db)
    (let ((ht (make-hash-table :test 'equal))
          (rows (if user-id
                    (sqlite:execute-to-list db
                      "SELECT key, value, updated_at FROM app_settings WHERE user_id = ?"
                      user-id)
                    (sqlite:execute-to-list db
                      "SELECT key, value, updated_at FROM app_settings"))))
      (dolist (row rows)
        (destructuring-bind (key value updated-at) row
          (setf (gethash key ht)
                (list :value value :updated-at updated-at))))
      ht)))

(defun migrate-user-context-to-db ()
  "One-time migration of context.txt from file to database.
   Renames the file to context.txt.migrated after successful migration."
  (let ((context-file (merge-pathnames "context.txt" (config-directory))))
    (when (and (probe-file context-file)
               (not (db-load-setting "user_context")))
      (let ((content (uiop:read-file-string context-file)))
        (when (and content (> (length content) 0))
          ;; Save to database
          (db-save-setting "user_context" content)
          ;; Rename file
          (let ((backup-file (merge-pathnames "context.txt.migrated"
                                             (config-directory))))
            (rename-file context-file backup-file)
            (format t "~&Migrated user context to database (backup: ~A)~%"
                    backup-file)))))))

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
    (declare (ignore row-id valid-from valid-to parent-id estimated-minutes))
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
        nil  ; estimated_minutes removed
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

(defun db-save-todo (todo &key valid-from user-id)
  "Save a TODO to the database using append-only semantics.
   If the TODO already exists (by ID), the old version is marked as superseded.
   Large text fields (description, location_info) are stored in the blobs table.
   VALID-FROM can be specified for sync operations to preserve original timestamp.
   USER-ID scopes the row for multi-user sync; NIL for standalone mode.
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
      ;; Skip if data is unchanged
      (let ((current (sqlite:execute-to-list db
                       "SELECT title, description_hash, priority, status, scheduled_date,
                               due_date, tags, estimated_minutes, location_info_hash, url,
                               parent_id, completed_at, repeat_interval, repeat_unit
                        FROM todos WHERE id = ? AND valid_to IS NULL"
                       (todo-id todo))))
        (when (and current (= (length current) 1))
          (let* ((cur (first current))
                 (desc-hash (store-blob db (third values)))
                 (loc-hash (store-blob db (tenth values))))
            (when (and (equal (nth 0 cur) (second values))      ; title
                       (equal (nth 1 cur) desc-hash)              ; description
                       (equal (nth 2 cur) (fourth values))        ; priority
                       (equal (nth 3 cur) (fifth values))         ; status
                       (equal (nth 4 cur) (sixth values))         ; scheduled_date
                       (equal (nth 5 cur) (seventh values))       ; due_date
                       (equal (nth 6 cur) (eighth values))        ; tags
                       (eql   (nth 7 cur) (ninth values))         ; estimated_minutes
                       (equal (nth 8 cur) loc-hash)               ; location_info
                       (equal (nth 9 cur) (nth 10 values))        ; url
                       (equal (nth 10 cur) (nth 11 values))       ; parent_id
                       (equal (nth 11 cur) (nth 13 values))       ; completed_at
                       (eql   (nth 12 cur) (nth 15 values))       ; repeat_interval
                       (equal (nth 13 cur) (nth 16 values)))      ; repeat_unit
              (return-from db-save-todo t)))))
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
               (apply #'sqlite:execute-non-query db
                 (if user-id
                     "INSERT INTO todos (id, title, description_hash, priority, status,
                                        scheduled_date, due_date, tags, estimated_minutes,
                                        location_info_hash, url, parent_id, created_at,
                                        completed_at, valid_from, valid_to, device_id,
                                        repeat_interval, repeat_unit, enriching_p, attachment_hashes, user_id)
                     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULL, ?, ?, ?, ?, ?, ?)"
                     "INSERT INTO todos (id, title, description_hash, priority, status,
                                        scheduled_date, due_date, tags, estimated_minutes,
                                        location_info_hash, url, parent_id, created_at,
                                        completed_at, valid_from, valid_to, device_id,
                                        repeat_interval, repeat_unit, enriching_p, attachment_hashes)
                     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULL, ?, ?, ?, ?, ?)")
                 (append (list (first values) (second values) desc-hash (fourth values)
                               (fifth values) (sixth values) (seventh values) (eighth values)
                               (ninth values) loc-hash (nth 10 values) (nth 11 values)
                               (nth 12 values) (nth 13 values) now (nth 14 values)
                               (nth 15 values) (nth 16 values) (nth 17 values) (nth 18 values))
                         (when user-id (list user-id))))
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

(defun db-delete-todo (todo-id &key user-id)
  "Delete a TODO by marking all its versions as superseded.
   USER-ID scopes the delete for multi-user sync; NIL for standalone mode.
   The data remains for time-travel queries."
  (with-db (db)
    (let ((now (now-iso)))
      (if user-id
          (sqlite:execute-non-query db "
            UPDATE todos SET valid_to = ?
            WHERE id = ? AND valid_to IS NULL AND user_id = ?"
            now todo-id user-id)
          (sqlite:execute-non-query db "
            UPDATE todos SET valid_to = ?
            WHERE id = ? AND valid_to IS NULL"
            now todo-id)))))

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

(defvar *db-initializing* nil
  "Guard against re-entrant calls to ensure-db-initialized.")

(defun ensure-db-initialized ()
  "Ensure the database is initialized, migrating from JSON if needed."
  (when (and *use-sqlite* (not *db-initializing*))
    (let ((*db-initializing* t))
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
      (migrate-user-context-to-db)
      ;; Ensure the built-in Groceries list exists (ensure-default-lists is defined
      ;; in lists.lisp, loaded after db.lisp, but this runs lazily at first use)
      (when (fboundp 'ensure-default-lists)
        (ensure-default-lists)))))

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

;;── List Definition CRUD ──────────────────────────────────────────────────────

(defun row-to-list-definition (row)
  "Convert a database row to a list-definition object.
   ROW is a list: (row_id id name description sections created_at device_id valid_from valid_to)."
  (destructuring-bind (row-id id name description sections created-at
                       device-id valid-from valid-to) row
    (declare (ignore row-id valid-from valid-to))
    (make-instance 'list-definition
                   :id id
                   :name name
                   :description (unless (or (eql description :null)
                                          (eql description 'null))
                                 description)
                   :sections (when (and sections
                                        (not (eql sections :null))
                                        (not (eql sections 'null)))
                               (coerce (jzon:parse sections) 'list))
                   :created-at (lt:parse-timestring created-at)
                   :device-id (unless (or (eql device-id :null)
                                          (eql device-id 'null))
                                device-id))))

(defun db-save-list-definition (list-def &key valid-from user-id)
  "Save a list definition using append-only temporal semantics.
   USER-ID scopes the row for multi-user sync; NIL for standalone mode.
   Returns T on success, NIL if the upsert was rejected (e.g., stale)."
  (with-db (db)
    (let ((now (or valid-from (now-iso)))
          (committed nil))
      ;; Skip if data is unchanged
      (let ((current (sqlite:execute-to-list db
                       "SELECT name, description, sections
                        FROM list_definitions WHERE id = ? AND valid_to IS NULL"
                       (list-def-id list-def))))
        (when (and current (= (length current) 1))
          (destructuring-bind (cur-name cur-desc cur-sections) (first current)
            (when (and (equal cur-name (list-def-name list-def))
                       (equal cur-desc (list-def-description list-def))
                       (equal cur-sections
                              (when (list-def-sections list-def)
                                (jzon:stringify (coerce (list-def-sections list-def) 'vector)))))
              (return-from db-save-list-definition t)))))
      (sqlite:execute-non-query db "BEGIN IMMEDIATE")
      (unwind-protect
           (progn
             ;; Check if this list was deleted more recently than the incoming upsert.
             ;; If the most recent row for this ID has valid_to > incoming valid_from,
             ;; then a delete happened after this upsert was created — reject it.
             (let ((most-recent-valid-to
                     (sqlite:execute-single db "
                       SELECT MAX(valid_to) FROM list_definitions
                       WHERE id = ? AND valid_to IS NOT NULL"
                       (list-def-id list-def))))
               (when (and most-recent-valid-to
                          (stringp most-recent-valid-to)
                          (string> most-recent-valid-to now))
                 (llog:info "Rejecting stale list upsert (deleted more recently)"
                            :id (list-def-id list-def)
                            :upsert-time now
                            :delete-time most-recent-valid-to)
                 (sqlite:execute-non-query db "ROLLBACK")
                 (setf committed t)  ; prevent double rollback in cleanup
                 (return-from db-save-list-definition nil)))
             ;; Mark existing current version as superseded (by id)
             (sqlite:execute-non-query db "
               UPDATE list_definitions SET valid_to = ?
               WHERE id = ? AND valid_to IS NULL"
               now (list-def-id list-def))
             ;; Also supersede any current row with the same normalized name
             ;; (handles merge when two devices independently created same-named list)
             (sqlite:execute-non-query db "
               UPDATE list_definitions SET valid_to = ?
               WHERE LOWER(name) = LOWER(?) AND valid_to IS NULL"
               now (list-def-name list-def))
             ;; Insert the new version
             (apply #'sqlite:execute-non-query db
               (if user-id
                   "INSERT INTO list_definitions (id, name, description, sections, created_at,
                                                  device_id, valid_from, valid_to, user_id)
                   VALUES (?, ?, ?, ?, ?, ?, ?, NULL, ?)"
                   "INSERT INTO list_definitions (id, name, description, sections, created_at,
                                                  device_id, valid_from, valid_to)
                   VALUES (?, ?, ?, ?, ?, ?, ?, NULL)")
               (append (list (list-def-id list-def)
                             (list-def-name list-def)
                             (list-def-description list-def)
                             (when (list-def-sections list-def)
                               (jzon:stringify (coerce (list-def-sections list-def) 'vector)))
                             (lt:format-rfc3339-timestring nil (list-def-created-at list-def))
                             (or (list-def-device-id list-def) (get-device-id))
                             now)
                       (when user-id (list user-id))))
             (sqlite:execute-non-query db "COMMIT")
             (setf committed t)
             ;; Notify sync
             (when (and *list-change-hook* (not *suppress-change-notifications*))
               (handler-case (funcall *list-change-hook* list-def)
                 (error (e) (llog:warn "List change notification failed: ~A" e))))
             t)
        (unless committed
          (ignore-errors (sqlite:execute-non-query db "ROLLBACK")))))))

(defun db-load-list-definitions ()
  "Load all current (non-superseded) list definitions."
  (ensure-db-initialized)
  (with-db (db)
    (let ((rows (sqlite:execute-to-list db "
      SELECT row_id, id, name, description, sections, created_at,
             device_id, valid_from, valid_to
      FROM list_definitions
      WHERE valid_to IS NULL
      ORDER BY name COLLATE NOCASE")))
      (mapcar #'row-to-list-definition rows))))

(defun db-find-list-by-name (name)
  "Find a current list definition by exact case-insensitive name match.
   Returns a list-definition or NIL."
  (with-db (db)
    (let ((rows (sqlite:execute-to-list db "
      SELECT row_id, id, name, description, sections, created_at,
             device_id, valid_from, valid_to
      FROM list_definitions
      WHERE LOWER(name) = LOWER(?) AND valid_to IS NULL"
      name)))
      (when rows
        (row-to-list-definition (first rows))))))

(defun db-find-list-by-id (list-id)
  "Find a current list definition by ID.
   Returns a list-definition or NIL."
  (with-db (db)
    (let ((rows (sqlite:execute-to-list db "
      SELECT row_id, id, name, description, sections, created_at,
             device_id, valid_from, valid_to
      FROM list_definitions
      WHERE id = ? AND valid_to IS NULL"
      list-id)))
      (when rows
        (row-to-list-definition (first rows))))))

(defun db-delete-list-definition (list-def-id &key user-id)
  "Delete a list definition and all its items by temporal close-out.
   USER-ID scopes the delete for multi-user sync; NIL for standalone mode.
   Both the definition and items are closed in one transaction.
   The built-in Groceries list cannot be deleted."
  (when (groceries-list-p list-def-id)
    (llog:warn "Cannot delete the Groceries list" :id list-def-id)
    (return-from db-delete-list-definition nil))
  (with-db (db)
    (let ((now (now-iso))
          (committed nil))
      (sqlite:execute-non-query db "BEGIN IMMEDIATE")
      (unwind-protect
           (progn
             ;; Close all current items on this list
             (if user-id
                 (sqlite:execute-non-query db "
                   UPDATE list_items SET valid_to = ?
                   WHERE list_id = ? AND valid_to IS NULL AND user_id = ?"
                   now list-def-id user-id)
                 (sqlite:execute-non-query db "
                   UPDATE list_items SET valid_to = ?
                   WHERE list_id = ? AND valid_to IS NULL"
                   now list-def-id))
             ;; Close the definition
             (if user-id
                 (sqlite:execute-non-query db "
                   UPDATE list_definitions SET valid_to = ?
                   WHERE id = ? AND valid_to IS NULL AND user_id = ?"
                   now list-def-id user-id)
                 (sqlite:execute-non-query db "
                   UPDATE list_definitions SET valid_to = ?
                   WHERE id = ? AND valid_to IS NULL"
                   now list-def-id))
             (sqlite:execute-non-query db "COMMIT")
             (setf committed t)
             ;; Notify sync
             (when (and *list-delete-hook* (not *suppress-change-notifications*))
               (handler-case (funcall *list-delete-hook* list-def-id)
                 (error (e) (llog:warn "List delete notification failed: ~A" e)))))
        (unless committed
          (ignore-errors (sqlite:execute-non-query db "ROLLBACK")))))))

;;── List Item CRUD ───────────────────────────────────────────────────────────

(defun row-to-list-item (row)
  "Convert a database row to a list-item object.
   ROW is a list: (row_id id list_id title section checked notes created_at device_id valid_from valid_to)."
  (destructuring-bind (row-id id list-id title section checked notes
                       created-at device-id valid-from valid-to) row
    (declare (ignore row-id valid-from valid-to))
    (make-instance 'list-item
                   :id id
                   :list-id list-id
                   :title title
                   :section (unless (or (eql section :null) (eql section 'null))
                              section)
                   :checked (and checked
                                 (not (eql checked :null))
                                 (not (eql checked 'null))
                                 (= checked 1))
                   :notes (unless (or (eql notes :null) (eql notes 'null))
                            notes)
                   :created-at (lt:parse-timestring created-at)
                   :device-id (unless (or (eql device-id :null) (eql device-id 'null))
                                device-id))))

(defun db-save-list-item (item &key valid-from user-id)
  "Save a list item using append-only temporal semantics.
   Validates that list_id references a current list definition.
   USER-ID scopes the row for multi-user sync; NIL for standalone mode.
   Returns T on success, NIL if list doesn't exist."
  (with-db (db)
    ;; Validate list_id exists
    (let ((list-exists (sqlite:execute-single db
                         "SELECT 1 FROM list_definitions WHERE id = ? AND valid_to IS NULL"
                         (list-item-list-id item))))
      (unless list-exists
        (llog:warn "Cannot save list item: list not found" :list-id (list-item-list-id item))
        (return-from db-save-list-item nil)))
    ;; Skip if data is unchanged
    (let ((current (sqlite:execute-to-list db
                     "SELECT list_id, title, section, checked, notes
                      FROM list_items WHERE id = ? AND valid_to IS NULL"
                     (list-item-id item))))
      (when (and current (= (length current) 1))
        (destructuring-bind (cur-list-id cur-title cur-section cur-checked cur-notes)
            (first current)
          (when (and (equal cur-list-id (list-item-list-id item))
                     (equal cur-title (list-item-title item))
                     (equal cur-section (list-item-section item))
                     (eql cur-checked (if (list-item-checked item) 1 0))
                     (equal cur-notes (list-item-notes item)))
            (return-from db-save-list-item t)))))
    (let ((now (or valid-from (now-iso)))
          (committed nil))
      (sqlite:execute-non-query db "BEGIN IMMEDIATE")
      (unwind-protect
           (progn
             ;; Mark existing current version as superseded
             (sqlite:execute-non-query db "
               UPDATE list_items SET valid_to = ?
               WHERE id = ? AND valid_to IS NULL"
               now (list-item-id item))
             ;; Insert the new version
             (apply #'sqlite:execute-non-query db
               (if user-id
                   "INSERT INTO list_items (id, list_id, title, section, checked, notes,
                                            created_at, device_id, valid_from, valid_to, user_id)
                   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, NULL, ?)"
                   "INSERT INTO list_items (id, list_id, title, section, checked, notes,
                                            created_at, device_id, valid_from, valid_to)
                   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, NULL)")
               (append (list (list-item-id item)
                             (list-item-list-id item)
                             (list-item-title item)
                             (list-item-section item)
                             (if (list-item-checked item) 1 0)
                             (list-item-notes item)
                             (lt:format-rfc3339-timestring nil (list-item-created-at item))
                             (or (list-item-device-id item) (get-device-id))
                             now)
                       (when user-id (list user-id))))
             (sqlite:execute-non-query db "COMMIT")
             (setf committed t)
             ;; Notify sync
             (when (and *list-item-change-hook* (not *suppress-change-notifications*))
               (handler-case (funcall *list-item-change-hook* item)
                 (error (e) (llog:warn "List item change notification failed: ~A" e))))
             t)
        (unless committed
          (ignore-errors (sqlite:execute-non-query db "ROLLBACK")))))))

(defun db-load-list-items (list-id)
  "Load all current items for a given list, ordered by section then title."
  (with-db (db)
    (let ((rows (sqlite:execute-to-list db "
      SELECT row_id, id, list_id, title, section, checked, notes,
             created_at, device_id, valid_from, valid_to
      FROM list_items
      WHERE list_id = ? AND valid_to IS NULL
      ORDER BY section COLLATE NOCASE, title COLLATE NOCASE"
      list-id)))
      (mapcar #'row-to-list-item rows))))

(defun db-check-list-item (item-id checked)
  "Toggle the checked state of a list item.
   CHECKED should be T or NIL. Returns T on success."
  (with-db (db)
    (let ((rows (sqlite:execute-to-list db "
      SELECT row_id, id, list_id, title, section, checked, notes,
             created_at, device_id, valid_from, valid_to
      FROM list_items
      WHERE id = ? AND valid_to IS NULL"
      item-id)))
      (when rows
        (let* ((item (row-to-list-item (first rows)))
               (now (now-iso)))
          ;; Close out old row
          (sqlite:execute-non-query db "
            UPDATE list_items SET valid_to = ?
            WHERE id = ? AND valid_to IS NULL"
            now item-id)
          ;; Insert updated row
          (sqlite:execute-non-query db "
            INSERT INTO list_items (id, list_id, title, section, checked, notes,
                                    created_at, device_id, valid_from)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
            (list-item-id item)
            (list-item-list-id item)
            (list-item-title item)
            (or (list-item-section item) "")
            (if checked 1 0)
            (or (list-item-notes item) "")
            (lt:format-rfc3339-timestring nil (list-item-created-at item))
            (or (list-item-device-id item) "")
            now)
          ;; Notify sync
          (setf (list-item-checked item) checked)
          (when (and *list-item-change-hook* (not *suppress-change-notifications*))
            (handler-case (funcall *list-item-change-hook* item)
              (error (e) (llog:warn "List item change notification failed: ~A" e))))
          t)))))

(defun db-delete-list-item (item-id &key user-id)
  "Delete a list item by temporal close-out.
   USER-ID scopes the delete for multi-user sync; NIL for standalone mode."
  (with-db (db)
    (let ((now (now-iso)))
      (if user-id
          (sqlite:execute-non-query db "
            UPDATE list_items SET valid_to = ?
            WHERE id = ? AND valid_to IS NULL AND user_id = ?"
            now item-id user-id)
          (sqlite:execute-non-query db "
            UPDATE list_items SET valid_to = ?
            WHERE id = ? AND valid_to IS NULL"
            now item-id))
      ;; Notify sync
      (when (and *list-item-delete-hook* (not *suppress-change-notifications*))
        (handler-case (funcall *list-item-delete-hook* item-id)
          (error (e) (llog:warn "List item delete notification failed: ~A" e)))))))

(defun db-find-list-item-by-title (list-id title)
  "Find a current list item by list-id and case-insensitive title match.
   Returns a list-item or NIL."
  (with-db (db)
    (let ((rows (sqlite:execute-to-list db "
      SELECT row_id, id, list_id, title, section, checked, notes,
             created_at, device_id, valid_from, valid_to
      FROM list_items
      WHERE list_id = ? AND LOWER(title) = LOWER(?) AND valid_to IS NULL"
      list-id title)))
      (when rows
        (row-to-list-item (first rows))))))

;;── Atomic Todo-to-List-Item Conversion ──────────────────────────────────────

(defun db-convert-todo-to-list-item (todo-id list-id title &key section notes)
  "Atomically convert a todo into a list item.
   Creates the list item and deletes the todo in a single transaction.
   Returns the new list-item on success, NIL on failure."
  (with-db (db)
    ;; Validate list exists
    (let ((list-exists (sqlite:execute-single db
                         "SELECT 1 FROM list_definitions WHERE id = ? AND valid_to IS NULL"
                         list-id)))
      (unless list-exists
        (llog:warn "Cannot convert: list not found" :list-id list-id)
        (return-from db-convert-todo-to-list-item nil)))
    ;; Validate todo exists
    (let ((todo-exists (sqlite:execute-single db
                         "SELECT 1 FROM todos WHERE id = ? AND valid_to IS NULL"
                         todo-id)))
      (unless todo-exists
        (llog:warn "Cannot convert: todo not found" :todo-id todo-id)
        (return-from db-convert-todo-to-list-item nil)))
    (let ((now (now-iso))
          (item-id (generate-id))
          (committed nil)
          (new-item nil))
      (sqlite:execute-non-query db "BEGIN IMMEDIATE")
      (unwind-protect
           (progn
             ;; Close the todo
             (sqlite:execute-non-query db "
               UPDATE todos SET valid_to = ?
               WHERE id = ? AND valid_to IS NULL"
               now todo-id)
             ;; Create the list item
             (sqlite:execute-non-query db "
               INSERT INTO list_items (id, list_id, title, section, checked, notes,
                                       created_at, device_id, valid_from, valid_to)
               VALUES (?, ?, ?, ?, 0, ?, ?, ?, ?, NULL)"
               item-id list-id title section notes
               now (get-device-id) now)
             (sqlite:execute-non-query db "COMMIT")
             (setf committed t)
             (setf new-item (make-instance 'list-item
                                           :id item-id
                                           :list-id list-id
                                           :title title
                                           :section section
                                           :notes notes
                                           :device-id (get-device-id)
                                           :created-at (lt:now)))
             new-item)
        (unless committed
          (ignore-errors (sqlite:execute-non-query db "ROLLBACK")))))))

;;── List Sync Helpers ────────────────────────────────────────────────────────

(defun db-load-current-list-rows-since (timestamp &key user-id)
  "Load current list definition rows where valid_from > timestamp.
   USER-ID scopes the query for multi-user sync; NIL returns all rows.
   Returns list of hash tables for sync."
  (ensure-db-initialized)
  (let ((ts (if (stringp timestamp) timestamp
                (lt:format-rfc3339-timestring nil timestamp))))
    (with-db (db)
      (let ((rows (if user-id
                      (sqlite:execute-to-list db "
        SELECT row_id, id, name, description, sections, created_at,
               device_id, valid_from, valid_to
        FROM list_definitions
        WHERE valid_from > ? AND valid_to IS NULL AND user_id = ?
        ORDER BY valid_from ASC" ts user-id)
                      (sqlite:execute-to-list db "
        SELECT row_id, id, name, description, sections, created_at,
               device_id, valid_from, valid_to
        FROM list_definitions
        WHERE valid_from > ? AND valid_to IS NULL
        ORDER BY valid_from ASC" ts))))
        (mapcar (lambda (row)
                  (destructuring-bind (row-id id name description sections created-at
                                      device-id valid-from valid-to) row
                    (declare (ignore row-id))
                    (let ((ht (make-hash-table :test #'equal)))
                      (setf (gethash "id" ht) id)
                      (setf (gethash "name" ht) name)
                      (setf (gethash "description" ht) (db-null-to-json-null description))
                      (setf (gethash "sections" ht) (db-null-to-json-null sections))
                      (setf (gethash "created_at" ht) created-at)
                      (setf (gethash "device_id" ht) device-id)
                      (setf (gethash "valid_from" ht) valid-from)
                      (setf (gethash "valid_to" ht) (db-null-to-json-null valid-to))
                      ht)))
                rows)))))

(defun db-load-current-list-item-rows-since (timestamp &key user-id)
  "Load current list item rows where valid_from > timestamp.
   USER-ID scopes the query for multi-user sync; NIL returns all rows.
   Returns list of hash tables for sync."
  (ensure-db-initialized)
  (let ((ts (if (stringp timestamp) timestamp
                (lt:format-rfc3339-timestring nil timestamp))))
    (with-db (db)
      (let ((rows (if user-id
                      (sqlite:execute-to-list db "
        SELECT row_id, id, list_id, title, section, checked, notes,
               created_at, device_id, valid_from, valid_to
        FROM list_items
        WHERE valid_from > ? AND valid_to IS NULL AND user_id = ?
        ORDER BY valid_from ASC" ts user-id)
                      (sqlite:execute-to-list db "
        SELECT row_id, id, list_id, title, section, checked, notes,
               created_at, device_id, valid_from, valid_to
        FROM list_items
        WHERE valid_from > ? AND valid_to IS NULL
        ORDER BY valid_from ASC" ts))))
        (mapcar (lambda (row)
                  (destructuring-bind (row-id id list-id title section checked notes
                                      created-at device-id valid-from valid-to) row
                    (declare (ignore row-id))
                    (let ((ht (make-hash-table :test #'equal)))
                      (setf (gethash "id" ht) id)
                      (setf (gethash "list_id" ht) list-id)
                      (setf (gethash "title" ht) title)
                      (setf (gethash "section" ht) (db-null-to-json-null section))
                      (setf (gethash "checked" ht) (and checked (not (eql checked :null)) (= checked 1)))
                      (setf (gethash "notes" ht) (db-null-to-json-null notes))
                      (setf (gethash "created_at" ht) created-at)
                      (setf (gethash "device_id" ht) device-id)
                      (setf (gethash "valid_from" ht) valid-from)
                      (setf (gethash "valid_to" ht) (db-null-to-json-null valid-to))
                      ht)))
                rows)))))

;;── Device Capabilities ─────────────────────────────────────────────────────

(defun db-save-device-capabilities (device-id capabilities &optional device-name)
  "Save or update device capabilities. CAPABILITIES is a list of strings."
  (with-db (db)
    (sqlite:execute-non-query db "
      INSERT INTO device_capabilities (device_id, capabilities, last_seen, device_name)
      VALUES (?, ?, ?, ?)
      ON CONFLICT(device_id) DO UPDATE SET
        capabilities = excluded.capabilities,
        last_seen = excluded.last_seen,
        device_name = COALESCE(excluded.device_name, device_capabilities.device_name)"
      device-id
      (jzon:stringify (coerce capabilities 'vector))
      (now-iso)
      device-name)))

(defun db-load-device-capabilities (device-id)
  "Load capabilities for a device. Returns a list of capability strings."
  (with-db (db)
    (let ((caps-json (sqlite:execute-single db
                       "SELECT capabilities FROM device_capabilities WHERE device_id = ?"
                       device-id)))
      (when (and caps-json (not (eql caps-json :null)))
        (coerce (jzon:parse caps-json) 'list)))))

(defun db-all-active-devices-have-capability-p (capability &key (days 90))
  "Check if all devices seen within DAYS have the given capability.
   Returns T if so (or if no devices at all)."
  (with-db (db)
    (let* ((cutoff (lt:format-rfc3339-timestring nil
                     (lt:timestamp- (lt:now) days :day)))
           (without (sqlite:execute-single db "
             SELECT COUNT(*) FROM device_capabilities
             WHERE last_seen > ?
               AND (capabilities IS NULL
                    OR capabilities NOT LIKE ?)"
             cutoff (format nil "%~A%" capability))))
      (or (null without) (eql without :null) (zerop without)))))

(defun db-remove-device-capabilities (device-id)
  "Remove a device from the capabilities table (for cert revoke)."
  (with-db (db)
    (sqlite:execute-non-query db
      "DELETE FROM device_capabilities WHERE device_id = ?"
      device-id)))

;;── User Data Migration ──────────────────────────────────────────────────────

(defun db-migrate-user-data (from-user-id to-user-id)
  "Migrate all data from FROM-USER-ID to TO-USER-ID.
   Updates todos, list_definitions, list_items, and app_settings.
   Returns a plist of counts for each table updated."
  (ensure-db-initialized)
  (with-db (db)
    (let ((committed nil)
          (todo-count 0)
          (list-def-count 0)
          (list-item-count 0)
          (settings-count 0))
      (sqlite:execute-non-query db "BEGIN IMMEDIATE")
      (unwind-protect
           (progn
             (setf todo-count
                   (progn
                     (sqlite:execute-non-query db
                       "UPDATE todos SET user_id = ? WHERE user_id = ?"
                       to-user-id from-user-id)
                     (sqlite:execute-single db "SELECT changes()")))
             (setf list-def-count
                   (progn
                     (sqlite:execute-non-query db
                       "UPDATE list_definitions SET user_id = ? WHERE user_id = ?"
                       to-user-id from-user-id)
                     (sqlite:execute-single db "SELECT changes()")))
             (setf list-item-count
                   (progn
                     (sqlite:execute-non-query db
                       "UPDATE list_items SET user_id = ? WHERE user_id = ?"
                       to-user-id from-user-id)
                     (sqlite:execute-single db "SELECT changes()")))
             (setf settings-count
                   (progn
                     (sqlite:execute-non-query db
                       "UPDATE app_settings SET user_id = ? WHERE user_id = ?"
                       to-user-id from-user-id)
                     (sqlite:execute-single db "SELECT changes()")))
             (sqlite:execute-non-query db "COMMIT")
             (setf committed t)
             (list :todos todo-count
                   :list-definitions list-def-count
                   :list-items list-item-count
                   :settings settings-count))
        (unless committed
          (ignore-errors (sqlite:execute-non-query db "ROLLBACK")))))))

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
    (declare (ignore estimated-minutes))
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

(defun db-load-current-rows-since (timestamp &key user-id)
  "Load only CURRENT (non-superseded) rows where valid_from > timestamp.
   This is the correct function for sync - it sends the latest state of each todo
   without replaying historical versions that could overwrite newer data.
   USER-ID scopes the query for multi-user sync; NIL returns all rows.
   Returns raw hash tables suitable for JSON serialization."
  (ensure-db-initialized)
  (let ((ts (if (stringp timestamp)
                timestamp
                (lt:format-rfc3339-timestring nil timestamp))))
    (with-db (db)
      (let ((rows (if user-id
                      (sqlite:execute-to-list db "
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
        WHERE t.valid_from > ? AND t.valid_to IS NULL AND t.user_id = ?
        ORDER BY t.valid_from ASC" ts user-id)
                      (sqlite:execute-to-list db "
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
        ORDER BY t.valid_from ASC" ts))))
        (mapcar #'row-to-sync-hash-table rows)))))
