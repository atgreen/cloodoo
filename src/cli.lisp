;;; cli.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cloodoo)

;;── Date Parsing ──────────────────────────────────────────────────────────────

(defun parse-due-date (date-string)
  "Parse a due date string. Supports:
   - 'today' or 'tod'
   - 'tomorrow' or 'tom'
   - 'next week' or 'nextweek'
   - Day names: 'monday', 'tuesday', etc.
   - ISO format: 'YYYY-MM-DD'
   - Short format: 'MM-DD' or 'M/D'"
  (when (and date-string (> (length date-string) 0))
    (let ((lower (string-downcase (str:trim date-string))))
      (cond
        ;; Today
        ((or (string= lower "today") (string= lower "tod"))
         (local-today))

        ;; Tomorrow
        ((or (string= lower "tomorrow") (string= lower "tom"))
         (lt:timestamp+ (local-today) 1 :day))

        ;; Next week
        ((or (string= lower "next week") (string= lower "nextweek"))
         (lt:timestamp+ (local-today) 7 :day))

        ;; Day names
        ((member lower '("sunday" "sun") :test #'string=)
         (next-weekday 0))
        ((member lower '("monday" "mon") :test #'string=)
         (next-weekday 1))
        ((member lower '("tuesday" "tue") :test #'string=)
         (next-weekday 2))
        ((member lower '("wednesday" "wed") :test #'string=)
         (next-weekday 3))
        ((member lower '("thursday" "thu") :test #'string=)
         (next-weekday 4))
        ((member lower '("friday" "fri") :test #'string=)
         (next-weekday 5))
        ((member lower '("saturday" "sat") :test #'string=)
         (next-weekday 6))

        ;; ISO format YYYY-MM-DD
        ((and (= (length lower) 10)
              (char= (char lower 4) #\-)
              (char= (char lower 7) #\-))
         (handler-case
             (lt:parse-timestring date-string)
           (error () nil)))

        ;; Try various date formats
        (t
         (handler-case
             (parse-flexible-date date-string)
           (error () nil)))))))

(defun next-weekday (target-day)
  "Return the next occurrence of TARGET-DAY (0=Sunday, 1=Monday, etc.)."
  (let* ((today (local-today))
         (current-day (lt:timestamp-day-of-week today))
         (days-ahead (mod (- target-day current-day) 7)))
    ;; If it's the same day, go to next week
    (when (zerop days-ahead)
      (setf days-ahead 7))
    (lt:timestamp+ today days-ahead :day)))

(defun parse-flexible-date (date-string)
  "Try to parse various date formats."
  (let* ((parts (or (str:split #\- date-string)
                    (str:split #\/ date-string)))
         (now (lt:now)))
    (cond
      ;; MM-DD or M/D format
      ((= (length parts) 2)
       (let ((month (parse-integer (first parts) :junk-allowed t))
             (day (parse-integer (second parts) :junk-allowed t)))
         (when (and month day
                    (>= month 1) (<= month 12)
                    (>= day 1) (<= day 31))
           (let ((year (lt:timestamp-year now)))
             ;; If the date has passed this year, use next year
             (let ((result (lt:encode-timestamp 0 0 0 0 day month year)))
               (if (lt:timestamp< result now)
                   (lt:encode-timestamp 0 0 0 0 day month (1+ year))
                   result))))))

      ;; MM-DD-YYYY or M/D/YYYY format
      ((= (length parts) 3)
       (let ((month (parse-integer (first parts) :junk-allowed t))
             (day (parse-integer (second parts) :junk-allowed t))
             (year (parse-integer (third parts) :junk-allowed t)))
         (when (and month day year
                    (>= month 1) (<= month 12)
                    (>= day 1) (<= day 31))
           ;; Handle 2-digit years
           (when (< year 100)
             (setf year (+ 2000 year)))
           (lt:encode-timestamp 0 0 0 0 day month year))))

      (t nil))))

;;── CLI Commands using clingon ─────────────────────────────────────────────────

(defun make-add-command ()
  "Create the 'add' subcommand."
  (let ((priority-opt (clingon:make-option
                       :string
                       :short-name #\p
                       :long-name "priority"
                       :key :priority
                       :initial-value "medium"
                       :description "Priority: high, medium, low"))
        (due-opt (clingon:make-option
                  :string
                  :short-name #\d
                  :long-name "due"
                  :key :due
                  :description "Due date (YYYY-MM-DD, 'today', 'tomorrow', or day name)"))
        (tags-opt (clingon:make-option
                   :list
                   :short-name #\t
                   :long-name "tag"
                   :key :tags
                   :description "Tag (can be repeated)"))
        (desc-opt (clingon:make-option
                   :string
                   :short-name #\n
                   :long-name "note"
                   :key :note
                   :description "Optional description/notes"))
        (attachment-opt (clingon:make-option
                         :filepath
                         :short-name #\a
                         :long-name "attachment"
                         :key :attachment
                         :description "Attach a file (screenshot, document, etc.)")))
    (clingon:make-command
     :name "add"
     :description "Add a new TODO"
     :usage "TITLE"
     :options (list priority-opt due-opt tags-opt desc-opt attachment-opt)
     :handler (lambda (cmd)
                (let ((args (clingon:command-arguments cmd))
                      (priority (clingon:getopt cmd :priority))
                      (due (clingon:getopt cmd :due))
                      (tags (clingon:getopt cmd :tags))
                      (note (clingon:getopt cmd :note))
                      (attachment-path (clingon:getopt cmd :attachment)))
                  (if args
                      (let* ((title (format nil "~{~A~^ ~}" args))
                             (due-date (when due (parse-due-date due)))
                             (todo (make-todo title
                                             :priority (intern (string-upcase priority) :keyword)
                                             :due-date due-date
                                             :description note
                                             :tags (parse-tags tags))))
                        ;; Handle attachment if provided
                        (when (and attachment-path (probe-file attachment-path))
                          (with-db (db)
                            (let ((hash (store-attachment db attachment-path)))
                              (setf (todo-attachment-hashes todo) (list hash)))))
                        (save-todo todo)
                        (format t "~A Added: ~A~%"
                               (tui:colored "✓" :fg tui:*fg-green*)
                               title)
                        (when attachment-path
                          (if (probe-file attachment-path)
                              (format t "  Attachment: ~A~%" (file-namestring attachment-path))
                              (format t "  ~A Attachment file not found: ~A~%"
                                     (tui:colored "⚠" :fg tui:*fg-yellow*) attachment-path)))
                        (when due-date
                          (format t "  Due: ~A~%"
                                 (lt:format-timestring nil due-date
                                                      :format '(:long-weekday ", " :long-month " " :day ", " :year)))))
                      (format t "Error: Please provide a title for the TODO~%")))))))

(defun make-list-command ()
  "Create the 'list' subcommand."
  (let ((status-opt (clingon:make-option
                     :string
                     :short-name #\s
                     :long-name "status"
                     :key :status
                     :description "Filter by status: pending, in-progress, completed"))
        (priority-opt (clingon:make-option
                       :string
                       :short-name #\p
                       :long-name "priority"
                       :key :priority
                       :description "Filter by priority: high, medium, low"))
        (all-opt (clingon:make-option
                  :flag
                  :short-name #\a
                  :long-name "all"
                  :key :all
                  :description "Show all TODOs including completed")))
    (clingon:make-command
     :name "list"
     :description "List TODOs"
     :options (list status-opt priority-opt all-opt)
     :handler (lambda (cmd)
                (let* ((status-filter (clingon:getopt cmd :status))
                       (priority-filter (clingon:getopt cmd :priority))
                       (show-all (clingon:getopt cmd :all))
                       (todos (load-todos))
                       (filtered todos))
                  ;; Apply filters
                  (when status-filter
                    (let ((status-key (intern (string-upcase status-filter) :keyword)))
                      (setf filtered (remove-if-not
                                     (lambda (item) (eq (todo-status item) status-key))
                                     filtered))))
                  (when priority-filter
                    (let ((priority-key (intern (string-upcase priority-filter) :keyword)))
                      (setf filtered (remove-if-not
                                     (lambda (item) (eq (todo-priority item) priority-key))
                                     filtered))))
                  ;; Unless --all, hide completed by default
                  (unless show-all
                    (setf filtered (remove-if
                                   (lambda (item) (eq (todo-status item) :completed))
                                   filtered)))
                  ;; Sort by priority
                  (setf filtered (sort (copy-list filtered)
                                      (lambda (a b)
                                        (> (priority-order (todo-priority a))
                                           (priority-order (todo-priority b))))))
                  ;; Display
                  (if filtered
                      (progn
                        (format t "~%")
                        (dolist (todo filtered)
                          (format t "~A ~A ~A"
                                 (org-status-colored (todo-status todo))
                                 (org-priority-colored (todo-priority todo))
                                 (todo-title todo))
                          (when (todo-due-date todo)
                            (format t " ~A" (format-due-date-cli (todo-due-date todo))))
                          (format t "~%"))
                        (format t "~%~A~%"
                               (tui:colored (format nil "~D item~:P" (length filtered))
                                           :fg tui:*fg-bright-black*)))
                      (format t "~%No items found.~%~%")))))))

(defun make-done-command ()
  "Create the 'done' subcommand to mark a TODO as completed."
  (clingon:make-command
   :name "done"
   :description "Mark a TODO as completed"
   :usage "SEARCH_TERM"
   :handler (lambda (cmd)
              (let* ((args (clingon:command-arguments cmd))
                     (search-term (when args (format nil "~{~A~^ ~}" args)))
                     (todos (load-todos)))
                (if search-term
                    (let ((matches (remove-if-not
                                   (lambda (item)
                                     (search (string-downcase search-term)
                                            (string-downcase (todo-title item))))
                                   todos)))
                      (cond
                        ((null matches)
                         (format t "No TODO found matching '~A'~%" search-term))
                        ((= (length matches) 1)
                         (let ((todo (first matches)))
                           (setf (todo-status todo) :completed)
                           (setf (todo-completed-at todo) (lt:now))
                           (save-todo todo)
                           (format t "~A Completed: ~A~%"
                                  (tui:colored "✓" :fg tui:*fg-green*)
                                  (todo-title todo))))
                        (t
                         (format t "Multiple matches found:~%")
                         (dolist (todo matches)
                           (format t "  - ~A~%" (todo-title todo)))
                         (format t "Please be more specific.~%"))))
                    (format t "Usage: cloodoo done SEARCH_TERM~%"))))))

(defun make-stats-command ()
  "Create the 'stats' subcommand."
  (clingon:make-command
   :name "stats"
   :description "Show TODO statistics"
   :handler (lambda (cmd)
              (declare (ignore cmd))
              (let* ((todos (load-todos))
                     (total (length todos))
                     (pending (count-if (lambda (item) (eq (todo-status item) :pending)) todos))
                     (in-progress (count-if (lambda (item) (eq (todo-status item) :in-progress)) todos))
                     (completed (count-if (lambda (item) (eq (todo-status item) :completed)) todos))
                     (high (count-if (lambda (item) (eq (todo-priority item) :high)) todos))
                     (overdue (count-if (lambda (item)
                                         (and (todo-due-date item)
                                              (not (eq (todo-status item) :completed))
                                              (lt:timestamp< (todo-due-date item) (lt:now))))
                                       todos)))
                (format t "~%~A~%~%"
                       (tui:bold (tui:colored "  CLOODOO STATS  " :fg tui:*fg-black* :bg tui:*bg-cyan*)))
                (format t "  Total TODOs:    ~D~%" total)
                (format t "  Pending:        ~A~%"
                       (tui:colored (format nil "~D" pending) :fg tui:*fg-white*))
                (format t "  In Progress:    ~A~%"
                       (tui:colored (format nil "~D" in-progress) :fg tui:*fg-cyan*))
                (format t "  Completed:      ~A~%"
                       (tui:colored (format nil "~D" completed) :fg tui:*fg-green*))
                (format t "~%")
                (format t "  High Priority:  ~A~%"
                       (tui:colored (format nil "~D" high) :fg tui:*fg-red*))
                (when (> overdue 0)
                  (format t "  Overdue:        ~A~%"
                         (tui:bold (tui:colored (format nil "~D" overdue) :fg tui:*fg-red*))))
                (format t "~%")))))

(defun make-compact-command ()
  "Create the 'compact' subcommand."
  (let ((yes-opt (clingon:make-option
                  :flag
                  :short-name #\y
                  :long-name "yes"
                  :key :yes
                  :description "Skip confirmation prompt")))
    (clingon:make-command
     :name "compact"
     :description "Remove old versions from the database to reclaim space"
     :options (list yes-opt)
     :handler (lambda (cmd)
                (block nil
                  (let ((skip-confirm (clingon:getopt cmd :yes)))
                    (ensure-db-initialized)
                  (with-db (db)
                    ;; Count before
                    (let ((before (caar (sqlite:execute-to-list db "SELECT COUNT(*) FROM todos")))
                          (current (caar (sqlite:execute-to-list db "SELECT COUNT(*) FROM todos WHERE valid_to IS NULL"))))
                      (format t "Current todos: ~D~%" current)
                      (format t "Total rows before: ~D~%" before)
                      (format t "Old versions to remove: ~D~%~%" (- before current))
                      (when (= before current)
                        (format t "Nothing to compact.~%")
                        (return))
                      ;; Ask for confirmation unless --yes flag
                      (unless skip-confirm
                        (format t "This will permanently delete ~D old versions.~%" (- before current))
                        (format t "This cannot be undone. Continue? [y/N] ")
                        (finish-output)
                        (let ((response (read-line *standard-input* nil "")))
                          (unless (member response '("y" "Y" "yes" "Yes" "YES") :test #'string=)
                            (format t "Aborted.~%")
                            (return))))
                      ;; Delete superseded rows
                      (sqlite:execute-non-query db "DELETE FROM todos WHERE valid_to IS NOT NULL")
                      ;; Remove orphaned blobs (no longer referenced by any row)
                      (sqlite:execute-non-query db "
                        DELETE FROM blobs WHERE hash NOT IN (
                          SELECT description_hash FROM todos WHERE description_hash IS NOT NULL
                          UNION
                          SELECT location_info_hash FROM todos WHERE location_info_hash IS NOT NULL)")
                      ;; Vacuum to reclaim space
                      (sqlite:execute-non-query db "VACUUM")
                      ;; Count after
                      (let ((after (caar (sqlite:execute-to-list db "SELECT COUNT(*) FROM todos"))))
                        (format t "~%Total rows after: ~D~%" after)
                        (format t "Removed ~D old versions.~%" (- before after)))))))))))

(defun sql-escape-string (str)
  "Escape a string for SQL insertion (single quotes doubled)."
  (if (or (null str) (eq str :null))
      "NULL"
      (format nil "'~A'" (str:replace-all "'" "''" str))))

(defun make-dump-command ()
  "Create the 'dump' subcommand."
  (clingon:make-command
   :name "dump"
   :description "Dump the SQLite database as SQL statements"
   :handler (lambda (cmd)
              (declare (ignore cmd))
              (ensure-db-initialized)
              (format t "-- Cloodoo database dump~%")
              (format t "-- Generated: ~A~%~%" (lt:format-rfc3339-timestring nil (lt:now)))
              (format t "BEGIN TRANSACTION;~%~%")
              ;; Output schema
              (format t "-- Table: todos~%")
              (format t "CREATE TABLE IF NOT EXISTS todos (
  row_id INTEGER PRIMARY KEY AUTOINCREMENT,
  id TEXT NOT NULL,
  title TEXT NOT NULL,
  description TEXT,
  priority TEXT NOT NULL DEFAULT 'medium',
  status TEXT NOT NULL DEFAULT 'pending',
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
  device_id TEXT,
  repeat_interval INTEGER,
  repeat_unit TEXT,
  enriching_p INTEGER DEFAULT 0
);~%~%")
              (format t "-- Table: tag_presets~%")
              (format t "CREATE TABLE IF NOT EXISTS tag_presets (
  slot INTEGER PRIMARY KEY,
  tags TEXT
);~%~%")
              (with-db (db)
                ;; Dump todos data
                (format t "-- Data: todos~%")
                (let ((rows (sqlite:execute-to-list db "
                  SELECT t.id, t.title,
                         COALESCE(b1.content, t.description) as description,
                         t.priority, t.status,
                         t.scheduled_date, t.due_date, t.tags, t.estimated_minutes,
                         COALESCE(b2.content, t.location_info) as location_info,
                         t.url, t.parent_id, t.created_at, t.completed_at,
                         t.valid_from, t.valid_to, t.device_id, t.repeat_interval,
                         t.repeat_unit, t.enriching_p
                  FROM todos t
                  LEFT JOIN blobs b1 ON t.description_hash = b1.hash
                  LEFT JOIN blobs b2 ON t.location_info_hash = b2.hash
                  WHERE t.valid_to IS NULL
                  ORDER BY t.created_at")))
                  (dolist (row rows)
                    (destructuring-bind (id title description priority status
                                         scheduled-date due-date tags estimated-minutes
                                         location-info url parent-id created-at completed-at
                                         valid-from valid-to device-id repeat-interval repeat-unit enriching-p) row
                      (format t "INSERT INTO todos (id, title, description, priority, status, scheduled_date, due_date, tags, estimated_minutes, location_info, url, parent_id, created_at, completed_at, valid_from, valid_to, device_id, repeat_interval, repeat_unit, enriching_p) VALUES (~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A);~%"
                              (sql-escape-string id)
                              (sql-escape-string title)
                              (sql-escape-string description)
                              (sql-escape-string priority)
                              (sql-escape-string status)
                              (sql-escape-string scheduled-date)
                              (sql-escape-string due-date)
                              (sql-escape-string tags)
                              (if (or (null estimated-minutes) (eq estimated-minutes :null)) "NULL" estimated-minutes)
                              (sql-escape-string location-info)
                              (sql-escape-string url)
                              "NULL"  ; parent-id no longer used
                              (sql-escape-string created-at)
                              (sql-escape-string completed-at)
                              (sql-escape-string valid-from)
                              (sql-escape-string valid-to)
                              (sql-escape-string device-id)
                              (if (or (null repeat-interval) (eq repeat-interval :null)) "NULL" repeat-interval)
                              (sql-escape-string repeat-unit)
                              (if (or (null enriching-p) (eq enriching-p :null)) "0" enriching-p))))
                  (format t "~%-- ~D todos exported~%~%" (length rows)))
                ;; Dump tag_presets data
                (format t "-- Data: tag_presets~%")
                (let ((presets (sqlite:execute-to-list db "SELECT slot, tags FROM tag_presets ORDER BY slot")))
                  (dolist (preset presets)
                    (format t "INSERT INTO tag_presets (slot, tags) VALUES (~A, ~A);~%"
                            (first preset)
                            (sql-escape-string (second preset))))
                  (format t "~%-- ~D presets exported~%~%" (length presets))))
              (format t "COMMIT;~%"))))

(defun make-sync-server-command ()
  "Create the 'sync-server' subcommand for gRPC streaming sync."
  (let ((port-opt (clingon:make-option
                   :integer
                   :short-name #\p
                   :long-name "port"
                   :key :port
                   :initial-value 50051
                   :description "gRPC port to listen on"))
        (bind-opt (clingon:make-option
                   :string
                   :short-name #\b
                   :long-name "bind"
                   :key :bind
                   :initial-value "0.0.0.0"
                   :description "Address to bind to"))
        (no-tls-opt (clingon:make-option
                     :flag
                     :long-name "no-tls"
                     :key :no-tls
                     :description "Disable TLS (for testing)")))
    (clingon:make-command
     :name "sync-server"
     :description "Start gRPC streaming sync server for real-time sync"
     :options (list port-opt bind-opt no-tls-opt)
     :handler (lambda (cmd)
                (let ((port (clingon:getopt cmd :port))
                      (address (clingon:getopt cmd :bind))
                      (no-tls (clingon:getopt cmd :no-tls)))
                  (start-grpc-sync-server :port port :host address :require-tls (not no-tls))
                  ;; Keep running until interrupted
                  (handler-case
                      (loop (sleep 1))
                    (#+sbcl sb-sys:interactive-interrupt
                     #+ccl ccl:interrupt-signal-condition
                     #-(or sbcl ccl) error ()
                     (format t "~%Shutting down gRPC sync server...~%")
                     (stop-grpc-sync-server))))))))

(defun make-sync-connect-command ()
  "Create the 'sync-connect' subcommand to connect TUI to a remote sync server."
  (let ((host-opt (clingon:make-option
                   :string
                   :short-name #\h
                   :long-name "host"
                   :key :host
                   :initial-value "localhost"
                   :description "Sync server host address"))
        (port-opt (clingon:make-option
                   :integer
                   :short-name #\p
                   :long-name "port"
                   :key :port
                   :initial-value 50051
                   :description "Sync server gRPC port"))
        (cert-opt (clingon:make-option
                   :string
                   :long-name "client-cert"
                   :key :client-cert
                   :description "Path to client certificate for mTLS"))
        (key-opt (clingon:make-option
                  :string
                  :long-name "client-key"
                  :key :client-key
                  :description "Path to client private key for mTLS")))
    (clingon:make-command
     :name "sync-connect"
     :description "Start TUI and connect to a remote sync server"
     :options (list host-opt port-opt cert-opt key-opt)
     :handler (lambda (cmd)
                (let ((host (clingon:getopt cmd :host))
                      (port (clingon:getopt cmd :port))
                      (client-cert (clingon:getopt cmd :client-cert))
                      (client-key (clingon:getopt cmd :client-key)))
                  ;; Initialize enrichment
                  (init-enrichment)
                  ;; Run with error handling that logs backtraces
                  (handler-bind ((error (lambda (e)
                                          (log-error-with-backtrace e)
                                          (signal e))))
                    #+sbcl
                    (handler-bind ((warning #'muffle-warning))
                      (let* ((model (make-initial-model)))
                        ;; Start sync client connection
                        (start-sync-client host port :model model
                                           :client-certificate client-cert
                                           :client-key client-key)
                        ;; Start TUI
                        (unwind-protect
                             (let ((program (tui:make-program model :alt-screen t :mouse :cell-motion)))
                               (tui:run program))
                          ;; Cleanup sync client on exit
                          (stop-sync-client))))
                    #-sbcl
                    (let* ((model (make-initial-model)))
                      (start-sync-client host port :model model
                                           :client-certificate client-cert
                                           :client-key client-key)
                      (unwind-protect
                           (let ((program (tui:make-program model :alt-screen t :mouse :cell-motion)))
                             (tui:run program))
                        (stop-sync-client)))))))))

;;── Certificate Management Commands ────────────────────────────────────────────

(defun make-cert-init-command ()
  "Create the 'cert init' subcommand."
  (let ((force-opt (clingon:make-option
                    :flag
                    :short-name #\f
                    :long-name "force"
                    :key :force
                    :description "Force re-initialization (WARNING: invalidates all existing certificates)"))
        (days-opt (clingon:make-option
                   :integer
                   :short-name #\d
                   :long-name "days"
                   :key :days
                   :initial-value 3650
                   :description "CA certificate validity in days (default: 10 years)")))
    (clingon:make-command
     :name "init"
     :description "Initialize the Certificate Authority for mTLS"
     :options (list force-opt days-opt)
     :handler (lambda (cmd)
                (let ((force (clingon:getopt cmd :force))
                      (days (clingon:getopt cmd :days)))
                  (handler-case
                      (progn
                        (init-ca :force force :days days)
                        (init-server-cert :days (min days 365))
                        (format t "~%~A mTLS certificates initialized!~%"
                                (tui:colored "✓" :fg tui:*fg-green*))
                        (format t "~%You can now issue client certificates with:~%")
                        (format t "  cloodoo cert issue DEVICE_NAME~%~%"))
                    (error (e)
                      (format t "~A Error: ~A~%"
                              (tui:colored "✗" :fg tui:*fg-red*) e))))))))

(defun make-cert-issue-command ()
  "Create the 'cert issue' subcommand."
  (let ((days-opt (clingon:make-option
                   :integer
                   :short-name #\d
                   :long-name "days"
                   :key :days
                   :initial-value 365
                   :description "Certificate validity in days"))
        (port-opt (clingon:make-option
                   :integer
                   :short-name #\p
                   :long-name "port"
                   :key :port
                   :initial-value 9876
                   :description "HTTP server port for pairing URL"))
        (no-server-opt (clingon:make-option
                        :flag
                        :long-name "no-server"
                        :key :no-server
                        :description "Don't start pairing server, just create certificate")))
    (clingon:make-command
     :name "issue"
     :description "Issue a client certificate for a device"
     :usage "DEVICE_NAME"
     :options (list days-opt port-opt no-server-opt)
     :handler (lambda (cmd)
                (let ((args (clingon:command-arguments cmd))
                      (days (clingon:getopt cmd :days))
                      (port (clingon:getopt cmd :port))
                      (no-server (clingon:getopt cmd :no-server)))
                  (if args
                      (let ((device-name (first args)))
                        (handler-case
                            (if no-server
                                ;; Just create the cert, no pairing server
                                (let ((passphrase (issue-client-cert device-name :days days)))
                                  (format t "~%~A Certificate created for '~A'~%"
                                          (tui:colored "✓" :fg tui:*fg-green*) device-name)
                                  (format t "~%Cert: ~A~%" (namestring (client-cert-file device-name)))
                                  (format t "Key:  ~A~%" (namestring (client-key-file device-name)))
                                  (format t "Passphrase: ~A~%~%" passphrase))
                                ;; Create cert and start pairing server
                                (let* ((request (create-pairing-request device-name))
                                       (token (pairing-request-token request))
                                       (passphrase (pairing-request-passphrase request))
                                       (ips (detect-local-ips))
                                       (primary-ip
                                         (cond
                                           ;; No IPs found
                                           ((null ips) "localhost")
                                           ;; Only one IP - use it
                                           ((null (cdr ips)) (first ips))
                                           ;; Multiple IPs - let user choose
                                           (t (format t "~%Available network addresses:~%")
                                              (loop for ip in ips
                                                    for i from 1
                                                    do (format t "  ~A) ~A~A~%"
                                                               i ip
                                                               (if (str:starts-with-p "100." ip)
                                                                   (tui:colored " (VPN)" :fg tui:*fg-cyan*)
                                                                   "")))
                                              (format t "~%Select address [1]: ")
                                              (force-output)
                                              (let* ((input (str:trim (read-line)))
                                                     (choice (if (zerop (length input))
                                                                 1
                                                                 (parse-integer input :junk-allowed t))))
                                                (if (and choice (>= choice 1) (<= choice (length ips)))
                                                    (nth (1- choice) ips)
                                                    (first ips))))))
                                       (url (format nil "http://~A:~A/pair/~A" primary-ip port token)))
                                  (format t "~%~A Certificate created for '~A'~%"
                                          (tui:colored "✓" :fg tui:*fg-green*) device-name)
                                  (format t "~%Scan this QR code in the Cloodoo app, or run on the client:~%")
                                  (format t "  ~A~%~%" (tui:colored (format nil "cloodoo cert pair ~A" url) :fg tui:*fg-cyan*))
                                  ;; Try to generate QR code
                                  (let ((qr (generate-qr-ascii url)))
                                    (when qr
                                      (format t "~A~%" qr)))
                                  (format t "~A Link expires in 10 minutes.~%"
                                          (tui:colored "!" :fg tui:*fg-yellow*))
                                  (format t "~%Passphrase: ~A~%~%"
                                          (tui:bold (tui:colored passphrase :fg tui:*fg-green*)))
                                  ;; Start HTTP server for pairing
                                  (start-server :port port :address "0.0.0.0")
                                  (format t "Waiting for device to connect...~%")
                                  (format t "(Press Ctrl+C when done)~%~%")
                                  ;; Wait until interrupted or token consumed
                                  (handler-case
                                      (loop
                                        (sleep 2)
                                        (cleanup-expired-pairings)
                                        ;; Check if token was consumed (cert downloaded)
                                        (unless (get-pairing-request token)
                                          (format t "~%~A Device '~A' has downloaded its certificate.~%"
                                                  (tui:colored "✓" :fg tui:*fg-green*) device-name)
                                          (return)))
                                    (#+sbcl sb-sys:interactive-interrupt
                                     #+ccl ccl:interrupt-signal-condition
                                     #-(or sbcl ccl) error ()
                                     (format t "~%")))
                                  (stop-server)))
                          (usocket:address-in-use-error ()
                            (format t "~A Port ~A is already in use.~%"
                                    (tui:colored "✗" :fg tui:*fg-red*) port)
                            (format t "  Use --port to specify a different port, or stop the existing server.~%"))
                          (error (e)
                            (format t "~A Error: ~A~%"
                                    (tui:colored "✗" :fg tui:*fg-red*) e))))
                      (format t "Usage: cloodoo cert issue DEVICE_NAME~%")))))))

(defun make-cert-list-command ()
  "Create the 'cert list' subcommand."
  (clingon:make-command
   :name "list"
   :description "List all issued client certificates"
   :handler (lambda (cmd)
              (declare (ignore cmd))
              (if (ca-initialized-p)
                  (let ((certs (list-client-certs)))
                    (if certs
                        (progn
                          (format t "~%~A~%~%"
                                  (tui:bold "Issued Certificates"))
                          (format t "  ~30A ~12A ~A~%"
                                  "DEVICE" "STATUS" "ISSUED")
                          (format t "  ~30A ~12A ~A~%"
                                  "------" "------" "------")
                          (dolist (cert certs)
                            (let ((name (getf cert :name))
                                  (revoked (getf cert :revoked-p))
                                  (issued (getf cert :issued-at)))
                              (format t "  ~30A ~12A ~A~%"
                                      name
                                      (if revoked
                                          (tui:colored "REVOKED" :fg tui:*fg-red*)
                                          (tui:colored "active" :fg tui:*fg-green*))
                                      (lt:format-timestring
                                       nil (lt:universal-to-timestamp issued)
                                       :format '(:year "-" (:month 2) "-" (:day 2))))))
                          (format t "~%"))
                        (format t "~%No certificates issued yet.~%~%")))
                  (format t "~%CA not initialized. Run 'cloodoo cert init' first.~%~%")))))

(defun make-cert-revoke-command ()
  "Create the 'cert revoke' subcommand."
  (clingon:make-command
   :name "revoke"
   :description "Revoke a client certificate"
   :usage "DEVICE_NAME"
   :handler (lambda (cmd)
              (let ((args (clingon:command-arguments cmd)))
                (if args
                    (let ((device-name (first args)))
                      (handler-case
                          (progn
                            (revoke-client-cert device-name)
                            (format t "~%~A Certificate for '~A' has been revoked.~%~%"
                                    (tui:colored "✓" :fg tui:*fg-green*) device-name))
                        (error (e)
                          (format t "~A Error: ~A~%"
                                  (tui:colored "✗" :fg tui:*fg-red*) e))))
                    (format t "Usage: cloodoo cert revoke DEVICE_NAME~%"))))))

(defun make-cert-pair-command ()
  "Create the 'cert pair' subcommand."
  (clingon:make-command
   :name "pair"
   :description "Pair with a remote server to receive a client certificate"
   :usage "URL"
   :handler (lambda (cmd)
              (let ((args (clingon:command-arguments cmd)))
                (if args
                    (let ((url (first args)))
                      (handler-case
                          (pair-with-server url)
                        (error (e)
                          (format t "~A Error: ~A~%"
                                  (tui:colored "✗" :fg tui:*fg-red*) e))))
                    (progn
                      (format t "Usage: cloodoo cert pair URL~%~%")
                      (format t "Example: cloodoo cert pair http://192.168.1.100:9876/pair/abc123~%")))))))

(defun sanitize-device-id (device-id)
  "Sanitize a device ID to prevent path traversal attacks.
   Removes path separators and parent directory references."
  (unless device-id
    (error "Server did not return a device_id"))
  (let ((sanitized (str:replace-all "/" "" (str:replace-all "\\" "" device-id))))
    ;; Also reject if it contains .. or is empty after sanitization
    (when (or (zerop (length sanitized))
              (search ".." sanitized))
      (error "Invalid device_id received from server: ~A" device-id))
    sanitized))

(defun http-get (url)
  "GET a URL, returning (values body status-code) without signaling on HTTP errors."
  (handler-bind ((dex:http-request-failed
                   (lambda (e)
                     (return-from http-get
                       (values (dex:response-body e) (dex:response-status e))))))
    (dex:get url)))

(defun http-post (url &key content content-type)
  "POST to a URL, returning (values body status-code) without signaling on HTTP errors."
  (handler-bind ((dex:http-request-failed
                   (lambda (e)
                     (return-from http-post
                       (values (dex:response-body e) (dex:response-status e))))))
    (apply #'dex:post url
           (append
            (when content (list :content content))
            (when content-type (list :headers `(("content-type" . ,content-type))))))))

(defun parse-pairing-url (url)
  "Parse a pairing URL into (values host port token).
URL format: http://HOST:PORT/pair/TOKEN"
  (let* ((without-scheme (if (str:starts-with-p "http://" url)
                             (subseq url 7)
                             (if (str:starts-with-p "https://" url)
                                 (subseq url 8)
                                 url)))
         (path-start (position #\/ without-scheme))
         (host-port (subseq without-scheme 0 path-start))
         (path (when path-start (subseq without-scheme path-start)))
         (colon-pos (position #\: host-port))
         (host (if colon-pos (subseq host-port 0 colon-pos) host-port))
         (port (if colon-pos
                   (parse-integer (subseq host-port (1+ colon-pos)))
                   9876))
         (token (when (and path (str:starts-with-p "/pair/" path))
                  (subseq path 6))))
    (unless token
      (error "Invalid pairing URL. Expected format: http://HOST:PORT/pair/TOKEN"))
    (values host port token)))

(defun pair-with-server (url)
  "Execute the pairing flow: fetch info, download PEM cert/key, store locally."
  (multiple-value-bind (host port token) (parse-pairing-url url)
    (let ((base-url (format nil "http://~A:~A" host port)))

      ;; Step 1: Get pairing info
      (format t "~%Connecting to ~A...~%" base-url)
      (multiple-value-bind (body status)
          (http-get (format nil "~A/pair/~A" base-url token))
        (unless (= status 200)
          (error "Pairing token not found or expired (HTTP ~A)" status))
        (let* ((info (jzon:parse body))
               (device-name (gethash "device_name" info))
               (expires-in (gethash "expires_in" info)))
          (format t "~A Server is offering certificate for '~A'~%"
                  (tui:colored "✓" :fg tui:*fg-green*) device-name)
          (format t "  Expires in ~A seconds~%~%" expires-in)

          ;; Step 2: Get server device ID for storage path
          (multiple-value-bind (dev-body dev-status)
              (http-get (format nil "~A/api/pair/status" base-url))
            (unless (= dev-status 200)
              (error "Failed to get server device info (HTTP ~A)" dev-status))
            (let* ((dev-info (jzon:parse dev-body))
                   (server-device-id (sanitize-device-id (gethash "device_id" dev-info))))

              ;; Step 3: Prompt for passphrase
              (format t "Enter passphrase: ")
              (force-output)
              (let ((passphrase (str:trim (read-line))))
                (when (zerop (length passphrase))
                  (error "Passphrase cannot be empty"))

                ;; Step 4: Download cert and key as PEM
                (format t "Downloading certificate...~%")
                (multiple-value-bind (pem-body pem-status)
                    (http-post (format nil "~A/pair/~A/pem" base-url token)
                               :content (jzon:stringify
                                         (alexandria:plist-hash-table
                                          (list "passphrase" passphrase)
                                          :test #'equal))
                               :content-type "application/json")
                  (unless (= pem-status 200)
                    (error "Failed to download certificate (HTTP ~A). Token may have expired."
                           pem-status))

                  (let* ((pem-data (jzon:parse pem-body))
                         (cert-pem (gethash "cert" pem-data))
                         (key-pem (gethash "key" pem-data))
                         (cert-path (paired-client-cert-file server-device-id))
                         (key-path (paired-client-key-file server-device-id)))

                    ;; Save cert
                    (ensure-directories-exist cert-path)
                    (with-open-file (stream cert-path :direction :output
                                                      :if-exists :supersede)
                      (write-string cert-pem stream))

                    ;; Save key with restricted permissions
                    (with-open-file (stream key-path :direction :output
                                                     :if-exists :supersede)
                      (write-string key-pem stream))
                    (uiop:run-program (list "chmod" "600" (namestring key-path))
                                      :ignore-error-status t)

                    ;; Save sync config for auto-connect
                    (let ((sync-config-path (paired-sync-config-file server-device-id)))
                      (with-open-file (stream sync-config-path :direction :output
                                                               :if-exists :supersede
                                                               :if-does-not-exist :create)
                        (let ((config (make-hash-table :test #'equal)))
                          (setf (gethash "host" config) host)
                          (setf (gethash "port" config) 50051)
                          (jzon:stringify config :stream stream))))

                    ;; Success
                    (format t "~%~A Pairing complete!~%"
                            (tui:colored "✓" :fg tui:*fg-green*))
                    (format t "~%Certificate stored in:~%")
                    (format t "  ~A~%" (namestring cert-path))
                    (format t "  ~A~%" (namestring key-path))
                    (format t "~%Cloodoo will auto-connect to this server on next launch.~%~%")))))))))))

(defun make-cert-command ()
  "Create the 'cert' command group."
  (clingon:make-command
   :name "cert"
   :description "Manage mTLS certificates for secure sync"
   :usage "[command]"
   :handler (lambda (cmd)
              ;; Default: show help
              (clingon:print-usage cmd t))
   :sub-commands (list (make-cert-init-command)
                       (make-cert-issue-command)
                       (make-cert-pair-command)
                       (make-cert-list-command)
                       (make-cert-revoke-command))))

;;── Enrichment Command ─────────────────────────────────────────────────────────

(defun make-enrich-pending-command ()
  "Create the 'enrich-pending' subcommand for background enrichment."
  (clingon:make-command
   :name "enrich-pending"
   :description "Enrich TODOs that are pending enrichment (internal use)"
   :handler (lambda (cmd)
              (declare (ignore cmd))
              (let* ((todos (load-todos))
                     (pending (remove-if-not #'todo-enriching-p todos)))
                (when pending
                  (init-enrichment)
                  (dolist (todo pending)
                    (handler-case
                        (let* ((enriched (enrich-todo-input (todo-title todo)
                                                            (todo-description todo)
                                                            nil)))
                          (when enriched
                            (when (getf enriched :title)
                              (setf (todo-title todo) (getf enriched :title)))
                            (when (getf enriched :description)
                              (setf (todo-description todo) (getf enriched :description)))
                            (when (getf enriched :priority)
                              (setf (todo-priority todo) (getf enriched :priority)))
                            (when (getf enriched :category)
                              (let ((tag (category-to-tag (getf enriched :category))))
                                (when tag
                                  (setf (todo-tags todo) (list tag)))))
                            (when (getf enriched :estimated-minutes)
                              (setf (todo-estimated-minutes todo) (getf enriched :estimated-minutes)))
                            (when (getf enriched :scheduled-date)
                              (setf (todo-scheduled-date todo) (getf enriched :scheduled-date)))
                            (when (getf enriched :due-date)
                              (setf (todo-due-date todo) (getf enriched :due-date)))
                            (when (getf enriched :location-info)
                              (setf (todo-location-info todo) (getf enriched :location-info)))))
                      (error (e)
                        (declare (ignore e))))
                    ;; Clear enriching flag regardless of success/failure
                    (setf (todo-enriching-p todo) nil)
                    (save-todo todo)))))))

;;── Native Messaging for Browser Extension ────────────────────────────────────

(defun native-host-log (format-string &rest args)
  "Write debug log for native messaging host."
  (let ((log-file (merge-pathnames "native-host.log" (cache-directory))))
    (ensure-directories-exist log-file)
    (with-open-file (stream log-file
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
      (format stream "[~A] " (lt:format-timestring nil (lt:now) :format '(:hour ":" :min ":" :sec)))
      (apply #'format stream format-string args)
      (terpri stream))))

(defun read-native-message (input-stream)
  "Read a native messaging message from INPUT-STREAM.
   Format: 4-byte little-endian length + JSON payload.
   Times out after 5 seconds."
  (handler-case
      (sb-sys:with-deadline (:seconds 5)
        ;; Read 4-byte length prefix
        (let ((b0 (read-byte input-stream nil nil))
              (b1 (read-byte input-stream nil nil))
              (b2 (read-byte input-stream nil nil))
              (b3 (read-byte input-stream nil nil)))
          (when (and b0 b1 b2 b3)
            (let ((length (+ b0 (ash b1 8) (ash b2 16) (ash b3 24))))
              ;; Safety check: Chrome limits messages to 1MB
              (when (and (> length 0) (< length 1048576))
                (let ((json-bytes (make-array length :element-type '(unsigned-byte 8))))
                  (read-sequence json-bytes input-stream)
                  ;; Convert bytes to string and parse JSON
                  (let ((json-string (map 'string #'code-char json-bytes)))
                    (jzon:parse json-string))))))))
    (sb-sys:deadline-timeout ()
      (native-host-log "Timeout waiting for message")
      nil)))

(defun write-native-message (data output-stream)
  "Write a native messaging response to OUTPUT-STREAM.
   Format: 4-byte little-endian length + JSON payload."
  (let* ((json-string (jzon:stringify data))
         (length (length json-string)))
    ;; Write 4-byte length prefix (little-endian)
    (write-byte (logand length #xff) output-stream)
    (write-byte (logand (ash length -8) #xff) output-stream)
    (write-byte (logand (ash length -16) #xff) output-stream)
    (write-byte (logand (ash length -24) #xff) output-stream)
    ;; Write JSON payload as bytes
    (loop for char across json-string
          do (write-byte (char-code char) output-stream))
    (force-output output-stream)))

(defun db-todo-exists-p (id)
  "Check if a current (non-superseded) TODO with the given ID exists."
  (with-db (db)
    (let ((result (sqlite:execute-single db
                    "SELECT 1 FROM todos WHERE id = ? AND valid_to IS NULL" id)))
      (not (null result)))))

(defun sync-push-todo (todo)
  "Push a single todo to the sync server as a one-shot gRPC operation.
   Connects, performs the SyncStream handshake, sends the upsert, and disconnects.
   Used by the native host to immediately sync browser extension TODOs.
   Returns T on success, NIL on failure.  Errors are logged but never propagated."
  (handler-case
      (multiple-value-bind (host port server-id) (find-paired-sync-config)
        (unless host
          (native-host-log "No sync config found, skipping sync push")
          (return-from sync-push-todo nil))
        (native-host-log "Sync push to ~A:~A" host port)
        (let* ((cert-file (paired-client-cert-file server-id))
               (key-file (paired-client-key-file server-id))
               (use-tls (and (probe-file cert-file) (probe-file key-file)))
               (channel nil)
               (stream nil))
          ;; Disable ENABLE_PUSH (deprecated in RFC 9113, gRPC doesn't use server push)
          (let ((push-setting (assoc ag-http2::+settings-enable-push+
                                     ag-http2::*default-settings*)))
            (when push-setting
              (setf (cdr push-setting) 0)))
          (unwind-protect
               (progn
                 ;; Connect to sync server
                 (setf channel
                       (if use-tls
                           (ag-grpc:make-channel host port
                                                 :tls t
                                                 :tls-client-certificate (namestring cert-file)
                                                 :tls-client-key (namestring key-file))
                           (ag-grpc:make-channel host port :tls nil)))
                 (let ((stub (make-todo-sync-stub channel)))
                   (setf stream (todo-sync-sync-stream stub)))
                 ;; Send SyncInit with since=now to minimize pending changes
                 (let ((now (now-iso)))
                   (ag-grpc:stream-send stream
                     (make-sync-init-message (get-device-id) now now)))
                 ;; Read SyncAck
                 (let ((ack-msg (ag-grpc:stream-read-message stream)))
                   (unless (and ack-msg (eq (proto-msg-case ack-msg) :ack))
                     (native-host-log "Sync push: no ACK received")
                     (return-from sync-push-todo nil))
                   (let* ((ack (proto-msg-ack ack-msg))
                          (error-msg (proto-ack-error ack))
                          (pending (proto-ack-pending-changes ack)))
                     (when (and error-msg (plusp (length error-msg)))
                       (native-host-log "Sync push rejected: ~A" error-msg)
                       (return-from sync-push-todo nil))
                     ;; Drain any pending changes the server sends
                     (dotimes (i pending)
                       (ag-grpc:stream-read-message stream))))
                 ;; Send the upsert
                 (ag-grpc:stream-send stream
                   (make-sync-upsert-message (get-device-id) todo))
                 (native-host-log "Sync push: sent todo ~A" (todo-id todo))
                 t)
            ;; Cleanup
            (when stream
              (handler-case (ag-grpc:stream-close-send stream) (error () nil)))
            (when channel
              (handler-case (ag-grpc:channel-close channel) (error () nil))))))
    (error (e)
      (native-host-log "Sync push failed (non-fatal): ~A" e)
      nil)))

(defun handle-native-create-todo (message)
  "Handle a createTodo message from the browser extension.
   Creates the TODO immediately and returns, then spawns background enrichment.
   If client_id is provided and a todo with that ID already exists, returns
   success without creating a duplicate (idempotent)."
  (let* ((todo-data (gethash "todo" message))
         (title (gethash "title" todo-data))
         (client-id (let ((cid (gethash "client_id" todo-data)))
                      (when (and cid (stringp cid) (plusp (length cid))) cid)))
         (description (let ((d (gethash "description" todo-data)))
                        (unless (or (null d) (eq d 'null)) d)))
         (priority-str (or (gethash "priority" todo-data) "medium"))
         (priority (intern (string-upcase priority-str) :keyword))
         (due-date-str (gethash "due_date" todo-data))
         (due-date (when (and due-date-str (not (eq due-date-str 'null)) (stringp due-date-str))
                     (handler-case (lt:parse-timestring due-date-str)
                       (error () nil))))
         (tags-raw (gethash "tags" todo-data))
         (tags (when (and tags-raw (not (eq tags-raw 'null)))
                 (parse-tags (if (stringp tags-raw)
                                 tags-raw
                                 (coerce tags-raw 'list)))))
         (url (let ((u (gethash "url" todo-data)))
                (unless (or (null u) (eq u 'null)) u))))
    ;; Idempotency: if client_id provided and todo already exists, return success
    (when (and client-id (db-todo-exists-p client-id))
      (native-host-log "TODO already exists for client_id ~A, skipping" client-id)
      (let ((response (make-hash-table :test #'equal)))
        (setf (gethash "success" response) t)
        (setf (gethash "duplicate" response) t)
        (return-from handle-native-create-todo response)))
    (if title
        ;; Create TODO immediately with raw data, mark for async enrichment
        (let ((todo (make-todo title
                               :description description
                               :priority priority
                               :due-date due-date
                               :tags tags
                               :url url)))
          ;; Use client-provided ID for idempotency instead of the generated one
          (when client-id
            (setf (todo-id todo) client-id))
          ;; Mark as needing enrichment
          (setf (todo-enriching-p todo) t)
          (save-todo todo)
          (native-host-log "Created TODO: ~A (pending enrichment)" title)
          ;; Push to sync server immediately (best-effort, non-fatal)
          (sync-push-todo todo)
          ;; Spawn background enrichment process (best-effort, must not
          ;; prevent the success response from being sent — otherwise the
          ;; extension thinks creation failed and retries every 5 minutes,
          ;; creating duplicates)
          (handler-case
              (let ((exe-path (get-cloodoo-executable)))
                (native-host-log "Spawning enrichment: ~A enrich-pending" exe-path)
                (uiop:launch-program (list exe-path "enrich-pending")
                                     :output nil
                                     :error-output nil))
            (error (e)
              (native-host-log "Enrichment spawn failed (non-fatal): ~A" e)))
          ;; Return success immediately
          (let ((response (make-hash-table :test #'equal)))
            (setf (gethash "success" response) t)
            (setf (gethash "enriching" response) t)
            (setf (gethash "todo" response) (todo-to-hash-table todo))
            response))
        ;; Return error if no title
        (let ((response (make-hash-table :test #'equal)))
          (setf (gethash "success" response) nil)
          (setf (gethash "error" response) "Missing title")
          response))))

(defun dom-samples-directory ()
  "Return the directory for storing DOM samples for analysis."
  (ensure-cache-directory)
  (merge-pathnames "dom-samples/" (cache-directory)))

(defun handle-native-record-dom (message)
  "Handle a recordDom message from the browser extension.
   Saves the DOM/HTML content for later analysis to improve extraction."
  (let* ((url (gethash "url" message))
         (html (gethash "html" message))
         (site (gethash "site" message))  ; e.g., 'gmail', 'outlook'
         (extraction-result (gethash "extractionResult" message)))
    (if (and url html)
        (handler-case
            (let* ((samples-dir (dom-samples-directory))
                   (timestamp (lt:format-timestring nil (lt:now)
                                :format '(:year :month :day "-" :hour :min :sec)))
                   (safe-site (or site "unknown"))
                   (filename (format nil "~A-~A.html" safe-site timestamp))
                   (filepath (merge-pathnames filename samples-dir))
                   (meta-filepath (merge-pathnames (format nil "~A-~A.json" safe-site timestamp)
                                                   samples-dir)))
              ;; Ensure directory exists
              (ensure-directories-exist filepath)
              ;; Write HTML content
              (with-open-file (stream filepath
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create
                                      :external-format :utf-8)
                (write-string html stream))
              ;; Write metadata
              (with-open-file (stream meta-filepath
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create
                                      :external-format :utf-8)
                (let ((meta (make-hash-table :test #'equal)))
                  (setf (gethash "url" meta) url)
                  (setf (gethash "site" meta) safe-site)
                  (setf (gethash "timestamp" meta) timestamp)
                  (setf (gethash "extractionResult" meta) extraction-result)
                  (write-string (jzon:stringify meta) stream)))
              (native-host-log "Saved DOM sample: ~A" filename)
              (let ((response (make-hash-table :test #'equal)))
                (setf (gethash "success" response) t)
                (setf (gethash "filename" response) filename)
                response))
          (error (e)
            (native-host-log "Error saving DOM: ~A" e)
            (let ((response (make-hash-table :test #'equal)))
              (setf (gethash "success" response) nil)
              (setf (gethash "error" response) (format nil "~A" e))
              response)))
        ;; Missing required fields
        (let ((response (make-hash-table :test #'equal)))
          (setf (gethash "success" response) nil)
          (setf (gethash "error" response) "Missing url or html")
          response))))

(defun handle-native-get-tags (message)
  "Handle a getTags message from the browser extension.
   Returns a list of all unique tags used across todos."
  (declare (ignore message))
  (handler-case
      (let* ((todos (load-todos))
             (all-tags (make-hash-table :test #'equal)))
        ;; Collect unique tags from all todos
        (dolist (todo todos)
          (dolist (tag (todo-tags todo))
            (when (and tag (stringp tag))
              (setf (gethash tag all-tags) t))))
        ;; Convert to sorted list
        (let ((tags-list (sort (loop for tag being the hash-keys of all-tags
                                     collect tag)
                               #'string<)))
          (let ((response (make-hash-table :test #'equal)))
            (setf (gethash "success" response) t)
            (setf (gethash "tags" response) (coerce tags-list 'vector))
            response)))
    (error (e)
      (native-host-log "Error getting tags: ~A" e)
      (let ((response (make-hash-table :test #'equal)))
        (setf (gethash "success" response) nil)
        (setf (gethash "error" response) (format nil "~A" e))
        response))))

(defun make-native-host-command ()
  "Create the 'native-host' subcommand for browser extension native messaging."
  (clingon:make-command
   :name "native-host"
   :description "Native messaging host for browser extension (internal use)"
   :handler (lambda (cmd)
              (declare (ignore cmd))
              (native-host-log "Native host started")
              ;; Use SBCL's file descriptor streams for binary I/O
              (let ((input-stream (sb-sys:make-fd-stream 0
                                        :input t
                                        :element-type '(unsigned-byte 8)
                                        :buffering :full
                                        :name "stdin-binary"))
                    (output-stream (sb-sys:make-fd-stream 1
                                         :output t
                                         :element-type '(unsigned-byte 8)
                                         :buffering :none
                                         :name "stdout-binary")))
                (unwind-protect
                    (handler-case
                        (progn
                          (native-host-log "Reading message...")
                          (let ((message (read-native-message input-stream)))
                            (native-host-log "Message received: ~A" (if message "yes" "nil"))
                            (when message
                              (let* ((action (gethash "action" message))
                                     (_ (native-host-log "Action: ~A" action))
                                     (response
                                       (cond
                                         ((string= action "createTodo")
                                          (native-host-log "Creating TODO...")
                                          (handle-native-create-todo message))
                                         ((string= action "ping")
                                          (native-host-log "Ping received")
                                          (let ((r (make-hash-table :test #'equal)))
                                            (setf (gethash "success" r) t)
                                            (setf (gethash "pong" r) t)
                                            r))
                                         ((string= action "recordDom")
                                          (native-host-log "Recording DOM sample...")
                                          (handle-native-record-dom message))
                                         ((string= action "getTags")
                                          (native-host-log "Getting tags...")
                                          (handle-native-get-tags message))
                                         (t
                                          (native-host-log "Unknown action: ~A" action)
                                          (let ((r (make-hash-table :test #'equal)))
                                            (setf (gethash "success" r) nil)
                                            (setf (gethash "error" r) "Unknown action")
                                            r)))))
                                (declare (ignore _))
                                (native-host-log "Sending response...")
                                (write-native-message response output-stream)
                                (native-host-log "Response sent")))))
                      (error (e)
                        (native-host-log "ERROR: ~A" e)
                        (let ((r (make-hash-table :test #'equal)))
                          (setf (gethash "success" r) nil)
                          (setf (gethash "error" r) (format nil "~A" e))
                          (write-native-message r output-stream))))
                  ;; Cleanup
                  (native-host-log "Cleaning up and exiting")
                  (when input-stream (close input-stream))
                  (when output-stream (close output-stream))))
              ;; Explicitly exit to prevent clingon from doing anything else
              (uiop:quit 0))))

;;── Install Native Messaging Host ─────────────────────────────────────────────

(defun get-cloodoo-executable ()
  "Get the path to the cloodoo executable."
  (or
   ;; Best option: the currently running executable (works when compiled)
   (let ((self (namestring sb-ext:*runtime-pathname*)))
     (when (and self (probe-file self)) self))
   ;; Check if we're running from a known location
   (let ((exe (merge-pathnames "cloodoo" (uiop:getcwd))))
     (when (probe-file exe) (namestring exe)))
   ;; Check the git/cloodoo build directory
   (let ((exe (merge-pathnames "git/cloodoo/cloodoo" (user-homedir-pathname))))
     (when (probe-file exe) (namestring exe)))
   ;; Check common install locations
   (let ((exe "/usr/local/bin/cloodoo"))
     (when (probe-file exe) exe))
   (let ((exe (merge-pathnames "bin/cloodoo" (user-homedir-pathname))))
     (when (probe-file exe) (namestring exe)))
   ;; Fall back to just "cloodoo" and hope it's in PATH
   "cloodoo"))

(defun native-messaging-manifest (extension-id)
  "Generate the native messaging host manifest JSON."
  (let ((manifest (make-hash-table :test #'equal))
        (exe-path (get-cloodoo-executable)))
    (setf (gethash "name" manifest) "com.cloodoo.native")
    (setf (gethash "description" manifest) "Cloodoo TODO manager native messaging host")
    (setf (gethash "path" manifest) exe-path)
    (setf (gethash "type" manifest) "stdio")
    (setf (gethash "allowed_origins" manifest)
          (if extension-id
              (vector (format nil "chrome-extension://~A/" extension-id))
              ;; Allow any extension during development
              (vector "chrome-extension://*/")))
    manifest))

(defun get-native-host-dirs ()
  "Get native messaging host directories for all supported browsers.
   Returns a list of (browser-name directory-path manifest-type) triples.
   manifest-type is :chrome or :firefox (they use different JSON keys)."
  (let ((os (uiop:operating-system))
        (home (user-homedir-pathname))
        (dirs nil))
    (cond
      ;; Linux
      ((or (search "linux" (string-downcase os))
           (eq os :linux))
       (let ((chrome-dir (merge-pathnames ".config/google-chrome/NativeMessagingHosts/" home))
             (chrome-beta-dir (merge-pathnames ".config/google-chrome-beta/NativeMessagingHosts/" home))
             (chrome-unstable-dir (merge-pathnames ".config/google-chrome-unstable/NativeMessagingHosts/" home))
             (chromium-dir (merge-pathnames ".config/chromium/NativeMessagingHosts/" home))
             (firefox-dir (merge-pathnames ".mozilla/native-messaging-hosts/" home)))
         (when (probe-file (merge-pathnames ".config/google-chrome/" home))
           (push (list "Google Chrome" chrome-dir :chrome) dirs))
         (when (probe-file (merge-pathnames ".config/google-chrome-beta/" home))
           (push (list "Google Chrome Beta" chrome-beta-dir :chrome) dirs))
         (when (probe-file (merge-pathnames ".config/google-chrome-unstable/" home))
           (push (list "Google Chrome Unstable" chrome-unstable-dir :chrome) dirs))
         (when (probe-file (merge-pathnames ".config/chromium/" home))
           (push (list "Chromium" chromium-dir :chrome) dirs))
         (when (probe-file (merge-pathnames ".mozilla/" home))
           (push (list "Firefox" firefox-dir :firefox) dirs))
         ;; If no browsers detected, default to Chromium
         (unless dirs
           (push (list "Chromium" chromium-dir :chrome) dirs))))
      ;; macOS
      ((or (search "darwin" (string-downcase os))
           (eq os :darwin)
           (search "macos" (string-downcase os)))
       (push (list "Google Chrome"
                   (merge-pathnames "Library/Application Support/Google/Chrome/NativeMessagingHosts/" home)
                   :chrome)
             dirs)
       (push (list "Google Chrome Beta"
                   (merge-pathnames "Library/Application Support/Google/Chrome Beta/NativeMessagingHosts/" home)
                   :chrome)
             dirs)
       (push (list "Google Chrome Dev"
                   (merge-pathnames "Library/Application Support/Google/Chrome Dev/NativeMessagingHosts/" home)
                   :chrome)
             dirs)
       (push (list "Chromium"
                   (merge-pathnames "Library/Application Support/Chromium/NativeMessagingHosts/" home)
                   :chrome)
             dirs)
       (push (list "Firefox"
                   (merge-pathnames "Library/Application Support/Mozilla/NativeMessagingHosts/" home)
                   :firefox)
             dirs))
      ;; Windows
      ((or (search "windows" (string-downcase os))
           (search "win" (string-downcase os))
           (eq os :windows))
       (push (list "Chrome/Chromium"
                   (merge-pathnames "AppData/Local/Cloodoo/" home)
                   :chrome)
             dirs))
      ;; Default
      (t
       (push (list "Chromium"
                   (merge-pathnames ".config/chromium/NativeMessagingHosts/" home)
                   :chrome)
             dirs)))
    (nreverse dirs)))

(defun create-native-host-wrapper (wrapper-path cloodoo-path)
  "Create a wrapper script that invokes 'cloodoo native-host'."
  (let ((os (uiop:operating-system)))
    (cond
      ;; Windows batch file
      ((or (search "windows" (string-downcase os))
           (search "win" (string-downcase os)))
       (with-open-file (stream wrapper-path
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
         (format stream "@echo off~%")
         (format stream "\"~A\" native-host~%" cloodoo-path)))
      ;; Unix shell script
      (t
       (with-open-file (stream wrapper-path
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
         (format stream "#!/bin/sh~%")
         (format stream "exec \"~A\" native-host~%" cloodoo-path))
       ;; Make executable
       (uiop:run-program (list "chmod" "+x" (namestring wrapper-path))
                        :ignore-error-status t)))))

(defun make-install-native-host-command ()
  "Create the 'setup-extension' subcommand."
  (let ((extension-id-opt (clingon:make-option
                           :string
                           :short-name #\e
                           :long-name "extension-id"
                           :key :extension-id
                           :description "Chrome extension ID (find at chrome://extensions with Developer mode on)"
                           :required t)))
    (clingon:make-command
     :name "setup-extension"
     :aliases '("install-native-host")
     :description "Set up the browser extension for Chrome, Chromium, and Firefox"
     :options (list extension-id-opt)
     :handler (lambda (cmd)
                (let* ((extension-id (clingon:getopt cmd :extension-id))
                       (browser-dirs (get-native-host-dirs))
                       (cloodoo-exe (get-cloodoo-executable))
                       (is-windows (search "win" (string-downcase (uiop:operating-system))))
                       (wrapper-name (if is-windows
                                         "cloodoo-native-host.bat"
                                         "cloodoo-native-host"))
                       (installed-count 0))
                  (dolist (entry browser-dirs)
                    (destructuring-bind (browser-name host-dir manifest-type) entry
                      (let ((wrapper-path (merge-pathnames wrapper-name host-dir))
                            (manifest-file (merge-pathnames "com.cloodoo.native.json" host-dir)))
                        ;; Ensure directory exists
                        (ensure-directories-exist host-dir)
                        ;; Create wrapper script
                        (create-native-host-wrapper wrapper-path cloodoo-exe)
                        ;; Create manifest with correct format for this browser
                        (let ((manifest (make-hash-table :test #'equal)))
                          (setf (gethash "name" manifest) "com.cloodoo.native")
                          (setf (gethash "description" manifest) "Cloodoo TODO manager native messaging host")
                          (setf (gethash "path" manifest) (namestring wrapper-path))
                          (setf (gethash "type" manifest) "stdio")
                          (case manifest-type
                            (:chrome
                             (setf (gethash "allowed_origins" manifest)
                                   (vector (format nil "chrome-extension://~A/" extension-id))))
                            (:firefox
                             (setf (gethash "allowed_extensions" manifest)
                                   (vector "cloodoo@moxielogic.com"))))
                          ;; Write manifest
                          (with-open-file (stream manifest-file
                                                  :direction :output
                                                  :if-exists :supersede
                                                  :if-does-not-exist :create)
                            (jzon:stringify manifest :stream stream :pretty t)))
                        (format t "~A ~A~%" (tui:colored "✓" :fg tui:*fg-green*) browser-name)
                        (format t "  Manifest: ~A~%" manifest-file)
                        (incf installed-count))))
                  (format t "~%Wrapper script: ~A~%" (merge-pathnames wrapper-name
                                                        (second (first browser-dirs))))
                  (format t "Cloodoo path:   ~A~%" cloodoo-exe)
                  (format t "Extension ID:   ~A~%" extension-id)
                  (format t "~%Installed for ~D browser~:P.~%" installed-count)
                  ;; Windows-specific instructions
                  (when is-windows
                    (format t "~%~A Windows Registry Setup Required:~%"
                           (tui:colored "!" :fg tui:*fg-yellow*))
                    (format t "Run this in an elevated Command Prompt:~%~%")
                    (format t "  REG ADD \"HKCU\\Software\\Google\\Chrome\\NativeMessagingHosts\\com.cloodoo.native\" /ve /t REG_SZ /d \"~A\" /f~%~%"
                           (namestring (merge-pathnames "com.cloodoo.native.json"
                                                        (second (first browser-dirs))))))
                  (format t "~%Restart your browser for changes to take effect.~%"))))))

(defun make-app ()
  "Create and return the command-line application."
  (clingon:make-command
   :name "cloodoo"
   :version +version+
   :description "Personal TODO system with a modern TUI"
   :authors (list "Anthony Green <green@moxielogic.com>")
   :license "MIT"
   :usage "[command]"
   :handler (lambda (cmd)
              (declare (ignore cmd))
              ;; Default: launch TUI
              (start-tui))
   :sub-commands (list (make-add-command)
                       (make-list-command)
                       (make-done-command)
                       (make-stats-command)
                       (make-dump-command)
                       (make-compact-command)
                       (make-sync-server-command)
                       (make-sync-connect-command)
                       (make-cert-command)
                       (make-enrich-pending-command)
                       (make-native-host-command)
                       (make-install-native-host-command))))
