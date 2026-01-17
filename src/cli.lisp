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
         (lt:today))

        ;; Tomorrow
        ((or (string= lower "tomorrow") (string= lower "tom"))
         (lt:timestamp+ (lt:today) 1 :day))

        ;; Next week
        ((or (string= lower "next week") (string= lower "nextweek"))
         (lt:timestamp+ (lt:today) 7 :day))

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
  (let* ((today (lt:today))
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
                   :description "Optional description/notes")))
    (clingon:make-command
     :name "add"
     :description "Add a new TODO"
     :usage "TITLE"
     :options (list priority-opt due-opt tags-opt desc-opt)
     :handler (lambda (cmd)
                (let ((args (clingon:command-arguments cmd))
                      (priority (clingon:getopt cmd :priority))
                      (due (clingon:getopt cmd :due))
                      (tags (clingon:getopt cmd :tags))
                      (note (clingon:getopt cmd :note)))
                  (if args
                      (let* ((title (format nil "~{~A~^ ~}" args))
                             (due-date (when due (parse-due-date due)))
                             (todo (make-todo title
                                             :priority (intern (string-upcase priority) :keyword)
                                             :due-date due-date
                                             :description note
                                             :tags tags))
                             (todos (load-todos)))
                        (push todo todos)
                        (save-todos todos)
                        (format t "~A Added: ~A~%"
                               (tui:colored "✓" :fg tui:*fg-green*)
                               title)
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
                           (save-todos todos)
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

(defun make-server-command ()
  "Create the 'server' subcommand."
  (let ((port-opt (clingon:make-option
                   :integer
                   :short-name #\p
                   :long-name "port"
                   :key :port
                   :initial-value 9876
                   :description "Port to listen on")))
    (clingon:make-command
     :name "server"
     :description "Start the API server for browser extension"
     :options (list port-opt)
     :handler (lambda (cmd)
                (let ((port (clingon:getopt cmd :port)))
                  (start-server :port port)
                  ;; Keep running until interrupted
                  (handler-case
                      (loop (sleep 1))
                    (#+sbcl sb-sys:interactive-interrupt
                     #+ccl ccl:interrupt-signal-condition
                     #-(or sbcl ccl) error ()
                     (format t "~%Shutting down...~%")
                     (stop-server))))))))

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
                       (make-server-command))))
