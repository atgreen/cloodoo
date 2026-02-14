;;; export.lisp - Export TODO data to various formats (org-agenda style, PDF)
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cloodoo)

;;── Date Utilities for Export ────────────────────────────────────────────────

(defun format-week-range (timestamp)
  "Format week number range for the given timestamp (e.g., 'W03-W04')."
  (let* ((year (lt:timestamp-year timestamp))
         (month (lt:timestamp-month timestamp))
         (day (lt:timestamp-day timestamp))
         ;; Calculate ISO week number
         (day-of-year (+ day
                        (loop for m from 1 below month
                              sum (case m
                                    ((1 3 5 7 8 10 12) 31)
                                    ((4 6 9 11) 30)
                                    (2 (if (zerop (mod year 4)) 29 28))))))
         (week-num (ceiling day-of-year 7)))
    (format nil "W~2,'0D-W~2,'0D" week-num (1+ week-num))))

(defun days-until (timestamp)
  "Return number of days from today until TIMESTAMP. Negative if overdue."
  (let* ((now (local-today))
         (target (lt:timestamp-minimize-part timestamp :hour))
         (diff-seconds (lt:timestamp-difference target now)))
    (round diff-seconds 86400)))

(defun format-scheduled-indicator (todo)
  "Format the scheduled/due date indicator (e.g., 'Sched.232x:' or 'Due.5d:')."
  (cond
    ((todo-scheduled-date todo)
     (let ((days (days-until (todo-scheduled-date todo))))
       (cond
         ((< days 0)
          (format nil "Sched.~Dx:" (abs days)))  ; Overdue by X days
         ((zerop days)
          "Sched.today:")
         (t
          (format nil "Sched.~Dd:" days)))))     ; Due in X days
    ((todo-due-date todo)
     (let ((days (days-until (todo-due-date todo))))
       (cond
         ((< days 0)
          (format nil "Due.~Dx:" (abs days)))
         ((zerop days)
          "Due.today:")
         (t
          (format nil "Due.~Dd:" days)))))
    (t nil)))

(defun priority-indicator (priority)
  "Format priority as org-mode style [#A], [#B], [#C]."
  (case priority
    (:high "[#A]")
    (:medium "[#B]")
    (:low "[#C]")
    (otherwise "[#B]")))

(defun status-keyword (status)
  "Convert status to org-mode keyword."
  (case status
    (:pending "TODO")
    (:in-progress "DOING")
    (:completed "DONE")
    (:waiting "WAITING")
    (:cancelled "CANCELLED")
    (otherwise "TODO")))

;;── Grouping and Sorting ──────────────────────────────────────────────────────

(defun group-todos-by-tag (todos)
  "Group TODOs by their primary tag. Returns alist of (tag . todo-list).
   TODOs with no tags go under NIL."
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (todo todos)
      (let ((tag (first (todo-tags todo))))
        (push todo (gethash tag groups nil))))
    ;; Convert to alist and sort by tag name
    (let ((result nil))
      (maphash (lambda (tag todo-list)
                 (push (cons tag (nreverse todo-list)) result))
               groups)
      (sort result #'string< :key (lambda (pair)
                                     (or (car pair) ""))))))

(defun group-todos-by-date-key (todos)
  "Group TODOs by scheduled/due date. Returns alist of (date . todo-list).
   TODOs with no date go under NIL."
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (todo todos)
      (let* ((date (or (todo-scheduled-date todo)
                      (todo-due-date todo)))
             (date-key (when date
                        (lt:format-timestring nil date :format '(:year "-" (:month 2) "-" (:day 2))))))
        (push todo (gethash date-key groups nil))))
    ;; Convert to alist and sort by date
    (let ((result nil))
      (maphash (lambda (date todo-list)
                 (push (cons date (nreverse todo-list)) result))
               groups)
      (sort result #'string< :key (lambda (pair)
                                     (or (car pair) "9999-99-99"))))))

;;── Text Export (org-agenda style) ────────────────────────────────────────────

(defun export-todos-text (todos &key (stream t) (title "Week-agenda") (by-tag nil))
  "Export TODOs to STREAM in org-agenda format.
   If BY-TAG is true, group by tags; otherwise group by date."
  (let ((now (lt:now)))
    ;; Header
    (format stream "~A (~A):~%"
            title
            (format-week-range now))

    ;; Group and display
    (if by-tag
        ;; Group by tags
        (let ((groups (group-todos-by-tag todos)))
          (dolist (group groups)
            (let ((tag (car group))
                  (group-todos (cdr group)))
              ;; Tag header
              (if tag
                  (format stream "~%:~A:~%" tag)
                  (format stream "~%Untagged:~%"))

              ;; Sort by date within group
              (let ((sorted (sort (copy-list group-todos)
                                (lambda (a b)
                                  (let ((date-a (or (todo-scheduled-date a) (todo-due-date a)))
                                        (date-b (or (todo-scheduled-date b) (todo-due-date b))))
                                    (cond
                                      ((and date-a date-b)
                                       (lt:timestamp< date-a date-b))
                                      (date-a t)
                                      (t nil)))))))
                (dolist (todo sorted)
                  (let ((sched-indicator (format-scheduled-indicator todo)))
                    (format stream "  ~A~A ~A ~A~A~%"
                            (if sched-indicator
                                (format nil "~18A " sched-indicator)
                                (format nil "~18A " ""))
                            (status-keyword (todo-status todo))
                            (priority-indicator (todo-priority todo))
                            (todo-title todo)
                            (if (todo-url todo)
                                (format nil " ~A" (todo-url todo))
                                ""))))))))

        ;; Group by date
        (let ((groups (group-todos-by-date-key todos)))
          (dolist (group groups)
            (let ((date-str (car group))
                  (group-todos (cdr group)))
              ;; Date header
              (when date-str
                (let* ((date (lt:parse-timestring date-str))
                       (weekday (lt:format-timestring nil date :format '(:short-weekday)))
                       (day (lt:timestamp-day date))
                       (month (lt:format-timestring nil date :format '(:long-month)))
                       (year (lt:timestamp-year date)))
                  (format stream "~%~A  ~D ~A ~D~%"
                          weekday day month year)))

              ;; Sort by priority within group
              (let ((sorted (sort (copy-list group-todos)
                                (lambda (a b)
                                  (> (priority-order (todo-priority a))
                                     (priority-order (todo-priority b)))))))
                (dolist (todo sorted)
                  (let ((sched-indicator (format-scheduled-indicator todo)))
                    (format stream "  ~A~A ~A ~A~%"
                            (if sched-indicator
                                (format nil "~18A " sched-indicator)
                                (format nil "~18A " ""))
                            (status-keyword (todo-status todo))
                            (priority-indicator (todo-priority todo))
                            (todo-title todo)))))))))

    ;; Footer with statistics
    (let ((total (length todos))
          (completed (count-if (lambda (todo) (eq (todo-status todo) :completed)) todos))
          (overdue (count-if (lambda (todo)
                              (and (or (todo-scheduled-date todo) (todo-due-date todo))
                                   (not (eq (todo-status todo) :completed))
                                   (< (days-until (or (todo-scheduled-date todo) (todo-due-date todo))) 0)))
                            todos)))
      (format stream "~%---------------------------------------------------~%")
      (format stream "Total: ~D  Completed: ~D  Overdue: ~D~%"
              total completed overdue))))

;;── PDF Export (via external tools) ──────────────────────────────────────────

(defun command-exists-p (command)
  "Check if a command exists in PATH."
  (handler-case
      (progn
        (uiop:run-program (list "which" command)
                         :output nil
                         :error-output nil)
        t)
    (error () nil)))

(defun find-pdf-converter ()
  "Find an available PDF converter tool. Returns tool name or NIL."
  (cond
    ((command-exists-p "wkhtmltopdf") :wkhtmltopdf)
    ((command-exists-p "pandoc") :pandoc)
    ((command-exists-p "enscript") :enscript)
    (t nil)))

(defun text-to-pdf-wkhtmltopdf (text-file pdf-file)
  "Convert text file to PDF using wkhtmltopdf."
  ;; First convert to HTML with proper formatting
  (let ((html-file (format nil "~A.html" (pathname-name text-file))))
    (with-open-file (in text-file :direction :input)
      (with-open-file (out html-file :direction :output :if-exists :supersede)
        (format out "<!DOCTYPE html>~%<html><head>~%")
        (format out "<meta charset='UTF-8'>~%")
        (format out "<style>~%")
        (format out "body { font-family: 'Courier New', monospace; font-size: 11pt; margin: 2cm; }~%")
        (format out "pre { white-space: pre-wrap; }~%")
        (format out ".high { color: #FF0000; font-weight: bold; }~%")
        (format out ".medium { color: #FF8800; }~%")
        (format out ".low { color: #0088FF; }~%")
        (format out "</style>~%")
        (format out "</head><body><pre>~%")
        (loop for line = (read-line in nil nil)
              while line
              do (format out "~A~%" (escape-html line)))
        (format out "</pre></body></html>~%")))
    ;; Convert HTML to PDF
    (uiop:run-program (list "wkhtmltopdf" "-q" (namestring html-file) (namestring pdf-file)))
    ;; Clean up HTML
    (delete-file html-file)))

(defun text-to-pdf-pandoc (text-file pdf-file)
  "Convert text file to PDF using pandoc."
  (uiop:run-program (list "pandoc" (namestring text-file)
                          "-o" (namestring pdf-file)
                          "--pdf-engine=pdflatex"
                          "-V" "monofont:Courier"
                          "-V" "fontsize=11pt"
                          "-V" "geometry:margin=2cm")))

(defun text-to-pdf-enscript (text-file pdf-file)
  "Convert text file to PDF using enscript and ps2pdf."
  (let ((ps-file (format nil "~A.ps" (pathname-name text-file))))
    ;; Convert to PostScript
    (uiop:run-program (list "enscript" "-B" "-p" ps-file (namestring text-file)))
    ;; Convert PostScript to PDF
    (uiop:run-program (list "ps2pdf" ps-file (namestring pdf-file)))
    ;; Clean up PostScript
    (delete-file ps-file)))

(defun escape-html (text)
  "Escape HTML special characters."
  (let ((result text))
    (setf result (str:replace-all "&" "&amp;" result))
    (setf result (str:replace-all "<" "&lt;" result))
    (setf result (str:replace-all ">" "&gt;" result))
    result))

(defun export-todos-pdf (todos output-file &key (title "Week-agenda") (by-tag nil))
  "Export TODOs to PDF file. Returns T on success, NIL if no converter available."
  (let ((converter (find-pdf-converter)))
    (unless converter
      (return-from export-todos-pdf nil))

    ;; First export to text
    (let ((text-file (format nil "~A.txt" (pathname-name output-file))))
      (with-open-file (stream text-file :direction :output :if-exists :supersede)
        (export-todos-text todos :stream stream :title title :by-tag by-tag))

      ;; Convert to PDF
      (handler-case
          (progn
            (case converter
              (:wkhtmltopdf (text-to-pdf-wkhtmltopdf text-file output-file))
              (:pandoc (text-to-pdf-pandoc text-file output-file))
              (:enscript (text-to-pdf-enscript text-file output-file)))
            ;; Clean up text file
            (delete-file text-file)
            t)
        (error (e)
          (format *error-output* "Error converting to PDF: ~A~%" e)
          ;; Keep text file if PDF conversion failed
          nil)))))

;;── List Export ───────────────────────────────────────────────────────────────

(defun export-list (list-def items &key (stream t))
  "Export a list as formatted shareable text grouped by sections.
   LIST-DEF is a list-definition, ITEMS is a list of list-item objects."
  (let* ((now (lt:now))
         (date-str (lt:format-timestring nil now
                                         :format '(:day " " :short-month " " :year)))
         (sections (list-def-sections list-def))
         (section-items (make-hash-table :test #'equal))
         (no-section-items nil))
    ;; Group items by section
    (dolist (item items)
      (let ((section (list-item-section item)))
        (if (and section (> (length section) 0))
            (push item (gethash section section-items nil))
            (push item no-section-items))))
    ;; Header
    (format stream "~A (~A)~%" (list-def-name list-def) date-str)
    ;; Print items grouped by section (in defined section order)
    (dolist (section sections)
      (let ((sec-items (nreverse (gethash section section-items nil))))
        (when sec-items
          (format stream "~%~A~%" (string-upcase section))
          (dolist (item sec-items)
            (format stream "  [~A] ~A~%"
                    (if (list-item-checked item) "x" " ")
                    (list-item-title item))))))
    ;; Print items from sections not in the defined sections list
    (maphash (lambda (section sec-items)
               (unless (member section sections :test #'string-equal)
                 (let ((rev-items (nreverse sec-items)))
                   (format stream "~%~A~%" (string-upcase section))
                   (dolist (item rev-items)
                     (format stream "  [~A] ~A~%"
                             (if (list-item-checked item) "x" " ")
                             (list-item-title item))))))
             section-items)
    ;; Print items without a section under "Other"
    (when no-section-items
      (format stream "~%OTHER~%")
      (dolist (item (nreverse no-section-items))
        (format stream "  [~A] ~A~%"
                (if (list-item-checked item) "x" " ")
                (list-item-title item))))))

;;── Priority Helper ───────────────────────────────────────────────────────────

(defun priority-order (priority)
  "Return numeric order for sorting by priority."
  (case priority
    (:high 3)
    (:medium 2)
    (:low 1)
    (otherwise 0)))
