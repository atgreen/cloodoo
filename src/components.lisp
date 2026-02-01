;;; components.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cloodoo)

;;── UI Components ──────────────────────────────────────────────────────────────

;;; Title sanitization
(defun sanitize-title-for-display (title)
  "Strip newlines and collapse whitespace in todo titles for display."
  (str:collapse-whitespaces
   (str:replace-all (string #\Newline) " "
    (str:replace-all (string #\Return) " " title))))

;;; Priority formatting
(defun priority-string (priority)
  "Return priority indicator."
  (case priority
    (:high "!!!")
    (:medium "!! ")
    (:low "!  ")
    (otherwise "   ")))

(defun priority-colored (priority)
  "Return colored priority indicator."
  (let ((str (priority-string priority)))
    (case priority
      (:high (tui:bold (tui:colored str :fg tui:*fg-red*)))
      (:medium (tui:colored str :fg tui:*fg-yellow*))
      (:low (tui:colored str :fg tui:*fg-green*))
      (otherwise str))))

;;; Status formatting
(defun status-char (status)
  "Return status character."
  (case status
    (:completed "✓")
    (:in-progress "◐")
    (:pending "○")
    (:waiting "W")
    (:cancelled "✗")
    (:deleted "D")
    (otherwise " ")))

(defun status-colored (status)
  "Return colored status character."
  (let ((ch (status-char status)))
    (case status
      (:completed (tui:colored ch :fg tui:*fg-green*))
      (:in-progress (tui:colored ch :fg tui:*fg-cyan*))
      (:pending (tui:colored ch :fg tui:*fg-white*))
      (:waiting (tui:colored ch :fg tui:*fg-yellow*))
      (:cancelled (tui:colored ch :fg tui:*fg-bright-black*))
      (:deleted (tui:colored ch :fg tui:*fg-bright-black*))
      (otherwise ch))))

;;; Tags formatting
(defun format-tags (tags)
  "Format tags for display."
  (if (and tags (> (length tags) 0))
      (tui:colored (format nil "[~{~A~^,~}]" tags) :fg tui:*fg-magenta*)
      ""))

;;; Local timezone-aware "today" calculation
;;; Note: lt:today returns midnight UTC, which when converted to local time
;;; shows the previous day (e.g., 7pm EST on the 18th for midnight UTC on the 19th).
;;; This function returns midnight in the LOCAL timezone for correct date comparisons.
(defun local-today ()
  "Return a timestamp representing midnight today in local time.
   Unlike lt:today which uses midnight UTC, this uses midnight in the
   system's local timezone for correct date calculations."
  (let ((now (lt:now)))
    ;; Get year, month, day in local time, then create timestamp at midnight local
    (lt:encode-timestamp 0 0 0 0
                         (lt:timestamp-day now)
                         (lt:timestamp-month now)
                         (lt:timestamp-year now)
                         :timezone lt:*default-timezone*)))

;;; Due date formatting
(defun format-due-date (due-date)
  "Format due date for display with coloring."
  (when due-date
    (let* ((today (local-today))
           (tomorrow (lt:timestamp+ today 1 :day)))
      (cond
        ;; Overdue
        ((lt:timestamp< due-date today)
         (tui:bold (tui:colored
                   (format nil "OVERDUE ~A"
                          (lt:format-timestring nil due-date :format '(:short-month " " :day)))
                   :fg tui:*fg-red*)))
        ;; Today
        ((lt:timestamp< due-date tomorrow)
         (tui:bold (tui:colored "TODAY" :fg tui:*fg-yellow*)))
        ;; Tomorrow
        ((lt:timestamp< due-date (lt:timestamp+ tomorrow 1 :day))
         (tui:colored "Tomorrow" :fg tui:*fg-cyan*))
        ;; Future
        (t
         (tui:colored (lt:format-timestring nil due-date :format '(:short-month " " :day))
                     :fg tui:*fg-bright-black*))))))

;;; Date categorization for grouping
(defun categorize-by-date (todo)
  "Categorize a TODO by its scheduled date and due date.
   Uses scheduled date for grouping when to work on it,
   but checks due date for overdue status.
   Completed/cancelled items are never shown as overdue.
   Completed/cancelled items from before today return NIL (excluded from view).
   Deleted items always return NIL (never shown)."
  (let ((scheduled (todo-scheduled-date todo))
        (due (todo-due-date todo))
        (status (todo-status todo))
        (completed-at (todo-completed-at todo)))
    (let* ((today (local-today))
           (tomorrow (lt:timestamp+ today 1 :day))
           ;; Calculate end of current calendar week (Sunday)
           ;; day-of-week: 0=Sunday, 1=Monday, ..., 6=Saturday
           (day-of-week (lt:timestamp-day-of-week today))
           (days-to-sunday (if (zerop day-of-week) 0 (- 7 day-of-week)))
           (week-end (lt:timestamp+ today days-to-sunday :day)))
      ;; Deleted items are never shown
      (when (eql status :deleted)
        (return-from categorize-by-date nil))

      ;; Check if overdue (due date passed) - but not for completed/cancelled items
      (when (and due
                 (lt:timestamp< due today)
                 (not (member status '(:completed :cancelled))))
        (return-from categorize-by-date :overdue))

      ;; For completed/cancelled items, only show if completed today
      ;; Items completed before today should not appear in any date group
      (when (member status '(:completed :cancelled))
        (return-from categorize-by-date
          (if (and completed-at (lt:timestamp>= completed-at today))
              :today  ; Completed today - show in TODAY
              nil)))  ; Completed before today - exclude from view

      ;; Use scheduled date for grouping, fall back to due date
      (let ((primary-date (or scheduled due)))
        (if (null primary-date)
            :no-date
            (let ((days-until (truncate (lt:timestamp-difference primary-date today) 86400))
                  (next-week-end (lt:timestamp+ week-end 7 :day)))
              (cond
                ((lt:timestamp< primary-date tomorrow) :today)
                ((lt:timestamp< primary-date (lt:timestamp+ tomorrow 1 :day)) :tomorrow)
                ((lt:timestamp< primary-date week-end) :this-week)
                ((lt:timestamp< primary-date next-week-end) :next-week)
                ((<= days-until 30) :next-30-days)
                ((<= days-until 60) :next-60-days)
                ((<= days-until 90) :next-90-days)
                (t :later))))))))

(defun date-category-order (category)
  "Return sort order for date categories."
  (case category
    (:overdue 0)
    (:today 1)
    (:tomorrow 2)
    (:this-week 3)
    (:next-week 4)
    (:next-30-days 5)
    (:next-60-days 6)
    (:next-90-days 7)
    (:later 8)
    (:no-date 9)
    (otherwise 10)))

(defun date-category-label (category)
  "Return display label for date category."
  (case category
    (:overdue "OVERDUE")
    (:today (format nil "TODAY (~A)"
                   (lt:format-timestring nil (local-today) :format '(:short-weekday))))
    (:tomorrow "TOMORROW")
    (:this-week "THIS WEEK")
    (:next-week "NEXT WEEK")
    (:next-30-days "NEXT 30 DAYS")
    (:next-60-days "NEXT 60 DAYS")
    (:next-90-days "NEXT 90 DAYS")
    (:later "LATER")
    (:no-date "UNSCHEDULED")
    (otherwise "OTHER")))

(defun date-category-border (category)
  "Return the border style for a date category."
  (case category
    (:overdue (tui:make-border :top "─" :bottom "─" :left "│" :right "├"
                               :top-left "╭" :top-right "╮"
                               :bottom-left "╰" :bottom-right "╯"))
    (otherwise *border-header-title*)))

(defun date-category-colored (category width)
  "Return colored category header rendered as pager-style box with line.
   WIDTH is the total width to fill."
  (let* ((label (date-category-label category))
         (border (date-category-border category))
         (header (render-pager-header label width :border border)))
    ;; Apply category-specific coloring to the entire header
    (case category
      (:overdue (tui:bold (tui:colored header :fg tui:*fg-red*)))
      (:today (tui:bold (tui:colored header :fg tui:*fg-yellow*)))
      (:tomorrow (tui:colored header :fg tui:*fg-cyan*))
      (:this-week (tui:colored header :fg tui:*fg-blue*))
      (:next-week (tui:colored header :fg tui:*fg-bright-cyan*))
      (:next-30-days (tui:colored header :fg tui:*fg-green*))
      (:next-60-days (tui:colored header :fg tui:*fg-bright-green*))
      (:next-90-days (tui:colored header :fg tui:*fg-bright-blue*))
      (:later (tui:colored header :fg tui:*fg-magenta*))
      (:no-date (tui:colored header :fg tui:*fg-bright-black*))
      (otherwise header))))

(defun group-todos-by-date (todos)
  "Group TODOs by date category (each todo by its own dates).
   TODOs with NIL category (e.g., completed before today) are excluded."
  (let ((groups (make-hash-table)))
    (dolist (todo todos)
      (let ((cat (categorize-by-date todo)))
        (when cat  ; Skip todos with nil category
          (push todo (gethash cat groups nil)))))
    (let ((alist nil))
      (maphash (lambda (k v)
                 (push (cons k (nreverse v)) alist))
               groups)
      (sort alist #'< :key (lambda (pair) (date-category-order (first pair)))))))

;;; Todo line rendering
(defun render-todo-line (todo selected-p width)
  "Render a single TODO line in retro style."
  (let* ((status (status-colored (todo-status todo)))
         (priority (priority-colored (todo-priority todo)))
         (title (sanitize-title-for-display (todo-title todo)))
         (due (format-due-date (todo-due-date todo)))
         (completed-p (eql (todo-status todo) :completed))
         ;; Calculate widths
         (prefix-len 8)  ; "  ○ !!! "
         (due-len (if due (+ 2 (length (format nil "~A" due))) 0))
         (avail (- width prefix-len due-len 2))
         (trunc-title (if (> (length title) avail)
                         (concatenate 'string (subseq title 0 (- avail 2)) "..")
                         title))
         (title-disp (if completed-p
                        (tui:colored trunc-title :fg tui:*fg-bright-black*)
                        trunc-title))
         (padding (max 1 (- avail (length trunc-title)))))
    (if selected-p
        (format nil "~A ~A ~A ~A~A~@[ ~A~]"
                (tui:colored "►" :fg tui:*fg-cyan*)
                status priority
                (tui:bold title-disp)
                (make-string padding :initial-element #\Space)
                due)
        (format nil "  ~A ~A ~A~A~@[ ~A~]"
                status priority
                title-disp
                (make-string padding :initial-element #\Space)
                due))))

;;; Status bar
(defun render-status-bar (width &rest items)
  "Render a status bar with items."
  (let* ((content (format nil "~{ ~A ~^│~}" items))
         (padding (max 0 (- width (length content)))))
    (tui:colored
     (format nil "~A~A" content (make-string padding :initial-element #\Space))
     :fg tui:*fg-black* :bg tui:*bg-cyan*)))

;;; Title bar
(defun render-title-bar (title width &optional right-text)
  "Render a title bar."
  (let* ((right (or right-text ""))
         (right-len (length right))
         (title-len (length title))
         (padding (max 1 (- width title-len right-len 2))))
    (tui:bold
     (tui:colored
      (format nil " ~A~A~A " title (make-string padding :initial-element #\Space) right)
      :fg tui:*fg-white* :bg tui:*bg-blue*))))

;;; Help bar at bottom
(defun render-help-bar (&optional width)
  "Render the bottom help bar."
  (let* ((help "F1:Help │ ↑↓:Navigate │ Enter:View │ Space:Done │ A:Add │ E:Edit │ U:Context │ DEL:Del │ /:Search │ Q:Quit")
         (w (or width (length help))))
    (tui:colored
     (format nil "~A~A"
             help
             (make-string (max 0 (- w (length help))) :initial-element #\Space))
     :fg tui:*fg-yellow* :bg tui:*bg-blue*)))

;;── Org-mode style helpers (for compatibility) ────────────────────────────────

(defun org-status-colored (status)
  "Return org-mode style colored status keyword."
  (case status
    (:completed (tui:colored "DONE" :fg tui:*fg-green*))
    (:in-progress (tui:bold (tui:colored "STRT" :fg tui:*fg-cyan*)))
    (:pending (tui:bold (tui:colored "TODO" :fg tui:*fg-magenta*)))
    (:waiting (tui:bold (tui:colored "WAIT" :fg tui:*fg-yellow*)))
    (:cancelled (tui:colored "CNCL" :fg tui:*fg-bright-black*))
    (:deleted (tui:colored "DELE" :fg tui:*fg-bright-black*))
    (otherwise "    ")))

(defun org-priority-colored (priority)
  "Return org-mode style colored priority."
  (case priority
    (:high (tui:bold (tui:colored "[#A]" :fg tui:*fg-red*)))
    (:medium (tui:colored "[#B]" :fg tui:*fg-yellow*))
    (:low (tui:colored "[#C]" :fg tui:*fg-green*))
    (otherwise "    ")))

(defun org-tags-string (tags)
  "Format tags in org-mode style :tag1:tag2:."
  (if (and tags (> (length tags) 0))
      (format nil ":~{~A~^:~}:" tags)
      ""))

(defun format-schedule-info-plain (scheduled-date due-date)
  "Format schedule/deadline info as plain text (no colors).
   Returns fixed-width string like '262 d. ago: ' or 'Sched.137x: '."
  (let* ((today (local-today))
         (tomorrow (lt:timestamp+ today 1 :day))
         (today-day (lt:timestamp-day today))
         (today-month (lt:timestamp-month today))
         (today-year (lt:timestamp-year today)))
    (flet ((days-difference (ts1 ts2)
             (truncate (lt:timestamp-difference ts1 ts2) 86400)))
      (cond
        ;; Overdue: due date is before today (not just before current time)
        ((and due-date (lt:timestamp< due-date today))
         (format nil "~3D d. ago: " (max 1 (days-difference today due-date))))
        ;; Scheduled in past (but not due yet)
        ((and scheduled-date (lt:timestamp< scheduled-date today))
         (format nil "Sched.~3Dx: " (max 1 (days-difference today scheduled-date))))
        ;; Scheduled for today
        ((and scheduled-date
              (= (lt:timestamp-day scheduled-date) today-day)
              (= (lt:timestamp-month scheduled-date) today-month)
              (= (lt:timestamp-year scheduled-date) today-year))
         "Scheduled:  ")
        ;; Scheduled in future
        (scheduled-date
         (format nil "In ~3D d.:  " (max 1 (days-difference scheduled-date today))))
        ;; Due date (not overdue)
        (due-date
         (let ((days-until (days-difference due-date today)))
           (if (<= days-until 7)
               (format nil "Due ~2D d.:  " days-until)
               "            ")))
        (t "            ")))))

(defun format-schedule-info (scheduled-date due-date)
  "Format schedule/deadline info in org-agenda style.
   Returns fixed-width string like '262 d. ago: ' or 'Sched.137x: '."
  (let* ((today (local-today))
         (today-day (lt:timestamp-day today))
         (today-month (lt:timestamp-month today))
         (today-year (lt:timestamp-year today)))
    (flet ((days-difference (ts1 ts2)
             "Return number of days between two timestamps (positive if ts1 > ts2)."
             (truncate (lt:timestamp-difference ts1 ts2) 86400)))
      (cond
        ;; Deadline/due overdue - "262 d. ago: " (due date is before today)
        ((and due-date (lt:timestamp< due-date today))
         (let ((days-ago (max 1 (days-difference today due-date))))
           (tui:bold (tui:colored (format nil "~3D d. ago: " days-ago) :fg tui:*fg-red*))))
        ;; Scheduled in past - "Sched.137x: "
        ((and scheduled-date (lt:timestamp< scheduled-date today))
         (let ((days-ago (max 1 (days-difference today scheduled-date))))
           (tui:colored (format nil "Sched.~3Dx: " days-ago) :fg tui:*fg-magenta*)))
        ;; Scheduled for today
        ((and scheduled-date
              (= (lt:timestamp-day scheduled-date) today-day)
              (= (lt:timestamp-month scheduled-date) today-month)
              (= (lt:timestamp-year scheduled-date) today-year))
         (tui:colored "Scheduled:  " :fg tui:*fg-green*))
        ;; Scheduled in future
        (scheduled-date
         (let ((days-until (max 1 (days-difference scheduled-date today))))
           (tui:colored (format nil "In ~3D d.:  " days-until) :fg tui:*fg-cyan*)))
        ;; Due date coming up (not overdue)
        (due-date
         (let ((days-until (days-difference due-date today)))
           (if (<= days-until 7)
               (tui:colored (format nil "Due ~2D d.:  " days-until) :fg tui:*fg-yellow*)
               "            ")))
        ;; No date info
        (t "            ")))))

(defun render-filter-status (status priority search)
  "Render the current filter status for display."
  (let ((parts nil))
    (when search
      (push (tui:colored (format nil "Search:~A" search) :fg tui:*fg-cyan*) parts))
    (when priority
      (push (tui:colored (format nil "Priority:~A" priority) :fg tui:*fg-yellow*) parts))
    (when (and status (not (eql status :all)))
      (push (tui:colored (format nil "Status:~A" status) :fg tui:*fg-green*) parts))
    (if parts
        (format nil "[~{~A~^ ~}]" (nreverse parts))
        "")))

(defun format-due-date-cli (due-date)
  "Format due date for CLI output."
  (when due-date
    (let* ((today (local-today))
           (tomorrow (lt:timestamp+ today 1 :day)))
      (cond
        ((lt:timestamp< due-date today)
         (tui:bold (tui:colored "OVERDUE" :fg tui:*fg-red*)))
        ((lt:timestamp< due-date tomorrow)
         (tui:bold (tui:colored "TODAY" :fg tui:*fg-yellow*)))
        ((lt:timestamp< due-date (lt:timestamp+ tomorrow 1 :day))
         (tui:colored "Tomorrow" :fg tui:*fg-cyan*))
        (t
         (tui:colored (lt:format-timestring nil due-date :format '(:short-month " " :day))
                     :fg tui:*fg-bright-black*))))))

;;── Subtask Progress Formatting ───────────────────────────────────────────────

;;── Pager-style Header (like Charm's bubbletea) ────────────────────────────────

(defparameter *border-header-title*
  (tui:make-border :top "─" :bottom "─" :left "│" :right "├"
                   :top-left "╭" :top-right "╮"
                   :bottom-left "╰" :bottom-right "╯")
  "Border style for header title box - rounded corners with ├ on the right.")

(defun render-pager-header (title width &key (border *border-header-title*))
  "Render a pager-style header with title in a bordered box and a line extending to width.
   Format:
      ╭───────────╮
     ─┤ Mr. Pager ├────────────────────────────────
      ╰───────────╯
   The horizontal line extends from the middle (content) row, with ─┤ on the left."
  (let* (;; Render the title with padding inside the border
         (padded-title (format nil " ~A " title))
         ;; Create the bordered title box
         (title-box (tui:render-border padded-title border))
         ;; Create lines for each row of the title box
         (box-lines (tui:split-string-by-newline title-box))
         ;; Get the width of the title box (max visible width of all lines)
         (box-width (if box-lines
                        (apply #'max (mapcar #'tui:visible-length box-lines))
                        0))
         ;; Calculate remaining width for the line (subtract 1 for the leading ─)
         (line-width (max 0 (- width box-width 1)))
         (num-lines (length box-lines))
         ;; Middle row is where we extend with the horizontal line
         (middle-row (floor num-lines 2)))
    ;; Join each line of the title box with appropriate fill
    (with-output-to-string (s)
      (loop for line in box-lines
            for i from 0
            do (when (> i 0) (format s "~%"))
               (if (= i middle-row)
                   ;; Middle row: ─┤ content ├────
                   ;; Replace leading │ with ─┤
                   (let ((modified-line (if (and (> (length line) 0)
                                                 (char= (char line 0) #\│))
                                            (concatenate 'string "─┤" (subseq line 1))
                                            (concatenate 'string "─┤" line))))
                     (format s "~A~A" modified-line (make-string line-width :initial-element #\─)))
                   ;; Other rows: space prefix to align with ─┤
                   (format s " ~A~A" line (make-string line-width :initial-element #\Space)))))))
