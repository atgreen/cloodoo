;;; components.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cloodoo)

;;── 90s Text-Mode IDE Style UI Components ─────────────────────────────────────

;;; Box drawing characters
(defparameter *box-h* "─")      ; horizontal
(defparameter *box-v* "│")      ; vertical
(defparameter *box-tl* "┌")     ; top-left
(defparameter *box-tr* "┐")     ; top-right
(defparameter *box-bl* "└")     ; bottom-left
(defparameter *box-br* "┘")     ; bottom-right
(defparameter *box-lt* "├")     ; left-tee
(defparameter *box-rt* "┤")     ; right-tee
(defparameter *box-tt* "┬")     ; top-tee
(defparameter *box-bt* "┴")     ; bottom-tee

;;; Double-line box drawing
(defparameter *dbox-h* "═")
(defparameter *dbox-v* "║")
(defparameter *dbox-tl* "╔")
(defparameter *dbox-tr* "╗")
(defparameter *dbox-bl* "╚")
(defparameter *dbox-br* "╝")

;;; Block/shade characters
(defparameter *shade-light* "░")
(defparameter *shade-med* "▒")
(defparameter *shade-dark* "▓")
(defparameter *block-full* "█")

;;; Helper to make horizontal lines
(defun make-hline (width &optional (char *box-h*))
  (make-string width :initial-element (char char 0)))

(defun make-box-top (width &optional (title nil))
  "Create top of a box with optional centered title."
  (if title
      (let* ((inner (- width 4))
             (title-len (min (length title) (- inner 2)))
             (trimmed (subseq title 0 title-len))
             (padding (- inner title-len))
             (left-pad (floor padding 2))
             (right-pad (- padding left-pad)))
        (format nil "~A~A~A ~A ~A~A~A"
                *box-tl*
                (make-hline left-pad)
                *box-rt*
                trimmed
                *box-lt*
                (make-hline right-pad)
                *box-tr*))
      (format nil "~A~A~A" *box-tl* (make-hline (- width 2)) *box-tr*)))

(defun make-box-bottom (width)
  "Create bottom of a box."
  (format nil "~A~A~A" *box-bl* (make-hline (- width 2)) *box-br*))

(defun make-box-line (content width)
  "Create a line inside a box with padding."
  (let* ((content-len (length content))
         (inner (- width 2))
         (padding (max 0 (- inner content-len))))
    (format nil "~A~A~A~A"
            *box-v*
            content
            (make-string padding :initial-element #\Space)
            *box-v*)))

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
    (otherwise " ")))

(defun status-colored (status)
  "Return colored status character."
  (let ((ch (status-char status)))
    (case status
      (:completed (tui:colored ch :fg tui:*fg-green*))
      (:in-progress (tui:colored ch :fg tui:*fg-cyan*))
      (:pending (tui:colored ch :fg tui:*fg-white*))
      (:waiting (tui:colored ch :fg tui:*fg-yellow*))
      (otherwise ch))))

;;; Tags formatting
(defun format-tags (tags)
  "Format tags for display."
  (if (and tags (> (length tags) 0))
      (tui:colored (format nil "[~{~A~^,~}]" tags) :fg tui:*fg-magenta*)
      ""))

;;; Due date formatting
(defun format-due-date (due-date)
  "Format due date for display with coloring."
  (when due-date
    (let* ((today (lt:today))
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
   but checks due date for overdue status."
  (let ((scheduled (todo-scheduled-date todo))
        (due (todo-due-date todo)))
    ;; Completed items go to completed section
    (when (eq (todo-status todo) :completed)
      (return-from categorize-by-date :completed))

    (let* ((today (lt:today))
           (tomorrow (lt:timestamp+ today 1 :day))
           (week-end (lt:timestamp+ today 7 :day)))
      ;; Check if overdue (due date passed)
      (when (and due (lt:timestamp< due today))
        (return-from categorize-by-date :overdue))

      ;; Use scheduled date for grouping, fall back to due date
      (let ((primary-date (or scheduled due)))
        (if (null primary-date)
            :no-date
            (cond
              ((lt:timestamp< primary-date tomorrow) :today)
              ((lt:timestamp< primary-date (lt:timestamp+ tomorrow 1 :day)) :tomorrow)
              ((lt:timestamp< primary-date week-end) :this-week)
              (t :later)))))))

(defun date-category-order (category)
  "Return sort order for date categories."
  (case category
    (:overdue 0)
    (:today 1)
    (:tomorrow 2)
    (:this-week 3)
    (:later 4)
    (:no-date 5)
    (:completed 6)
    (otherwise 7)))

(defun date-category-label (category)
  "Return display label for date category."
  (case category
    (:overdue "=== OVERDUE ===")
    (:today (format nil "=== TODAY (~A) ==="
                   (lt:format-timestring nil (lt:today) :format '(:short-weekday))))
    (:tomorrow "=== TOMORROW ===")
    (:this-week "=== THIS WEEK ===")
    (:later "=== LATER ===")
    (:no-date "=== UNSCHEDULED ===")
    (:completed "=== DONE ===")
    (otherwise "=== OTHER ===")))

(defun date-category-colored (category)
  "Return colored category label."
  (let ((label (date-category-label category)))
    (case category
      (:overdue (tui:bold (tui:colored label :fg tui:*fg-red*)))
      (:today (tui:bold (tui:colored label :fg tui:*fg-yellow*)))
      (:tomorrow (tui:colored label :fg tui:*fg-cyan*))
      (:this-week (tui:colored label :fg tui:*fg-blue*))
      (:later (tui:colored label :fg tui:*fg-magenta*))
      (:no-date (tui:colored label :fg tui:*fg-bright-black*))
      (:completed (tui:colored label :fg tui:*fg-green*))
      (otherwise label))))

(defun group-todos-by-date (todos)
  "Group TODOs by date category."
  (let ((groups (make-hash-table)))
    (dolist (todo todos)
      (let ((cat (categorize-by-date todo)))
        (push todo (gethash cat groups nil))))
    (let ((alist nil))
      (maphash (lambda (k v)
                 (push (cons k (nreverse v)) alist))
               groups)
      (sort alist #'< :key (lambda (pair) (date-category-order (car pair)))))))

;;; Todo line rendering
(defun render-todo-line (todo selected-p width)
  "Render a single TODO line in retro style."
  (let* ((status (status-colored (todo-status todo)))
         (priority (priority-colored (todo-priority todo)))
         (title (todo-title todo))
         (due (format-due-date (todo-due-date todo)))
         (completed-p (eq (todo-status todo) :completed))
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
  (let* ((help "F1:Help │ ↑↓:Navigate │ Enter:View │ Space:Done │ A:Add │ E:Edit │ D:Del │ /:Search │ Q:Quit")
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

(defun format-schedule-info (scheduled-date due-date)
  "Format schedule/deadline info in org-agenda style.
   Returns fixed-width string like '262 d. ago: ' or 'Sched.137x: '."
  (let* ((now (lt:now))
         (today-day (lt:timestamp-day now))
         (today-month (lt:timestamp-month now))
         (today-year (lt:timestamp-year now)))
    (flet ((days-difference (ts1 ts2)
             "Return number of days between two timestamps (positive if ts1 > ts2)."
             (truncate (lt:timestamp-difference ts1 ts2) 86400)))
      (cond
        ;; Deadline/due overdue - "262 d. ago: "
        ((and due-date (lt:timestamp< due-date now))
         (let ((days-ago (max 1 (days-difference now due-date))))
           (tui:bold (tui:colored (format nil "~3D d. ago: " days-ago) :fg tui:*fg-red*))))
        ;; Scheduled and overdue - "Sched.137x: "
        ((and scheduled-date (lt:timestamp< scheduled-date now))
         (let ((days-ago (max 1 (days-difference now scheduled-date))))
           (tui:colored (format nil "Sched.~3Dx: " days-ago) :fg tui:*fg-magenta*)))
        ;; Scheduled for today
        ((and scheduled-date
              (= (lt:timestamp-day scheduled-date) today-day)
              (= (lt:timestamp-month scheduled-date) today-month)
              (= (lt:timestamp-year scheduled-date) today-year))
         (tui:colored "Scheduled:  " :fg tui:*fg-green*))
        ;; Scheduled in future
        (scheduled-date
         (let ((days-until (max 1 (days-difference scheduled-date now))))
           (tui:colored (format nil "In ~3D d.:  " days-until) :fg tui:*fg-cyan*)))
        ;; Due date coming up (not overdue)
        (due-date
         (let ((days-until (days-difference due-date now)))
           (if (<= days-until 7)
               (tui:colored (format nil "Due ~2D d.:  " days-until) :fg tui:*fg-yellow*)
               "            ")))
        ;; No date info
        (t "            ")))))

(defun date-category-header-colored (category)
  "Return colored category header for org-agenda style grouping."
  (date-category-colored category))

(defun render-filter-status (status priority search)
  "Render the current filter status for display."
  (let ((parts nil))
    (when search
      (push (tui:colored (format nil "Search:~A" search) :fg tui:*fg-cyan*) parts))
    (when priority
      (push (tui:colored (format nil "Priority:~A" priority) :fg tui:*fg-yellow*) parts))
    (when (and status (not (eq status :all)))
      (push (tui:colored (format nil "Status:~A" status) :fg tui:*fg-green*) parts))
    (if parts
        (format nil "[~{~A~^ ~}]" (nreverse parts))
        "")))

(defun format-due-date-cli (due-date)
  "Format due date for CLI output."
  (when due-date
    (let* ((today (lt:today))
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

(defun format-subtask-progress (progress)
  "Format subtask progress (completed . total) for display.
   Returns nil if no progress to show."
  (when progress
    (let* ((completed (car progress))
           (total (cdr progress))
           (text (format nil "~D/~D" completed total)))
      (cond
        ((= completed total)
         (tui:colored text :fg tui:*fg-green*))
        ((> completed 0)
         (tui:colored text :fg tui:*fg-yellow*))
        (t
         (tui:colored text :fg tui:*fg-bright-black*))))))

(defun format-collapse-indicator (collapsed-p has-children-p)
  "Return collapse/expand indicator for a todo with children.
   Returns nil if todo has no children."
  (when has-children-p
    (if collapsed-p
        (tui:colored "▸" :fg tui:*fg-cyan*)
        (tui:colored "▾" :fg tui:*fg-cyan*))))
