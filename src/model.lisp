;;; model.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cloodoo)

;;── Constants ──────────────────────────────────────────────────────────────────

(defconstant +priority-high+ :high)
(defconstant +priority-medium+ :medium)
(defconstant +priority-low+ :low)

(defconstant +status-pending+ :pending)
(defconstant +status-in-progress+ :in-progress)
(defconstant +status-completed+ :completed)
(defconstant +status-waiting+ :waiting)
(defconstant +status-cancelled+ :cancelled)
(defconstant +status-deleted+ :deleted)

;;── TODO Class ─────────────────────────────────────────────────────────────────

(defclass todo ()
  ((id
    :initarg :id
    :accessor todo-id
    :type string
    :documentation "Unique identifier for the TODO.")
   (title
    :initarg :title
    :accessor todo-title
    :type string
    :documentation "Short description of the TODO.")
   (description
    :initarg :description
    :initform nil
    :accessor todo-description
    :type (or null string)
    :documentation "Optional longer notes.")
   (priority
    :initarg :priority
    :initform +priority-medium+
    :accessor todo-priority
    :type keyword
    :documentation "Priority level: :high, :medium, or :low.")
   (status
    :initarg :status
    :initform +status-pending+
    :accessor todo-status
    :type keyword
    :documentation "Status: :pending, :in-progress, or :completed.")
   (scheduled-date
    :initarg :scheduled-date
    :initform nil
    :accessor todo-scheduled-date
    :type (or null lt:timestamp)
    :documentation "Date when work on this task is scheduled to begin (SCHEDULED in org-mode).")
   (due-date
    :initarg :due-date
    :initform nil
    :accessor todo-due-date
    :type (or null lt:timestamp)
    :documentation "Date by which this task must be completed (DEADLINE in org-mode).")
   (tags
    :initarg :tags
    :initform nil
    :accessor todo-tags
    :type list
    :documentation "List of tag strings.")
   (estimated-minutes
    :initarg :estimated-minutes
    :initform nil
    :accessor todo-estimated-minutes
    :type (or null integer)
    :documentation "Estimated time to complete in minutes.")
   (location-info
    :initarg :location-info
    :initform nil
    :accessor todo-location-info
    :type (or null list)
    :documentation "Location/business info plist with :name :address :phone :map-url :website.")
   (url
    :initarg :url
    :initform nil
    :accessor todo-url
    :type (or null string)
    :documentation "URL associated with this TODO (e.g., email link).")
   (created-at
    :initarg :created-at
    :accessor todo-created-at
    :type lt:timestamp
    :documentation "Timestamp of creation.")
   (completed-at
    :initarg :completed-at
    :initform nil
    :accessor todo-completed-at
    :type (or null lt:timestamp)
    :documentation "Timestamp when completed.")
   (enriching-p
    :initarg :enriching-p
    :initform nil
    :accessor todo-enriching-p
    :type boolean
    :documentation "True if this TODO is currently being enriched by the LLM.")
   (parent-id
    :initarg :parent-id
    :initform nil
    :accessor todo-parent-id
    :type (or null string)
    :documentation "ID of parent TODO, nil for top-level items.")
   (device-id
    :initarg :device-id
    :initform nil
    :accessor todo-device-id
    :type (or null string)
    :documentation "UUID of the device that created/modified this TODO.")
   (repeat-interval
    :initarg :repeat-interval
    :initform nil
    :accessor todo-repeat-interval
    :type (or null integer)
    :documentation "Number of units between repetitions (e.g., 2 for every 2 days).")
   (repeat-unit
    :initarg :repeat-unit
    :initform nil
    :accessor todo-repeat-unit
    :type (or null keyword)
    :documentation "Unit for repetition: :day, :week, :month, :year."))
  (:documentation "A TODO item."))

(defun generate-id ()
  "Generate a simple unique ID."
  (format nil "~A-~A"
          (get-universal-time)
          (random 100000)))

(defun parse-tags (input)
  "Parse tags from INPUT, splitting on whitespace and commas.
   INPUT can be a string, a list of strings, or nil.
   Returns a flat list of non-empty tag strings."
  (cond
    ((null input) nil)
    ((stringp input)
     ;; Split string on whitespace and commas using regex
     (let ((result nil))
       (dolist (part (cl-ppcre:split "[\\s,]+" input))
         (when (> (length part) 0)
           (push part result)))
       (nreverse result)))
    ((listp input)
     ;; Process each element and flatten
     (let ((result nil))
       (dolist (item input)
         (dolist (tag (parse-tags item))
           (push tag result)))
       (nreverse result)))
    (t nil)))

(defun make-todo (title &key description priority scheduled-date due-date tags estimated-minutes location-info url parent-id device-id repeat-interval repeat-unit)
  "Create a new TODO item with the given properties."
  (make-instance 'todo
                 :id (generate-id)
                 :title title
                 :description description
                 :priority (or priority +priority-medium+)
                 :scheduled-date scheduled-date
                 :due-date due-date
                 :tags (or tags nil)
                 :estimated-minutes estimated-minutes
                 :location-info location-info
                 :url url
                 :parent-id parent-id
                 :device-id device-id
                 :repeat-interval repeat-interval
                 :repeat-unit repeat-unit
                 :created-at (lt:now)))

(defmethod print-object ((todo todo) stream)
  (print-unreadable-object (todo stream :type t :identity t)
    (format stream "~A [~A] ~A"
            (todo-id todo)
            (todo-priority todo)
            (todo-title todo))))

;;── Nested Task Helper Functions ──────────────────────────────────────────────

(defun get-children (todos parent-id)
  "Return list of todos that have the given parent-id."
  (remove-if-not (lambda (todo)
                   (equal (todo-parent-id todo) parent-id))
                 todos))

(defun get-todo-depth (todos todo)
  "Return nesting depth of TODO (0 for top-level items)."
  (let ((depth 0)
        (current-parent-id (todo-parent-id todo)))
    (loop while current-parent-id
          do (let ((parent (find current-parent-id todos
                                 :key #'todo-id :test #'equal)))
               (if parent
                   (progn
                     (incf depth)
                     (setf current-parent-id (todo-parent-id parent)))
                   (setf current-parent-id nil))))
    depth))

(defun count-subtask-progress (todos parent-id)
  "Return (completed . total) count for direct children of parent-id."
  (let ((children (get-children todos parent-id)))
    (if (null children)
        nil
        (let ((completed (count-if (lambda (todo)
                                     (eq (todo-status todo) +status-completed+))
                                   children))
              (total (length children)))
          (cons completed total)))))

(defun has-children-p (todos todo)
  "Return T if TODO has any children."
  (some (lambda (t2)
          (equal (todo-parent-id t2) (todo-id todo)))
        todos))

(defun get-descendants (todos parent-id)
  "Return all descendants of a todo (children, grandchildren, etc.)."
  (let ((result nil))
    (labels ((collect (pid)
               (dolist (child (get-children todos pid))
                 (push child result)
                 (collect (todo-id child)))))
      (collect parent-id))
    (nreverse result)))

;;── Repeating Task Helpers ────────────────────────────────────────────────────

(defun calculate-next-occurrence (todo)
  "Calculate next scheduled date based on repeat interval and unit.
   Returns a timestamp for the next occurrence, or NIL if not a repeating task."
  (let ((interval (todo-repeat-interval todo))
        (unit (todo-repeat-unit todo)))
    (when (and interval unit (> interval 0))
      (let ((base (or (todo-scheduled-date todo) (lt:now))))
        (case unit
          (:day (lt:timestamp+ base interval :day))
          (:week (lt:timestamp+ base (* interval 7) :day))
          (:month (lt:timestamp+ base interval :month))
          (:year (lt:timestamp+ base interval :year))
          (otherwise nil))))))
