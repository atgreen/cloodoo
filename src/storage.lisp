;;; storage.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cloodoo)

;;── Data Directory ─────────────────────────────────────────────────────────────

(defun data-directory ()
  "Return the path to the cloodoo data directory."
  (merge-pathnames ".cloodoo/" (user-homedir-pathname)))

(defun todos-file ()
  "Return the path to the todos.json file."
  (merge-pathnames "todos.json" (data-directory)))

(defun user-context-file ()
  "Return the path to the user context file."
  (merge-pathnames "context.txt" (data-directory)))

(defun ensure-data-directory ()
  "Ensure the data directory exists."
  (ensure-directories-exist (data-directory)))

;;── Serialization ──────────────────────────────────────────────────────────────

(defun location-info-to-hash-table (location-info)
  "Convert a location-info plist to a hash table for JSON serialization."
  (when location-info
    (let ((ht (make-hash-table :test #'equal)))
      (setf (gethash "name" ht) (getf location-info :name))
      (setf (gethash "address" ht) (getf location-info :address))
      (setf (gethash "phone" ht) (getf location-info :phone))
      (setf (gethash "map_url" ht) (getf location-info :map-url))
      (setf (gethash "website" ht) (getf location-info :website))
      ht)))

(defun hash-table-to-location-info (ht)
  "Convert a hash table from JSON to a location-info plist."
  (when (and ht (hash-table-p ht))
    (let ((name (gethash "name" ht)))
      (when name
        (list :name name
              :address (gethash "address" ht)
              :phone (gethash "phone" ht)
              :map-url (gethash "map_url" ht)
              :website (gethash "website" ht))))))

(defun todo-to-hash-table (todo)
  "Convert a TODO object to a hash table for JSON serialization."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "id" ht) (todo-id todo))
    (setf (gethash "title" ht) (todo-title todo))
    (setf (gethash "description" ht) (todo-description todo))
    (setf (gethash "priority" ht) (string-downcase (symbol-name (todo-priority todo))))
    (setf (gethash "status" ht) (string-downcase (symbol-name (todo-status todo))))
    (setf (gethash "scheduled_date" ht) (when (todo-scheduled-date todo)
                                           (lt:format-rfc3339-timestring nil (todo-scheduled-date todo))))
    (setf (gethash "due_date" ht) (when (todo-due-date todo)
                                     (lt:format-rfc3339-timestring nil (todo-due-date todo))))
    (setf (gethash "tags" ht) (todo-tags todo))
    (setf (gethash "estimated_minutes" ht) (todo-estimated-minutes todo))
    (setf (gethash "location_info" ht) (location-info-to-hash-table (todo-location-info todo)))
    (setf (gethash "url" ht) (todo-url todo))
    (setf (gethash "created_at" ht) (lt:format-rfc3339-timestring nil (todo-created-at todo)))
    (setf (gethash "completed_at" ht) (when (todo-completed-at todo)
                                         (lt:format-rfc3339-timestring nil (todo-completed-at todo))))
    (setf (gethash "parent_id" ht) (todo-parent-id todo))
    (setf (gethash "device_id" ht) (todo-device-id todo))
    (setf (gethash "repeat_interval" ht) (todo-repeat-interval todo))
    (setf (gethash "repeat_unit" ht) (when (todo-repeat-unit todo)
                                        (string-downcase (symbol-name (todo-repeat-unit todo)))))
    ht))

(defun hash-table-to-todo (ht)
  "Convert a hash table from jzon to a TODO object."
  (make-instance 'todo
                 :id (gethash "id" ht)
                 :title (gethash "title" ht)
                 :description (let ((d (gethash "description" ht)))
                                (unless (eq d 'null) d))
                 :priority (intern (string-upcase (gethash "priority" ht)) :keyword)
                 :status (intern (string-upcase (gethash "status" ht)) :keyword)
                 :scheduled-date (let ((d (gethash "scheduled_date" ht)))
                                   (when (and d (not (eq d 'null)) (stringp d))
                                     (lt:parse-timestring d)))
                 :due-date (let ((d (gethash "due_date" ht)))
                             (when (and d (not (eq d 'null)) (stringp d))
                               (lt:parse-timestring d)))
                 :tags (let ((tags (gethash "tags" ht)))
                         (when (and tags (not (eq tags 'null)) (typep tags 'sequence))
                           (coerce tags 'list)))
                 :estimated-minutes (let ((m (gethash "estimated_minutes" ht)))
                                      (unless (eq m 'null) m))
                 :location-info (hash-table-to-location-info (gethash "location_info" ht))
                 :url (let ((u (gethash "url" ht)))
                        (unless (eq u 'null) u))
                 :created-at (lt:parse-timestring (gethash "created_at" ht))
                 :completed-at (let ((c (gethash "completed_at" ht)))
                                 (when (and c (not (eq c 'null)) (stringp c))
                                   (lt:parse-timestring c)))
                 :parent-id (let ((p (gethash "parent_id" ht)))
                              (unless (eq p 'null) p))
                 :device-id (let ((d (gethash "device_id" ht)))
                              (unless (eq d 'null) d))
                 :repeat-interval (let ((i (gethash "repeat_interval" ht)))
                                    (unless (or (null i) (eq i 'null)) i))
                 :repeat-unit (let ((u (gethash "repeat_unit" ht)))
                                (when (and u (not (eq u 'null)) (stringp u))
                                  (intern (string-upcase u) :keyword)))))

;;── Load/Save ──────────────────────────────────────────────────────────────────

(defun load-todos ()
  "Load todos from the data file. Returns an empty list if file doesn't exist."
  (let ((file (todos-file)))
    (if (probe-file file)
        (let ((data (with-open-file (stream file :direction :input)
                      (jzon:parse stream))))
          (let ((todos-array (gethash "todos" data)))
            (when todos-array
              (map 'list #'hash-table-to-todo todos-array))))
        nil)))

(defun save-todos (todos &optional presets)
  "Save todos to the data file. Optionally save presets as well."
  (ensure-data-directory)
  (let ((data (make-hash-table :test #'equal)))
    (setf (gethash "version" data) 1)
    (setf (gethash "todos" data) (mapcar #'todo-to-hash-table todos))
    ;; Save presets if provided
    (when presets
      (setf (gethash "tag_presets" data)
            (map 'list (lambda (p) (if p (coerce p 'list) :null)) presets)))
    ;; Also preserve existing presets if not provided
    (unless presets
      (let ((existing (load-presets)))
        (when existing
          (setf (gethash "tag_presets" data)
                (map 'list (lambda (p) (if p (coerce p 'list) :null)) existing)))))
    (with-open-file (stream (todos-file)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (jzon:stringify data :stream stream :pretty t))))

(defun load-presets ()
  "Load tag presets from the data file. Returns array of 10 preset lists."
  (let ((file (todos-file)))
    (if (probe-file file)
        (let* ((data (with-open-file (stream file :direction :input)
                       (jzon:parse stream)))
               (presets-data (gethash "tag_presets" data)))
          ;; Check that presets-data is actually a sequence (not :null or nil)
          (if (and presets-data (typep presets-data 'sequence))
              (let ((presets (make-array 10 :initial-element nil)))
                (loop for i from 0 below (min 10 (length presets-data))
                      for p = (aref presets-data i)
                      do (when (and p (not (eq p 'null)) (typep p 'sequence))
                           (setf (aref presets i) (coerce p 'list))))
                presets)
              (make-array 10 :initial-element nil)))
        (make-array 10 :initial-element nil))))

(defun save-presets (presets)
  "Save just the presets by loading existing todos and resaving with new presets."
  (let ((todos (load-todos)))
    (save-todos todos presets)))

;;── User Context ──────────────────────────────────────────────────────────────

(defparameter *default-user-context*
  "# User Context for TODO Enrichment
#
# This file provides personal context to help the AI assistant better understand
# and enrich your TODO items. Add information about yourself, your work, family,
# preferences, common locations, etc.
#
# Examples:
# - I work at Acme Corp as a software engineer
# - My dentist is Dr. Smith at 123 Main St, phone 555-1234
# - My kids are Alice (10) and Bob (7)
# - I live in Springfield
# - My preferred grocery store is Whole Foods on Oak Street
#
# Add your context below:

")

(defun load-user-context ()
  "Load the user context from the context file. Returns nil if file doesn't exist or is empty."
  (let ((file (user-context-file)))
    (when (probe-file file)
      (let ((content (uiop:read-file-string file)))
        (when (> (length (string-trim '(#\Space #\Newline #\Tab #\Return) content)) 0)
          content)))))

(defun ensure-user-context-file ()
  "Ensure the user context file exists with default template."
  (ensure-data-directory)
  (let ((file (user-context-file)))
    (unless (probe-file file)
      (with-open-file (stream file
                              :direction :output
                              :if-does-not-exist :create)
        (write-string *default-user-context* stream)))
    file))

(defun get-editor ()
  "Get the user's preferred editor from EDITOR or VISUAL env vars, or fall back to common editors."
  (or (uiop:getenv "EDITOR")
      (uiop:getenv "VISUAL")
      (cond
        ((uiop:run-program "which nano" :ignore-error-status t :output nil) "nano")
        ((uiop:run-program "which vim" :ignore-error-status t :output nil) "vim")
        ((uiop:run-program "which vi" :ignore-error-status t :output nil) "vi")
        (t "nano"))))

(defun edit-user-context ()
  "Open the user context file in the user's preferred editor.
   Returns T if editor was launched successfully."
  (let ((file (ensure-user-context-file))
        (editor (get-editor)))
    (handler-case
        (progn
          (uiop:run-program (list editor (namestring file))
                           :input :interactive
                           :output :interactive
                           :error-output :interactive)
          t)
      (error (e)
        (declare (ignore e))
        nil))))
