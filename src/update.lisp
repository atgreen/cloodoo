;;; update.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cloodoo)

;;── Constants ─────────────────────────────────────────────────────────────────

(defconstant +header-lines+ 3
  "Number of lines each date category header takes (top border, content, bottom border).")
(defconstant +list-top-lines+ 2
  "Number of lines above the list viewport (title and date header).")

;;── Global TUI Program Reference ──────────────────────────────────────────────

(defvar *tui-program-ref* nil
  "Reference to the TUI program for forcing redraws after exec-cmd callbacks.
   This is a workaround for tuition library issue where exec-cmd callbacks
   don't trigger immediate redraws.")

;;── Application State (Model) ──────────────────────────────────────────────────

(defclass app-model ()
  ((todos
    :initarg :todos
    :initform nil
    :accessor model-todos
    :documentation "List of TODO items.")
   (cursor
    :initarg :cursor
    :initform 0
    :accessor model-cursor
    :documentation "Currently selected item index.")
   (view-state
    :initarg :view-state
   :initform :list
   :accessor model-view-state
    :documentation "Current view: :list, :detail, :add, :edit, :help, :search, :delete-confirm, :delete-done-confirm, :import, :edit-date, :add-scheduled-date, :add-due-date.") ; lint:suppress max-line-length
   (detail-urls
    :initform nil
    :accessor model-detail-urls
    :documentation "List of (start-line end-line url) for clickable URLs in detail view.")
   (filter-status
    :initarg :filter-status
    :initform nil
    :accessor model-filter-status
    :documentation "Filter by status, or nil for all.")
   (filter-priority
    :initarg :filter-priority
    :initform nil
    :accessor model-filter-priority
    :documentation "Filter by priority, or nil for all.")
   (search-query
    :initarg :search-query
    :initform ""
    :accessor model-search-query
    :documentation "Current search query.")
   (sort-by
    :initarg :sort-by
    :initform :priority
    :accessor model-sort-by
    :documentation "Sort field: :priority, :due-date, :created-at, :title.")
   (sort-descending
    :initarg :sort-descending
    :initform t
    :accessor model-sort-descending
    :documentation "Sort direction.")
   (term-width
    :initarg :term-width
    :initform 80
    :accessor model-term-width
    :documentation "Terminal width.")
   (term-height
    :initarg :term-height
    :initform 24
    :accessor model-term-height
    :documentation "Terminal height.")
   (scroll-offset
    :initarg :scroll-offset
    :initform 0
    :accessor model-scroll-offset
    :documentation "Scroll offset for the list view.")
   (scrollbar-dragging
    :initform nil
    :accessor model-scrollbar-dragging
    :documentation "Non-nil when dragging the scrollbar.")
   ;; Text input for add/edit/search
   (title-input
    :initform nil
    :accessor model-title-input
    :documentation "Text input for title.")
   (description-input
    :initform nil
    :accessor model-description-input
    :documentation "Text input for description.")
   (search-input
    :initform nil
    :accessor model-search-input
    :documentation "Text input for search.")
   (import-input
    :initform nil
    :accessor model-import-input
    :documentation "Text input for org-mode import filename.")
   (active-field
    :initform :title
    :accessor model-active-field
    :documentation "Currently active input field in add/edit view.")
   (edit-priority
    :initform :medium
    :accessor model-edit-priority
    :documentation "Priority being edited.")
   (edit-todo-id
    :initform nil
    :accessor model-edit-todo-id
    :documentation "ID of TODO being edited, nil for new TODO.")
   (status-message
    :initform nil
    :accessor model-status-message
    :documentation "Temporary status message to display.")
   (enrichment-spinner
    :initform nil
    :accessor model-enrichment-spinner
    :documentation "Spinner for enriching items.")
   ;; Sidebar state
   (selected-tags
    :initform (make-hash-table :test #'equal)
    :accessor model-selected-tags
    :documentation "Hash table of selected tag names for filtering.")
   (all-tags-cache
    :initform nil
    :accessor model-all-tags-cache
    :documentation "Cached sorted list of all unique tags.")
   (sidebar-visible
    :initform t
    :accessor model-sidebar-visible
    :documentation "Whether sidebar is shown.")
   (sidebar-focused
    :initform nil
    :accessor model-sidebar-focused
    :documentation "T if keyboard focus is on sidebar, nil for main list.")
   (sidebar-cursor
    :initform 0
    :accessor model-sidebar-cursor
    :documentation "Selected row in sidebar (0=All, 1+=tags).")
   (tag-presets
    :initform (make-array 10 :initial-element nil)
    :accessor model-tag-presets
    :documentation "Array of 10 preset tag selections (lists of tag names).")
   ;; Date picker for scheduled/deadline editing
   (date-picker
    :initform nil
    :accessor model-date-picker
    :documentation "Datepicker component for editing dates.")
   (editing-date-type
    :initform nil
    :accessor model-editing-date-type
    :documentation "Which date we're editing: :scheduled or :deadline.")
   ;; Form state for dates and repeating in add/edit view
   (edit-scheduled-date
    :initform nil
    :accessor model-edit-scheduled-date
    :documentation "Scheduled date being edited in add/edit form.")
   (edit-due-date
    :initform nil
    :accessor model-edit-due-date
    :documentation "Due date being edited in add/edit form.")
   (edit-repeat-interval
    :initform nil
    :accessor model-edit-repeat-interval
    :documentation "Repeat interval being edited (e.g., 1, 2, 7).")
   (edit-repeat-unit
    :initform nil
    :accessor model-edit-repeat-unit
    :documentation "Repeat unit being edited: :day, :week, :month, :year.")
   ;; Tags editing state
   (tags-input
    :initform nil
    :accessor model-tags-input
    :documentation "Text input for typing tag names in form.")
   (edit-tags
    :initform nil
    :accessor model-edit-tags
    :documentation "List of tags being edited in form or inline editor.")
   (tag-dropdown-visible
    :initform nil
    :accessor model-tag-dropdown-visible
    :documentation "Whether tag autocomplete dropdown is visible.")
   (tag-dropdown-cursor
    :initform 0
    :accessor model-tag-dropdown-cursor
    :documentation "Selected item in tag autocomplete dropdown.")
   (tag-dropdown-filtered
    :initform nil
    :accessor model-tag-dropdown-filtered
    :documentation "Filtered list of tags matching current input.")
   ;; Visible todos cache for stable ordering
   (visible-todos-cache
    :initform nil
    :accessor model-visible-todos-cache
    :documentation "Cached list of visible todos to maintain stable order.")
   (visible-todos-dirty
    :initform t
    :accessor model-visible-todos-dirty
    :documentation "When T, regenerate visible-todos-cache on next access.")
   (deleting-tag
    :initform nil
    :accessor model-deleting-tag
    :documentation "Tag name being deleted (for confirmation dialog).")
   ;; Sync client state
   (sync-status
    :initform :disconnected
    :accessor model-sync-status
    :documentation "Sync connection status: :disconnected, :connecting, :connected, :error.")
   (sync-server-address
    :initform nil
    :accessor model-sync-server-address
    :documentation "Address of the sync server (host:port) if connected as client.")
   (sync-pending-count
    :initform 0
    :accessor model-sync-pending-count
    :documentation "Number of pending changes from server during initial sync.")
   (sync-error-message
    :initform nil
    :accessor model-sync-error-message
    :documentation "Error message if sync failed.")
   ;; Lists management state
   (lists-data
    :initform nil
    :accessor model-lists-data
    :documentation "Loaded list definitions for lists overview.")
   (lists-cursor
    :initform 0
    :accessor model-lists-cursor
    :documentation "Selected list index in lists overview.")
   (list-detail-def
    :initform nil
    :accessor model-list-detail-def
    :documentation "List definition currently being viewed.")
   (list-detail-items
    :initform nil
    :accessor model-list-detail-items
    :documentation "Items for the list being viewed.")
   (list-detail-cursor
    :initform 0
    :accessor model-list-detail-cursor
    :documentation "Selected item index in list detail view.")
   (list-detail-scroll
    :initform 0
    :accessor model-list-detail-scroll
    :documentation "Scroll offset in list detail view.")
   (list-form-name-input
    :initform nil
    :accessor model-list-form-name-input
    :documentation "Text input for list name in create/edit form.")
   (list-form-desc-input
    :initform nil
    :accessor model-list-form-desc-input
    :documentation "Text input for list description in create/edit form.")
   (list-form-sections-input
    :initform nil
    :accessor model-list-form-sections-input
    :documentation "Text input for list sections in create/edit form.")
   (list-form-active-field
    :initform :name
    :accessor model-list-form-active-field
    :documentation "Active field in list create/edit form: :name, :desc, :sections, :submit.")
   (list-form-editing-id
    :initform nil
    :accessor model-list-form-editing-id
    :documentation "List ID being edited, nil for new list.")
   (list-item-add-input
    :initform nil
    :accessor model-list-item-add-input
    :documentation "Text input for adding a new item to a list."))
  (:documentation "The application model following TEA pattern."))

(defun make-initial-model ()
  "Create the initial application model."
  (let ((model (make-instance 'app-model
                              :todos (load-todos))))
    ;; Load persisted presets
    (setf (model-tag-presets model) (load-presets))
    model))

;;── Collapse State Management ─────────────────────────────────────────────────

;;── Tag Collection ────────────────────────────────────────────────────────────

(defun collect-all-tags (todos)
  "Return sorted list of unique tags from all TODOs."
  (let ((tags (make-hash-table :test #'equal)))
    (dolist (todo todos)
      (dolist (tag (todo-tags todo))
        (setf (gethash tag tags) t)))
    (sort (loop for k being the hash-keys of tags collect k) #'string<)))

(defun refresh-tags-cache (model)
  "Update the all-tags-cache from current todos."
  (setf (model-all-tags-cache model)
        (collect-all-tags (model-todos model))))

;;── Tag Filtering ─────────────────────────────────────────────────────────────

(defun filter-todos-by-tags (todos selected-tags)
  "Filter todos to those matching ANY selected tag.
   Empty selection = show all."
  (if (zerop (hash-table-count selected-tags))
      todos
      (remove-if-not
       (lambda (todo)
         (some (lambda (tag) (gethash tag selected-tags))
               (todo-tags todo)))
       todos)))

;;── Child Task Helpers ────────────────────────────────────────────────────────

;;── Tag Autocomplete Helpers ──────────────────────────────────────────────────

(defun filter-tags-by-query (all-tags query already-selected)
  "Filter tags containing query substring (case-insensitive).
   Excludes tags that are already selected."
  (let ((q (string-downcase (or query ""))))
    (if (zerop (length q))
        ;; No query - show all unselected tags
        (remove-if (lambda (tag) (member tag already-selected :test #'string=)) all-tags)
        ;; Filter by query substring
        (remove-if (lambda (tag)
                     (or (member tag already-selected :test #'string=)
                         (not (search q (string-downcase tag)))))
                   all-tags))))

(defun update-tag-dropdown (model)
  "Update filtered tags based on input, excluding already-selected tags."
  (let* ((query (tui.textinput:textinput-value (model-tags-input model)))
         (all-tags (model-all-tags-cache model))
         (edit-tags (model-edit-tags model))
         (filtered (filter-tags-by-query all-tags query edit-tags)))
    (setf (model-tag-dropdown-filtered model) filtered)
    ;; Reset cursor if out of bounds
    (when (>= (model-tag-dropdown-cursor model) (length filtered))
      (setf (model-tag-dropdown-cursor model) (max 0 (1- (length filtered)))))
    ;; Show dropdown if there are matches and input is non-empty
    (setf (model-tag-dropdown-visible model)
          (and (> (length filtered) 0)
               (> (length query) 0)))))

(defun add-tag-to-edit-tags (model tag)
  "Add tag(s) to the edit-tags list if not already present.
   TAG is parsed to split on whitespace/commas into multiple tags."
  (dolist (parsed-tag (parse-tags tag))
    (unless (member parsed-tag (model-edit-tags model) :test #'string=)
      (push parsed-tag (model-edit-tags model))))
  ;; Clear input and update dropdown
  (tui.textinput:textinput-set-value (model-tags-input model) "")
  (setf (model-tag-dropdown-visible model) nil)
  (setf (model-tag-dropdown-cursor model) 0))

(defun remove-last-tag-from-edit-tags (model)
  "Remove the last tag from edit-tags list (for backspace on empty input)."
  (when (model-edit-tags model)
    (setf (model-edit-tags model) (butlast (model-edit-tags model)))))

;;── Filtering and Sorting ─────────────────────────────────────────────────────

(defun filter-todos (todos &key status priority search-query)
  "Filter todos by status, priority, and search query.
   Deleted items are always excluded."
  (let ((result todos))
    ;; Always exclude deleted items
    (setf result (remove-if (lambda (todo)
                              (eql (todo-status todo) :deleted))
                            result))
    (when status
      (setf result (remove-if-not (lambda (todo)
                                    (eq (todo-status todo) status))
                                  result)))
    (when priority
      (setf result (remove-if-not (lambda (todo)
                                    (eq (todo-priority todo) priority))
                                  result)))
    (when (and search-query (> (length search-query) 0))
      (let ((query (string-downcase search-query)))
        (setf result (remove-if-not
                      (lambda (todo)
                        (or (search query (string-downcase (todo-title todo)))
                            (and (todo-description todo)
                                 (search query (string-downcase (todo-description todo))))))
                      result))))
    result))

(defun priority-order (priority)
  "Return numeric order for priority (higher = more important)."
  (case priority
    (:high 3)
    (:medium 2)
    (:low 1)
    (otherwise 0)))

(defun sort-todos (todos sort-by descending)
  "Sort todos by the given field."
  (let ((sorted (copy-list todos)))
    (setf sorted
          (sort sorted
                (lambda (a b)
                  (case sort-by
                    (:priority
                     (> (priority-order (todo-priority a))
                        (priority-order (todo-priority b))))
                    (:due-date
                     (let ((da (todo-due-date a))
                           (db (todo-due-date b)))
                       (cond
                         ((and da db) (lt:timestamp< da db))
                         (da t)
                         (db nil)
                         (t nil))))
                    (:created-at
                     (lt:timestamp< (todo-created-at a) (todo-created-at b)))
                    (:title
                     (string< (todo-title a) (todo-title b)))
                    (otherwise nil)))))
    (if descending
        sorted
        (reverse sorted))))

(defun invalidate-visible-todos-cache (model)
  "Mark the visible todos cache as dirty so it will be regenerated."
  (setf (model-visible-todos-dirty model) t))

(defun compute-visible-todos-grouped (model)
  "Compute the grouped list of visible todos after filtering, sorting, and collapsing.
   Returns an alist of (category . todo-ids) preserving display order.
   When children appear in a group, their parent chain is included for context."
  (let* ((all-todos (model-todos model))
         (tag-filtered (filter-todos-by-tags all-todos (model-selected-tags model)))
         (filtered (filter-todos tag-filtered
                                 :status (model-filter-status model)
                                 :priority (model-filter-priority model)
                                 :search-query (model-search-query model)))
         (groups (group-todos-by-date filtered))
         (result nil))
    ;; Build result with todo IDs (not objects) to allow status changes
    (dolist (group groups)
      (let* ((category (first group))
             (group-todos (rest group))
             ;; Sort by priority within group
             (sorted (sort-todos group-todos :priority t))
             (visible-ids (mapcar #'todo-id sorted)))
        (when visible-ids
          (push (cons category visible-ids) result))))
    (nreverse result)))

(defun get-visible-todos-grouped (model)
  "Get the grouped list of visible todos, using cache for stable ordering.
   Returns an alist of (category . todos) where todos are current objects.
   The cache preserves grouping when status is toggled (Space).
   Call invalidate-visible-todos-cache to force re-grouping (on 'r', add, delete)."
  (when (model-visible-todos-dirty model)
    ;; Regenerate the cache (stores IDs, not objects)
    (setf (model-visible-todos-cache model) (compute-visible-todos-grouped model))
    (setf (model-visible-todos-dirty model) nil))
  ;; Return cached groups, but resolve IDs to current todo objects
  (let ((all-todos (model-todos model)))
    (loop for (category . ids) in (model-visible-todos-cache model)
          for todos = (remove nil
                              (mapcar (lambda (id)
                                        (find id all-todos :key #'todo-id :test #'equal))
                                      ids))
          when todos
          collect (cons category todos))))

(defun get-visible-todos (model)
  "Get flat list of visible todos (for cursor navigation etc.)."
  (let ((groups (get-visible-todos-grouped model)))
    (loop for (category . todos) in groups
          append todos)))

(defun reorder-todos (todos sort-by descending)
  "Reorder todos by date category, then sort within each group."
  (let* ((groups (group-todos-by-date todos))
         (result nil))
    (dolist (group groups)
      (let* ((group-todos (rest group))
             (sorted (sort-todos group-todos sort-by descending)))
        (dolist (todo sorted)
          (push todo result))))
    (nreverse result)))

;;── Scroll Management ─────────────────────────────────────────────────────────

(defun list-line-index-for-cursor (groups cursor)
  "Return 0-based line index for the given cursor in grouped list output."
  (let ((current 0)
        (line-idx 0))
    (dolist (group groups)
      ;; Header takes +header-lines+ lines (pager-style box)
      (incf line-idx +header-lines+)
      (dolist (todo (rest group))
        (when (= current cursor)
          (return-from list-line-index-for-cursor line-idx))
        (incf current)
        (incf line-idx)))
    (max 0 (1- line-idx))))

(defun header-start-line-for-cursor (groups cursor)
  "Return the 0-based line index where the header for the cursor's group starts."
  (let ((current 0)
        (line-idx 0))
    (dolist (group groups)
      (let ((header-start line-idx))
        ;; Skip header lines
        (incf line-idx +header-lines+)
        (dolist (todo (rest group))
          (when (= current cursor)
            (return-from header-start-line-for-cursor header-start))
          (incf current)
          (incf line-idx))))
    0))

(defun cursor-index-for-line (groups line-idx)
  "Return the cursor index for a given 0-based line index, or NIL if none."
  (let ((current 0)
        (line 0))
    (dolist (group groups)
      ;; Header lines
      (when (< line-idx (+ line +header-lines+))
        (return-from cursor-index-for-line nil))
      (incf line +header-lines+)
      (dolist (todo (rest group))
        (when (= line-idx line)
          (return-from cursor-index-for-line current))
        (incf current)
        (incf line)))
    nil))

(defun adjust-scroll (model viewport-height)
  "Adjust scroll offset to keep selected item visible."
  (let* ((cursor (model-cursor model))
         (offset (model-scroll-offset model))
         (todos (get-visible-todos model))
         (groups (group-todos-by-date todos))
         (num-lines (max 1 (+ (length todos) (* (length groups) +header-lines+))))
         (cursor-line (list-line-index-for-cursor groups cursor))
         (header-start (header-start-line-for-cursor groups cursor))
         (max-offset (max 0 (- num-lines viewport-height))))
    (cond
      ;; Scrolling up: show the group header when the item would be off-screen
      ((< cursor-line offset)
       (setf (model-scroll-offset model) (max 0 header-start)))
      ;; Scrolling down: keep the item visible at bottom
      ((>= cursor-line (+ offset viewport-height))
       (setf (model-scroll-offset model)
             (min max-offset (- cursor-line viewport-height -1))))
      ;; Clamp to max offset
      ((> offset max-offset)
       (setf (model-scroll-offset model) max-offset)))))

(defun scroll-to-y-position (model y-pos viewport-height)
  "Scroll the list to position based on Y coordinate in the scrollbar area."
  (let* ((todos (get-visible-todos model))
         (groups (group-todos-by-date todos))
         (num-lines (max 1 (+ (length todos) (* (length groups) +header-lines+))))
         (max-offset (max 0 (- num-lines viewport-height)))
         (scroll-percent (/ (float y-pos) (max 1 (1- viewport-height))))
         (new-offset (round (* scroll-percent max-offset))))
    (setf (model-scroll-offset model) (max 0 (min max-offset new-offset)))))

(defun get-todo-urls (todo)
  "Extract all URLs from a todo as a list."
  (let ((urls nil))
    (when (todo-url todo)
      (push (todo-url todo) urls))
    (when (todo-location-info todo)
      (let ((loc (todo-location-info todo)))
        (when (getf loc :website)
          (push (getf loc :website) urls))
        (when (getf loc :map-url)
          (push (getf loc :map-url) urls))))
    (nreverse urls)))

(defun open-url (url)
  "Open URL in the system default browser."
  (let ((command #+linux "xdg-open"
                 #+darwin "open"
                 #-(or linux darwin) "xdg-open"))
    (uiop:launch-program (list command url))))

(defun open-attachment (hash)
  "Open an attachment by hash. Extracts to cache directory and opens with system viewer.
   Downloads from sync server if not available locally."
  (when hash
    ;; First check if we have it locally
    (with-db (db)
      (multiple-value-bind (stored-hash content filename mime-type size created-at)
          (resolve-attachment db hash)
        (declare (ignore stored-hash size created-at))
        ;; If not found locally, try to download from server
        (unless content
          (when (download-attachment-from-server hash)
            ;; Re-fetch after download
            (multiple-value-setq (stored-hash content filename mime-type size created-at)
              (resolve-attachment db hash))))
        (if content
            ;; Determine file extension from mime-type or filename
            (let* ((ext (cond
                          ((and filename (str:contains? "." filename))
                           (subseq filename (1+ (position #\. filename :from-end t))))
                          ((string= mime-type "image/jpeg") "jpg")
                          ((string= mime-type "image/png") "png")
                          ((string= mime-type "image/gif") "gif")
                          ((string= mime-type "image/webp") "webp")
                          (t "bin")))
                   (cache-dir (cache-directory))
                   (cache-file (merge-pathnames (format nil "attachments/~A.~A" hash ext) cache-dir)))
              ;; Ensure cache/attachments directory exists
              (ensure-directories-exist cache-file)
              ;; Write content to cache file if not already there
              (unless (probe-file cache-file)
                (with-open-file (out cache-file
                                     :direction :output
                                     :element-type '(unsigned-byte 8)
                                     :if-exists :supersede)
                  (write-sequence content out)))
              ;; Open with system viewer
              (open-url (namestring cache-file)))
            ;; No content available
            (llog:warn "Attachment not available" :hash hash))))))

;;── TEA Methods ───────────────────────────────────────────────────────────────

(defmethod tui:init ((model app-model))
  "Initialize the application model."
  (tui:set-terminal-title "cloodoo")
  (let ((size (tui:get-terminal-size)))
    (when size
      (setf (model-term-width model) (first size))
      (setf (model-term-height model) (rest size))))
  ;; Initialize text inputs
  (setf (model-title-input model)
        (tui.textinput:make-textinput
         :prompt "Title: "
         :placeholder "Enter TODO title"
         :width 50
         :char-limit 200))
  (setf (model-description-input model)
        (tui.textinput:make-textinput
         :prompt "Notes: "
         :placeholder "(press N to edit in $EDITOR)"
         :width 50
         :char-limit 0))
  (setf (model-search-input model)
        (tui.textinput:make-textinput
         :prompt "Search: "
         :placeholder "Type to filter..."
         :width 40
         :char-limit 100))
  (setf (model-import-input model)
        (tui.textinput:make-textinput
         :prompt "File: "
         :placeholder "Path to org-mode file..."
         :width 60
         :char-limit 500))
  (setf (model-tags-input model)
        (tui.textinput:make-textinput
         :prompt ""
         :placeholder "Type to add..."
         :width 25
         :char-limit 50))
  ;; Initialize enrichment spinner
  (setf (model-enrichment-spinner model)
        (tui.spinner:make-spinner :frames tui.spinner:*spinner-minidot*
                                  :fps 0.1))
  ;; Start spinner ticking
  (tui.spinner:spinner-init (model-enrichment-spinner model))
  ;; Initialize tags cache
  (refresh-tags-cache model)
  ;; Initialize datepicker with custom styles for better visibility
  (let ((custom-styles
          (tui.datepicker:make-datepicker-styles
           :cursor (tui:make-style :reverse t :foreground tui:*fg-cyan* :background tui:*bg-black*)
           :selected (tui:make-style :reverse t :foreground tui:*fg-green*)
           :selected-cursor (tui:make-style :reverse t :bold t :foreground tui:*fg-green* :background tui:*bg-black*)
           :today (tui:make-style :bold t :foreground tui:*fg-yellow*))))
    (setf (model-date-picker model)
          (tui.datepicker:make-datepicker :styles custom-styles))))

;;── Sidebar Key Handling ───────────────────────────────────────────────────────

(defun handle-sidebar-keys (model msg)
  "Handle keyboard input when sidebar is focused."
  (let* ((key (tui:key-msg-key msg))
         (tags (model-all-tags-cache model))
         (max-cursor (length tags)))  ; 0=All, 1..n=tags
    (cond
      ;; Tab - return focus to main list
      ((eql key :tab)
       (setf (model-sidebar-focused model) nil)
       (values model nil))

      ;; Move up
      ((or (eql key :up)
           (and (characterp key) (or (char= key #\k) (char= key #\p))))
       (when (> (model-sidebar-cursor model) 0)
         (decf (model-sidebar-cursor model)))
       (values model nil))

      ;; Move down
      ((or (eql key :down)
           (and (characterp key) (or (char= key #\j) (char= key #\n))))
       (when (< (model-sidebar-cursor model) max-cursor)
         (incf (model-sidebar-cursor model)))
       (values model nil))

      ;; Space - toggle tag selection
      ((and (characterp key) (char= key #\Space))
       (let ((cursor (model-sidebar-cursor model))
             (selected (model-selected-tags model)))
         (if (zerop cursor)
             ;; "All" selected - clear all filters
             (clrhash selected)
             ;; Toggle specific tag
             (let ((tag (nth (1- cursor) tags)))
               (when tag
                 (if (gethash tag selected)
                     (remhash tag selected)
                     (setf (gethash tag selected) t))))))
       (invalidate-visible-todos-cache model)
       (setf (model-cursor model) 0)  ; Reset main list cursor
       (values model nil))

      ;; 'a' - select all (clear filter)
      ((and (characterp key) (char= key #\a))
       (clrhash (model-selected-tags model))
       (invalidate-visible-todos-cache model)
       (setf (model-cursor model) 0)
       (values model nil))

      ;; 'n' - select none (clear all selections)
      ((and (characterp key) (char= key #\n))
       (clrhash (model-selected-tags model))
       (invalidate-visible-todos-cache model)
       (setf (model-cursor model) 0)
       (values model nil))

      ;; Shift+1 through Shift+9 and Shift+0 - save preset
      ;; Shift+1=! Shift+2=@ Shift+3=# Shift+4=$ Shift+5=% Shift+6=^ Shift+7=& Shift+8=* Shift+9=( Shift+0=)
      ((and (characterp key) (position key "!@#$%^&*()"))
       (let* ((pos (position key "!@#$%^&*()"))
              (preset-idx pos)  ; !=0, @=1, #=2, $=3, %=4, ^=5, &=6, *=7, (=8, )=9
              (selected (model-selected-tags model))
              (tag-list (loop for tag being the hash-keys of selected collect tag)))
         (setf (aref (model-tag-presets model) preset-idx)
               (if tag-list tag-list nil))
         (save-presets (model-tag-presets model)))
       (values model nil))

      ;; Escape or q - return to list without changes
      ((or (eql key :escape) (and (characterp key) (char= key #\q)))
       (setf (model-sidebar-focused model) nil)
       (values model nil))

      ;; Delete - delete the selected tag (not available for "All")
      ((or (eql key :delete) (eql key :backspace)
           (and (characterp key) (char= key #\d)))
       (let ((cursor (model-sidebar-cursor model)))
         (if (zerop cursor)
             ;; "All" is selected - can't delete
             (values model nil)
             ;; Delete specific tag
             (let ((tag (nth (1- cursor) tags)))
               (when tag
                 (setf (model-deleting-tag model) tag)
                 (setf (model-view-state model) :delete-tag-confirm))
               (values model nil)))))

      (t (values model nil)))))

;;── List View Key Handling ─────────────────────────────────────────────────────

(defun handle-list-keys (model msg)
  "Handle keyboard input in list view."
  ;; If sidebar is focused, dispatch to sidebar handler
  (when (model-sidebar-focused model)
    (return-from handle-list-keys (handle-sidebar-keys model msg)))

  (let ((key (tui:key-msg-key msg))
        (ctrl (tui:key-msg-ctrl msg))
        (todos (get-visible-todos model)))
    (llog:debug "List view key received"
                :key (format nil "~S" key)
                :ctrl ctrl
                :char-p (characterp key))
    (cond
      ;; Quit
      ((or (and (characterp key) (char= key #\q))
           (and ctrl (characterp key) (char= key #\c)))
       (values model (tui:quit-cmd)))

      ;; Toggle sidebar visibility (l)
      ((and (characterp key) (char= key #\l))
       (setf (model-sidebar-visible model) (not (model-sidebar-visible model)))
       (values model nil))

      ;; Focus sidebar (Tab when sidebar is visible and not at a todo for indenting)
      ;; Note: Tab for indent is handled separately below with the > key
      ((and (eql key :tab) (model-sidebar-visible model) (not (model-sidebar-focused model)))
       (setf (model-sidebar-focused model) t)
       (values model nil))

      ;; Apply preset 1-9 (digit keys apply presets)
      ((and (characterp key) (digit-char-p key) (not (char= key #\0)))
       (let* ((digit (digit-char-p key))
              (preset-idx (1- digit))  ; 1->0, 2->1, etc
              (preset (aref (model-tag-presets model) preset-idx)))
         (when preset
           (clrhash (model-selected-tags model))
           (dolist (tag preset)
             (setf (gethash tag (model-selected-tags model)) t))
           (invalidate-visible-todos-cache model)
           (setf (model-cursor model) 0)))
       (values model nil))

      ;; Apply preset 0 (stored in slot 9)
      ((and (characterp key) (char= key #\0))
       (let ((preset (aref (model-tag-presets model) 9)))
         (when preset
           (clrhash (model-selected-tags model))
           (dolist (tag preset)
             (setf (gethash tag (model-selected-tags model)) t))
           (invalidate-visible-todos-cache model)
           (setf (model-cursor model) 0)))
       (values model nil))

      ;; Move up (p/k/up - org uses p for previous)
      ((or (eql key :up)
           (and (characterp key) (or (char= key #\k) (char= key #\p))))
       (when (> (model-cursor model) 0)
         (decf (model-cursor model)))
       (values model nil))

      ;; Move down (n/j/down - org uses n for next)
      ((or (eql key :down)
           (and (characterp key) (or (char= key #\j) (char= key #\n))))
       (when (< (model-cursor model) (1- (length todos)))
         (incf (model-cursor model)))
       (values model nil))

      ;; Page up (Ctrl+U or Page Up key)
      ((or (eql key :page-up)
           (and ctrl (characterp key) (char= key #\u)))
       (let* ((page-size (max 1 (- (model-term-height model) 6)))
              (new-pos (max 0 (- (model-cursor model) page-size))))
         (setf (model-cursor model) new-pos))
       (values model nil))

      ;; Page down (Ctrl+D or Page Down key)
      ((or (eql key :page-down)
           (and ctrl (characterp key) (char= key #\d)))
       (let* ((page-size (max 1 (- (model-term-height model) 6)))
              (max-pos (max 0 (1- (length todos))))
              (new-pos (min max-pos (+ (model-cursor model) page-size))))
         (setf (model-cursor model) new-pos))
       (values model nil))

      ;; Increase priority (Shift+Up)
      ((eql key :shift-up)
       (when (< (model-cursor model) (length todos))
         (let ((todo (nth (model-cursor model) todos)))
           (setf (todo-priority todo)
                 (case (todo-priority todo)
                   (:low :medium)
                   (:medium :high)
                   (:high :high)))  ; Already at max
           (save-todo todo)))
       (values model nil))

      ;; Decrease priority (Shift+Down)
      ((eql key :shift-down)
       (when (< (model-cursor model) (length todos))
         (let ((todo (nth (model-cursor model) todos)))
           (setf (todo-priority todo)
                 (case (todo-priority todo)
                   (:high :medium)
                   (:medium :low)
                   (:low :low)))  ; Already at min
           (save-todo todo)))
       (values model nil))

      ;; Go to top
      ((or (eql key :home) (and (characterp key) (char= key #\g)))
       (setf (model-cursor model) 0)
       (values model nil))

      ;; Go to bottom
      ((or (eql key :end) (and (characterp key) (char-equal key #\G)))
       (setf (model-cursor model) (max 0 (1- (length todos))))
       (values model nil))

      ;; Toggle status (Space): TODO -> DONE -> WAITING -> CANCELLED -> TODO
      ;; For repeating tasks, completing reschedules to next occurrence
      ((and (characterp key) (char= key #\Space))
       (when (< (model-cursor model) (length todos))
         (let ((todo (nth (model-cursor model) todos)))
           (case (todo-status todo)
             ((:pending :in-progress)
              ;; Check if this is a repeating task
              (cond ((and (todo-repeat-interval todo) (todo-repeat-unit todo)) (let ((next-date (calculate-next-occurrence todo)))
                    (when next-date
                      (setf (todo-scheduled-date todo) next-date)
                      ;; Adjust due-date if it was set (shift by same interval)
                      (when (todo-due-date todo)
                        (let* ((interval (todo-repeat-interval todo))
                               (unit (todo-repeat-unit todo)))
                          (setf (todo-due-date todo)
                                (case unit
                                  (:day (lt:timestamp+ (todo-due-date todo) interval :day))
                                  (:week (lt:timestamp+ (todo-due-date todo) (* interval 7) :day))
                                  (:month (lt:timestamp+ (todo-due-date todo) interval :month))
                                  (:year (lt:timestamp+ (todo-due-date todo) interval :year))
                                  (otherwise (todo-due-date todo))))))
                      ;; Keep status as pending (or reset from in-progress)
                      (setf (todo-status todo) +status-pending+)
                      (setf (todo-completed-at todo) nil))
                    (setf (model-status-message model)
                          (format nil "Rescheduled to ~A"
                                  (lt:format-timestring nil next-date
                                                       :format '(:short-month " " :day))))))
      (t
                    (setf (todo-status todo) +status-completed+)
                    (setf (todo-completed-at todo) (lt:now)))))
             (:completed
              (setf (todo-status todo) +status-waiting+)
              (setf (todo-completed-at todo) nil))
             (:waiting
              (setf (todo-status todo) +status-cancelled+)
              (setf (todo-completed-at todo) nil))
             (:cancelled
              (setf (todo-status todo) +status-pending+)
              (setf (todo-completed-at todo) nil))
             (otherwise
              (setf (todo-status todo) +status-pending+)
              (setf (todo-completed-at todo) nil)))
           ;; Save but don't invalidate cache - item stays in place
           (save-todo todo)))
       (values model nil))

      ;; View details (Enter)
      ((eql key :enter)
       (when (< (model-cursor model) (length todos))
         (setf (model-view-state model) :detail))
       (values model nil))

      ;; Add new TODO (sibling at current level)
      ((and (characterp key) (char= key #\a))
       (llog:info "Add TODO triggered" :key key)
       (setf (model-view-state model) :add)
       (setf (model-edit-todo-id model) nil)
       (setf (model-edit-priority model) :medium)
       (setf (model-active-field model) :title)
       ;; Clear date and repeat fields
       (setf (model-edit-scheduled-date model) nil)
       (setf (model-edit-due-date model) nil)
       (setf (model-edit-repeat-interval model) nil)
       (setf (model-edit-repeat-unit model) nil)
       ;; Clear tags fields
       (setf (model-edit-tags model) nil)
       (tui.textinput:textinput-set-value (model-tags-input model) "")
       (setf (model-tag-dropdown-visible model) nil)
       (setf (model-tag-dropdown-cursor model) 0)
       ;; Reset inputs - clear value and set focus
       (tui.textinput:textinput-set-value (model-title-input model) "")
       (tui.textinput:textinput-set-value (model-description-input model) "")
       (tui.textinput:textinput-focus (model-title-input model))
       (tui.textinput:textinput-blur (model-description-input model))
       (llog:info "View state changed to :add" :view-state (model-view-state model))
       (values model nil))

      ;; Edit selected TODO
      ((and (characterp key) (char= key #\e))
       (when (< (model-cursor model) (length todos))
         (let ((todo (nth (model-cursor model) todos)))
           ;; Don't allow editing while enriching
           (when (todo-enriching-p todo)
             (setf (model-status-message model) "Cannot edit while enriching")
             (return-from handle-list-keys (values model nil)))
           (setf (model-view-state model) :edit)
           (setf (model-edit-todo-id model) (todo-id todo))
           (setf (model-edit-priority model) (todo-priority todo))
           (setf (model-active-field model) :title)
           ;; Pre-fill date and repeat fields
           (setf (model-edit-scheduled-date model) (todo-scheduled-date todo))
           (setf (model-edit-due-date model) (todo-due-date todo))
           (setf (model-edit-repeat-interval model) (todo-repeat-interval todo))
           (setf (model-edit-repeat-unit model) (todo-repeat-unit todo))
           ;; Pre-fill tags fields
           (setf (model-edit-tags model) (copy-list (todo-tags todo)))
           (tui.textinput:textinput-set-value (model-tags-input model) "")
           (setf (model-tag-dropdown-visible model) nil)
           (setf (model-tag-dropdown-cursor model) 0)
           ;; Pre-fill text inputs
           (tui.textinput:textinput-set-value (model-title-input model) (todo-title todo))
           (tui.textinput:textinput-set-value (model-description-input model) (or (todo-description todo) ""))
           (tui.textinput:textinput-focus (model-title-input model))
           (tui.textinput:textinput-blur (model-description-input model))))
       (values model nil))

      ;; Delete selected TODO (DEL key)
      ((eql key :delete)
       (when (< (model-cursor model) (length todos))
         (setf (model-view-state model) :delete-confirm))
       (values model nil))

      ;; Delete all DONE items (Shift+D)
      ((and (characterp key) (char= key #\D))
       (when (find-if (lambda (todo) (eq (todo-status todo) +status-completed+))
                      (model-todos model))
         (setf (model-view-state model) :delete-done-confirm))
       (values model nil))

      ;; Search
      ((and (characterp key) (char= key #\/))
       (setf (model-view-state model) :search)
       (tui.textinput:textinput-set-value (model-search-input model) (model-search-query model))
       (tui.textinput:textinput-focus (model-search-input model))
       (values model nil))

      ;; Edit user context (U) - opens external editor
      ((and (characterp key) (char= key #\U))
       (values model (make-user-context-editor-cmd)))

      ;; Filter by status (f cycles through)
      ((and (characterp key) (char= key #\f))
       (invalidate-visible-todos-cache model)
       (setf (model-filter-status model)
             (case (model-filter-status model)
               ((nil) :pending)
               (:pending :in-progress)
               (:in-progress :waiting)
               (:waiting :cancelled)
               (:cancelled :completed)
               (:completed nil)))
       (setf (model-cursor model) 0)
       (values model nil))

      ;; Sort (s cycles through)
      ((and (characterp key) (char= key #\s))
       (setf (model-sort-by model)
             (case (model-sort-by model)
               (:priority :due-date)
               (:due-date :created-at)
               (:created-at :title)
               (:title :priority)))
       (values model nil))

      ;; Set scheduled date (Shift+S) - opens modal datepicker
      ((and (characterp key) (char= key #\S))
       (when (< (model-cursor model) (length todos))
         (let* ((todo (nth (model-cursor model) todos))
                (scheduled (todo-scheduled-date todo))
                (picker (model-date-picker model)))
           ;; Initialize datepicker with current scheduled date or today
           (cond (scheduled (let ((utime (lt:timestamp-to-universal scheduled)))
                 (tui.datepicker:datepicker-set-time picker utime)
                 (setf (tui.datepicker:datepicker-selected picker) utime)))
      (t
                 (tui.datepicker:datepicker-set-time picker (get-universal-time))
                 (tui.datepicker:datepicker-unselect picker)))
           (tui.datepicker:datepicker-focus picker)
           (setf (model-editing-date-type model) :scheduled)
           (setf (model-view-state model) :list-set-date)))
       (values model nil))

      ;; Set deadline date (Shift+L) - opens modal datepicker
      ((and (characterp key) (char= key #\L))
       (when (< (model-cursor model) (length todos))
         (let* ((todo (nth (model-cursor model) todos))
                (deadline (todo-due-date todo))
                (picker (model-date-picker model)))
           ;; Initialize datepicker with current deadline or today
           (cond (deadline (let ((utime (lt:timestamp-to-universal deadline)))
                 (tui.datepicker:datepicker-set-time picker utime)
                 (setf (tui.datepicker:datepicker-selected picker) utime)))
      (t
                 (tui.datepicker:datepicker-set-time picker (get-universal-time))
                 (tui.datepicker:datepicker-unselect picker)))
           (tui.datepicker:datepicker-focus picker)
           (setf (model-editing-date-type model) :deadline)
           (setf (model-view-state model) :list-set-date)))
       (values model nil))

      ;; Refresh (r) - reload from database to pick up external changes, then reorder
      ((and (characterp key) (char= key #\r))
       (let* ((selected-id (when (< (model-cursor model) (length todos))
                             (todo-id (nth (model-cursor model) todos)))))
         ;; Reload from database to pick up externally added todos
         (setf (model-todos model) (load-todos))
         ;; Reorder after reload
         (setf (model-todos model)
               (reorder-todos (model-todos model)
                              (model-sort-by model)
                              (model-sort-descending model)))
         ;; Invalidate cache to pick up new ordering
         (invalidate-visible-todos-cache model)
         ;; Refresh tags cache in case new todos have new tags
         (refresh-tags-cache model)
         ;; Restore cursor position if possible
         (when selected-id
           (let* ((visible (get-visible-todos model))
                  (new-index (position selected-id visible :key #'todo-id :test #'string=)))
             (when new-index
               (setf (model-cursor model) new-index)))))
       (values model nil))

      ;; Clear filters (c)
      ((and (characterp key) (char= key #\c))
       (invalidate-visible-todos-cache model)
       (setf (model-filter-status model) nil)
       (setf (model-filter-priority model) nil)
       (setf (model-search-query model) "")
       (clrhash (model-selected-tags model))
       (setf (model-cursor model) 0)
       (values model nil))

      ;; Lists management (W)
      ((and (characterp key) (char= key #\W))
       (setf (model-lists-data model) (db-load-list-definitions))
       (setf (model-lists-cursor model) 0)
       (setf (model-view-state model) :lists-overview)
       (values model nil))

      ;; Help view
      ((and (characterp key) (char= key #\?))
       (setf (model-view-state model) :help)
       (values model nil))

      ;; Inline tag editor (t)
      ((and (characterp key) (char= key #\t))
       (when (< (model-cursor model) (length todos))
         (let ((todo (nth (model-cursor model) todos)))
           (setf (model-edit-todo-id model) (todo-id todo))
           (setf (model-edit-tags model) (copy-list (todo-tags todo)))
           (tui.textinput:textinput-set-value (model-tags-input model) "")
           (tui.textinput:textinput-focus (model-tags-input model))
           (setf (model-tag-dropdown-visible model) nil)
           (setf (model-tag-dropdown-cursor model) 0)
           (setf (model-view-state model) :inline-tags)))
       (values model nil))

      ;; Edit user context (u)
      ((and (characterp key) (char= key #\u))
       (llog:info "Opening user context editor")
       (let ((file (ensure-user-context-file))
             (editor (get-editor)))
         ;; Use exec-cmd for proper TUI suspension
         (values model
                 (tui:make-exec-cmd editor
                                    :args (list (namestring file))))))

      ;; Re-enrich selected TODO (&)
      ((and (characterp key) (char= key #\&))
       (when (< (model-cursor model) (length todos))
         (let ((todo (nth (model-cursor model) todos)))
           (unless (todo-enriching-p todo)
             (llog:info "Re-triggering enrichment"
                        :todo-id (todo-id todo)
                        :title (todo-title todo))
             (setf (todo-enriching-p todo) t)
             (save-todo todo)
             ;; If connected to sync server, send TODO and let server enrich it
             (cond
               ((sync-client-connected-p)
                (llog:info "Sending TODO to sync server for enrichment")
                (sync-client-send-upsert todo)
                (return-from handle-list-keys
                  (values model (list (make-spinner-start-cmd
                                       (model-enrichment-spinner model))))))
               ;; Otherwise, do local enrichment
               (t
                (return-from handle-list-keys
                  (values model (list (make-enrichment-cmd (todo-id todo)
                                                           (todo-title todo)
                                                           (todo-description todo))
                                      (make-spinner-start-cmd
                                       (model-enrichment-spinner model))))))))))
       (values model nil))

      ;; Import org-mode file (i)
      ((and (characterp key) (char= key #\i))
       (llog:info "Import org-mode triggered")
       (setf (model-view-state model) :import)
       (tui.textinput:textinput-set-value (model-import-input model) "")
       (tui.textinput:textinput-focus (model-import-input model))
       (values model nil))

      (t (values model nil)))))

;;── Add/Edit View Key Handling ─────────────────────────────────────────────────

(defun handle-add-edit-keys (model msg)
  "Handle keyboard input in add/edit view."
  (let ((key (tui:key-msg-key msg))
        (ctrl (tui:key-msg-ctrl msg)))
    (cond
      ;; Cancel with Escape
      ((eql key :escape)
       (setf (model-view-state model) :list)
       (values model nil))

      ;; Cancel with Ctrl+C
      ((and ctrl (characterp key) (char= key #\c))
       (setf (model-view-state model) :list)
       (values model nil))

      ;; Ctrl+E to edit notes in external editor
      ((and ctrl (characterp key) (char= key #\e))
       (let ((current-text (tui.textinput:textinput-value (model-description-input model))))
         (values model (make-editor-cmd current-text))))

      ;; Tab to next field: title → description → priority → scheduled → due → repeat → tags → title
      ((eql key :tab)
       (case (model-active-field model)
         (:title
          (setf (model-active-field model) :description)
          (tui.textinput:textinput-blur (model-title-input model))
          (tui.textinput:textinput-focus (model-description-input model)))
         (:description
          (setf (model-active-field model) :priority)
          (tui.textinput:textinput-blur (model-description-input model)))
         (:priority
          (setf (model-active-field model) :scheduled))
         (:scheduled
          (setf (model-active-field model) :due))
         (:due
          (setf (model-active-field model) :repeat))
         (:repeat
          (setf (model-active-field model) :tags)
          (tui.textinput:textinput-focus (model-tags-input model))
          (update-tag-dropdown model))
         (:tags
          ;; Tab in tags field: complete tag if input present, else move to next field
          (let* ((query (tui.textinput:textinput-value (model-tags-input model)))
                 (filtered (model-tag-dropdown-filtered model))
                 (cursor (model-tag-dropdown-cursor model))
                 (has-completion (or (and (model-tag-dropdown-visible model)
                                          (> (length filtered) 0))
                                     (> (length query) 0))))
            (cond (has-completion
                  (cond
                    ((and (model-tag-dropdown-visible model) (> (length filtered) 0))
                     (add-tag-to-edit-tags model (nth cursor filtered)))
                    ((> (length query) 0)
                     (add-tag-to-edit-tags model query)))
                  (update-tag-dropdown model))
      (t
                  (setf (model-active-field model) :title)
                  (tui.textinput:textinput-blur (model-tags-input model))
                  (setf (model-tag-dropdown-visible model) nil)
                  (tui.textinput:textinput-focus (model-title-input model)))))))
       (values model nil))

      ;; Shift+Tab to previous field
      ((eql key :backtab)
       (case (model-active-field model)
         (:title
          ;; Skip tags for child tasks (they inherit from parent)
          (tui.textinput:textinput-blur (model-title-input model))
          (setf (model-active-field model) :tags)
          (tui.textinput:textinput-focus (model-tags-input model))
          (update-tag-dropdown model))
         (:description
          (setf (model-active-field model) :title)
          (tui.textinput:textinput-blur (model-description-input model))
          (tui.textinput:textinput-focus (model-title-input model)))
         (:priority
          (setf (model-active-field model) :description)
          (tui.textinput:textinput-focus (model-description-input model)))
         (:scheduled
          (setf (model-active-field model) :priority))
         (:due
          (setf (model-active-field model) :scheduled))
         (:repeat
          (setf (model-active-field model) :due))
         (:tags
          (setf (model-active-field model) :repeat)
          (tui.textinput:textinput-blur (model-tags-input model))
          (setf (model-tag-dropdown-visible model) nil)))
       (values model nil))

      ;; Priority selection when in priority field (A/B/C org-style)
      ((and (eql (model-active-field model) :priority)
            (characterp key)
            (member key '(#\a #\A #\b #\B #\c #\C) :test #'char=))
       (setf (model-edit-priority model)
             (case (char-upcase key)
               (#\A :high)
               (#\B :medium)
               (#\C :low)))
       (values model nil))

      ;; Space to open datepicker for scheduled date
      ((and (eql (model-active-field model) :scheduled)
            (characterp key) (char= key #\Space))
       (let ((picker (model-date-picker model))
             (current (model-edit-scheduled-date model)))
         (cond (current (let ((utime (lt:timestamp-to-universal current)))
               (tui.datepicker:datepicker-set-time picker utime)
               (setf (tui.datepicker:datepicker-selected picker) utime)))
      (t
               (tui.datepicker:datepicker-set-time picker (get-universal-time))
               (tui.datepicker:datepicker-unselect picker)))
         (tui.datepicker:datepicker-focus picker)
         (setf (model-editing-date-type model) :scheduled)
         (setf (model-view-state model) :add-scheduled-date))
       (values model nil))

      ;; Space to open datepicker for due date
      ((and (eql (model-active-field model) :due)
            (characterp key) (char= key #\Space))
       (let ((picker (model-date-picker model))
             (current (model-edit-due-date model)))
         (cond (current (let ((utime (lt:timestamp-to-universal current)))
               (tui.datepicker:datepicker-set-time picker utime)
               (setf (tui.datepicker:datepicker-selected picker) utime)))
      (t
               (tui.datepicker:datepicker-set-time picker (get-universal-time))
               (tui.datepicker:datepicker-unselect picker)))
         (tui.datepicker:datepicker-focus picker)
         (setf (model-editing-date-type model) :due)
         (setf (model-view-state model) :add-due-date))
       (values model nil))

      ;; Backspace to clear dates
      ((and (member (model-active-field model) '(:scheduled :due))
            (eql key :backspace))
       (case (model-active-field model)
         (:scheduled (setf (model-edit-scheduled-date model) nil))
         (:due (setf (model-edit-due-date model) nil)))
       (values model nil))

      ;; Repeat field: Left/h for previous preset, Right/l for next preset
      ((and (eql (model-active-field model) :repeat)
            (or (eql key :left) (eql key :right)
                (and (characterp key) (or (char= key #\h) (char= key #\l)))))
       (let* ((presets '(nil                       ; None
                         (1 . :day)                ; Daily
                         (1 . :week)               ; Weekly
                         (2 . :week)               ; Biweekly
                         (1 . :month)              ; Monthly
                         (1 . :year)))             ; Yearly
              (current (cons (model-edit-repeat-interval model)
                             (model-edit-repeat-unit model)))
              (idx (or (position current presets :test #'equal) 0))
              (direction (if (or (eql key :left)
                                 (and (characterp key) (char= key #\h)))
                             -1 1))
              (new-idx (mod (+ idx direction) (length presets)))
              (new-val (nth new-idx presets)))
         (cond (new-val
               (setf (model-edit-repeat-interval model) (first new-val))
               (setf (model-edit-repeat-unit model) (rest new-val)))
      (t
               (setf (model-edit-repeat-interval model) nil)
               (setf (model-edit-repeat-unit model) nil))))
       (values model nil))

      ;; Tags field: Up/Down to navigate dropdown
      ((and (eql (model-active-field model) :tags)
            (or (eql key :up) (eql key :down)))
       (let* ((filtered (model-tag-dropdown-filtered model))
              (max-idx (max 0 (1- (length filtered)))))
         (when (and (model-tag-dropdown-visible model) (> (length filtered) 0))
           (if (eql key :up)
               (setf (model-tag-dropdown-cursor model)
                     (max 0 (1- (model-tag-dropdown-cursor model))))
               (setf (model-tag-dropdown-cursor model)
                     (min max-idx (1+ (model-tag-dropdown-cursor model)))))))
       (values model nil))

      ;; Tags field: Enter to select from dropdown OR create new tag
      ;; If nothing to complete, fall through to form submit
      ((and (eql (model-active-field model) :tags)
            (eql key :enter)
            (let ((query (tui.textinput:textinput-value (model-tags-input model)))
                  (filtered (model-tag-dropdown-filtered model)))
              (or (and (model-tag-dropdown-visible model) (> (length filtered) 0))
                  (> (length query) 0))))
       (let* ((query (tui.textinput:textinput-value (model-tags-input model)))
              (filtered (model-tag-dropdown-filtered model))
              (cursor (model-tag-dropdown-cursor model)))
         (cond
           ;; Dropdown visible with selection - add selected tag
           ((and (model-tag-dropdown-visible model) (> (length filtered) 0))
            (add-tag-to-edit-tags model (nth cursor filtered)))
           ;; No dropdown but has input - create new tag
           ((> (length query) 0)
            (add-tag-to-edit-tags model query)))
         (update-tag-dropdown model))
       (values model nil))

      ;; Tags field: Backspace with empty input removes last tag
      ((and (eql (model-active-field model) :tags)
            (eql key :backspace))
       (let ((query (tui.textinput:textinput-value (model-tags-input model))))
         (if (zerop (length query))
             ;; Empty input - remove last tag
             (remove-last-tag-from-edit-tags model)
             ;; Non-empty - pass to text input
             (multiple-value-bind (new-input cmd)
                 (tui.textinput:textinput-update (model-tags-input model) msg)
               (declare (ignore cmd))
               (setf (model-tags-input model) new-input)
               (update-tag-dropdown model))))
       (values model nil))

      ;; Tags field: Other keys pass to text input
      ((eql (model-active-field model) :tags)
       (multiple-value-bind (new-input cmd)
           (tui.textinput:textinput-update (model-tags-input model) msg)
         (declare (ignore cmd))
         (setf (model-tags-input model) new-input)
         (update-tag-dropdown model))
       (values model nil))

      ;; Save on Enter (when not in text field or when in priority field)
      ((eql key :enter)
       (let ((title (tui.textinput:textinput-value (model-title-input model))))
         (when (> (length title) 0)
           (if (model-edit-todo-id model)
               ;; Update existing TODO (no enrichment for edits)
               (let ((todo (find (model-edit-todo-id model) (model-todos model)
                                :key #'todo-id :test #'string=)))
                 (when todo
                   (setf (todo-title todo) title)
                   (setf (todo-description todo)
                         (let ((desc (tui.textinput:textinput-value (model-description-input model))))
                           (if (> (length desc) 0) desc nil)))
                   (setf (todo-priority todo) (model-edit-priority model))
                   ;; Save dates
                   (setf (todo-scheduled-date todo) (model-edit-scheduled-date model))
                   (setf (todo-due-date todo) (model-edit-due-date model))
                   ;; Save repeat settings
                   (setf (todo-repeat-interval todo) (model-edit-repeat-interval model))
                   (setf (todo-repeat-unit todo) (model-edit-repeat-unit model))
                   ;; Save tags
                   (setf (todo-tags todo) (reverse (model-edit-tags model)))
                   (save-todo todo)
                   ;; Refresh tags cache since tags may have changed
                   (refresh-tags-cache model))
                 (setf (model-view-state model) :list)
                 (return-from handle-add-edit-keys (values model nil)))
               ;; Create new TODO with async LLM enrichment
               (let* ((desc-input (tui.textinput:textinput-value (model-description-input model)))
                      (desc-value (when (> (length desc-input) 0) desc-input))
                      (tags (reverse (model-edit-tags model)))
                      ;; Create todo immediately with raw data, mark as enriching
                      (new-todo (make-todo title
                                          :description desc-value
                                          :priority (model-edit-priority model)
                                          :scheduled-date (model-edit-scheduled-date model)
                                          :due-date (model-edit-due-date model)
                                          :repeat-interval (model-edit-repeat-interval model)
                                          :repeat-unit (model-edit-repeat-unit model))))
                 ;; Set tags if provided
                 (when tags
                   (setf (todo-tags new-todo) tags))
                 ;; Mark as being enriched
                 (setf (todo-enriching-p new-todo) t)
                 (push new-todo (model-todos model))
                 (save-todo new-todo)
                 ;; Invalidate cache so new todo appears
                 (invalidate-visible-todos-cache model)
                 ;; Refresh tags cache since new tags may have been added
                 (refresh-tags-cache model)
                 (setf (model-view-state model) :list)
                 ;; If connected to sync server, let server enrich
                 (cond
                   ((sync-client-connected-p)
                    (llog:info "Created todo, server will enrich"
                               :todo-id (todo-id new-todo)
                               :title title)
                    (return-from handle-add-edit-keys
                      (values model (list (make-spinner-start-cmd
                                           (model-enrichment-spinner model))))))
                   ;; Otherwise, do local enrichment
                   (t
                    (llog:info "Created todo, starting local enrichment"
                               :todo-id (todo-id new-todo)
                               :title title)
                    (return-from handle-add-edit-keys
                      (values model (list (make-enrichment-cmd (todo-id new-todo)
                                                               title
                                                               desc-input)
                                          (make-spinner-start-cmd
                                           (model-enrichment-spinner model)))))))))))
       (values model nil))

      ;; Pass key to active text input
      (t
       (case (model-active-field model)
         (:title
          (multiple-value-bind (new-input cmd)
              (tui.textinput:textinput-update (model-title-input model) msg)
            (declare (ignore cmd))
            (setf (model-title-input model) new-input)))
         (:description
          (multiple-value-bind (new-input cmd)
              (tui.textinput:textinput-update (model-description-input model) msg)
            (declare (ignore cmd))
            (setf (model-description-input model) new-input))))
       (values model nil)))))

;;── Search View Key Handling ───────────────────────────────────────────────────

(defun handle-search-keys (model msg)
  "Handle keyboard input in search view."
  (let ((key (tui:key-msg-key msg)))
    (cond
      ;; Cancel with Escape
      ((eql key :escape)
       (setf (model-view-state model) :list)
       (values model nil))

      ;; Apply search with Enter
      ((eql key :enter)
       (setf (model-search-query model)
             (tui.textinput:textinput-value (model-search-input model)))
       (invalidate-visible-todos-cache model)
       (setf (model-cursor model) 0)
       (setf (model-view-state model) :list)
       (values model nil))

      ;; Pass key to search input
      (t
       (multiple-value-bind (new-input cmd)
           (tui.textinput:textinput-update (model-search-input model) msg)
         (declare (ignore cmd))
         (setf (model-search-input model) new-input)
         ;; Live search as you type
         (setf (model-search-query model)
               (tui.textinput:textinput-value (model-search-input model)))
         (invalidate-visible-todos-cache model)
         (setf (model-cursor model) 0))
       (values model nil)))))

;;── Inline Tags Editor Key Handling ────────────────────────────────────────────

(defun handle-inline-tags-keys (model msg)
  "Handle keyboard input in inline tag editor overlay."
  (let ((key (tui:key-msg-key msg)))
    (cond
      ;; Escape - save tags and close
      ((eql key :escape)
       (let ((todo (find (model-edit-todo-id model) (model-todos model)
                        :key #'todo-id :test #'string=)))
         (when todo
           (setf (todo-tags todo) (reverse (model-edit-tags model)))
           (save-todo todo)
           (refresh-tags-cache model)))
       (setf (model-view-state model) :list)
       (setf (model-edit-todo-id model) nil)
       (values model nil))

      ;; Up/Down - navigate dropdown
      ((or (eql key :up) (eql key :down))
       (let* ((filtered (model-tag-dropdown-filtered model))
              (max-idx (max 0 (1- (length filtered)))))
         (when (and (model-tag-dropdown-visible model) (> (length filtered) 0))
           (if (eql key :up)
               (setf (model-tag-dropdown-cursor model)
                     (max 0 (1- (model-tag-dropdown-cursor model))))
               (setf (model-tag-dropdown-cursor model)
                     (min max-idx (1+ (model-tag-dropdown-cursor model)))))))
       (values model nil))

      ;; Enter - add tag from dropdown or input, or save and close if empty
      ((eql key :enter)
       (let* ((query (tui.textinput:textinput-value (model-tags-input model)))
              (filtered (model-tag-dropdown-filtered model))
              (cursor (model-tag-dropdown-cursor model)))
         (cond
           ;; Dropdown visible with selection - add selected tag
           ((and (model-tag-dropdown-visible model) (> (length filtered) 0))
            (add-tag-to-edit-tags model (nth cursor filtered))
            (update-tag-dropdown model)
            (values model nil))
           ;; No dropdown but has input - create new tag
           ((> (length query) 0)
            (add-tag-to-edit-tags model query)
            (update-tag-dropdown model)
            (values model nil))
           ;; Empty input and no dropdown - save and close
           (t
            (let ((todo (find (model-edit-todo-id model) (model-todos model)
                             :key #'todo-id :test #'string=)))
              (when todo
                (setf (todo-tags todo) (reverse (model-edit-tags model)))
                (save-todo todo)
                (refresh-tags-cache model)))
            (setf (model-view-state model) :list)
            (setf (model-edit-todo-id model) nil)
            (values model nil)))))

      ;; Backspace - remove last tag if input empty, else pass to input
      ((eql key :backspace)
       (let ((query (tui.textinput:textinput-value (model-tags-input model))))
         (if (zerop (length query))
             (remove-last-tag-from-edit-tags model)
             (multiple-value-bind (new-input cmd)
                 (tui.textinput:textinput-update (model-tags-input model) msg)
               (declare (ignore cmd))
               (setf (model-tags-input model) new-input)
               (update-tag-dropdown model))))
       (values model nil))

      ;; Other keys - pass to text input
      (t
       (multiple-value-bind (new-input cmd)
           (tui.textinput:textinput-update (model-tags-input model) msg)
         (declare (ignore cmd))
         (setf (model-tags-input model) new-input)
         (update-tag-dropdown model))
       (values model nil)))))

;;── Import View Key Handling ───────────────────────────────────────────────────

(defun handle-import-keys (model msg)
  "Handle keyboard input in import view."
  (let ((key (tui:key-msg-key msg)))
    (cond
      ;; Cancel with Escape
      ((eql key :escape)
       (setf (model-view-state model) :list)
       (values model nil))

      ;; Start import with Enter
      ((eql key :enter)
       (let ((filename (tui.textinput:textinput-value (model-import-input model))))
         (cond ((and (> (length filename) 0)
                  (probe-file filename))
               (llog:info "Starting org-mode import" :filename filename)
               (setf (model-view-state model) :list)
               ;; Return command to perform async import
               (values model (list (make-import-cmd model filename)
                                   (make-spinner-start-cmd
                                    (model-enrichment-spinner model)))))
      (t
               (llog:warn "Import file not found" :filename filename)
               ;; Stay in import view - file doesn't exist
               (values model nil)))))

      ;; Pass key to import input
      (t
       (multiple-value-bind (new-input cmd)
           (tui.textinput:textinput-update (model-import-input model) msg)
         (declare (ignore cmd))
         (setf (model-import-input model) new-input))
       (values model nil)))))

;;── Delete Confirm Key Handling ────────────────────────────────────────────────

(defun handle-delete-confirm-keys (model msg)
  "Handle keyboard input in delete confirmation.
   Deletion is a status change to :deleted, not removal from database."
  (let ((key (tui:key-msg-key msg))
        (todos (get-visible-todos model)))
    (cond
      ;; Confirm delete with y
      ((and (characterp key) (char-equal key #\y))
       (when (< (model-cursor model) (length todos))
         (let* ((todo-to-delete (nth (model-cursor model) todos))
                (all-todos (model-todos model))
                ;; Get all descendants to delete as well
                (descendants (get-descendants all-todos (todo-id todo-to-delete)))
                (ids-to-delete (cons (todo-id todo-to-delete)
                                     (mapcar #'todo-id descendants)))
                (now (lt:now)))
           ;; Mark items as deleted instead of removing
           (dolist (todo all-todos)
             (when (member (todo-id todo) ids-to-delete :test #'string=)
               (setf (todo-status todo) :deleted)
               (setf (todo-completed-at todo) now)
               (save-todo todo)))
           ;; Invalidate cache so deleted items disappear
           (invalidate-visible-todos-cache model)
           (setf (model-cursor model)
                 (min (model-cursor model) (max 0 (1- (length (get-visible-todos model))))))))
       (setf (model-view-state model) :list)
       (values model nil))

      ;; Cancel with anything else
      (t
       (setf (model-view-state model) :list)
       (values model nil)))))

;;── Delete DONE Confirm Key Handling ──────────────────────────────────────────

(defun handle-delete-done-confirm-keys (model msg)
  "Handle keyboard input in delete-done confirmation.
   Marks all completed items as :deleted instead of removing."
  (let ((key (tui:key-msg-key msg)))
    (cond
      ;; Confirm delete with y
      ((and (characterp key) (char-equal key #\y))
       (let ((now (lt:now)))
         (dolist (todo (model-todos model))
           (when (eq (todo-status todo) +status-completed+)
             (setf (todo-status todo) :deleted)
             (setf (todo-completed-at todo) now)
             (save-todo todo))))
       ;; Invalidate cache so deleted items disappear
       (invalidate-visible-todos-cache model)
       (setf (model-cursor model)
             (min (model-cursor model) (max 0 (1- (length (get-visible-todos model))))))
       (setf (model-view-state model) :list)
       (values model nil))

      ;; Cancel with anything else
      (t
       (setf (model-view-state model) :list)
       (values model nil)))))

;;── Delete Tag Confirm Key Handling ────────────────────────────────────────────

(defun handle-delete-tag-confirm-keys (model msg)
  "Handle keyboard input in delete tag confirmation.
   Removes the tag from all TODOs and updates the sidebar."
  (let ((key (tui:key-msg-key msg))
        (tag (model-deleting-tag model)))
    (cond
      ;; Confirm delete with y
      ((and (characterp key) (char-equal key #\y))
       (when tag
         ;; Remove tag from affected todos and save each
         (dolist (todo (model-todos model))
           (when (member tag (todo-tags todo) :test #'string=)
             (setf (todo-tags todo)
                   (remove tag (todo-tags todo) :test #'string=))
             (save-todo todo)))
         ;; Remove from selected-tags filter if present
         (remhash tag (model-selected-tags model))
         ;; Invalidate caches
         (invalidate-visible-todos-cache model)
         (setf (model-all-tags-cache model) nil)
         ;; Adjust sidebar cursor if needed
         (let ((tags (update-all-tags-cache model)))
           (when (> (model-sidebar-cursor model) (length tags))
             (setf (model-sidebar-cursor model) (max 0 (length tags))))))
       (setf (model-deleting-tag model) nil)
       (setf (model-view-state model) :list)
       (values model nil))

      ;; Cancel with anything else
      (t
       (setf (model-deleting-tag model) nil)
       (setf (model-view-state model) :list)
       (values model nil)))))

;;── Detail View Key Handling ───────────────────────────────────────────────────

(defun handle-detail-keys (model msg)
  "Handle keyboard input in detail view."
  (let ((key (tui:key-msg-key msg))
        (todos (get-visible-todos model)))
    (cond
      ;; Back to list with Escape, q, or Enter
      ((or (eql key :escape)
           (and (characterp key) (char= key #\q))
           (eql key :enter))
       (setf (model-view-state model) :list)
       (values model nil))

      ;; Edit from detail view
      ((and (characterp key) (char= key #\e))
       (when (< (model-cursor model) (length todos))
         (let ((todo (nth (model-cursor model) todos)))
           (setf (model-view-state model) :edit)
           (setf (model-edit-todo-id model) (todo-id todo))
           (setf (model-edit-priority model) (todo-priority todo))
           (setf (model-active-field model) :title)
           ;; Pre-fill date and repeat fields
           (setf (model-edit-scheduled-date model) (todo-scheduled-date todo))
           (setf (model-edit-due-date model) (todo-due-date todo))
           (setf (model-edit-repeat-interval model) (todo-repeat-interval todo))
           (setf (model-edit-repeat-unit model) (todo-repeat-unit todo))
           ;; Pre-fill text inputs
           (tui.textinput:textinput-set-value (model-title-input model) (todo-title todo))
           (tui.textinput:textinput-set-value (model-description-input model) (or (todo-description todo) ""))
           (tui.textinput:textinput-focus (model-title-input model))
           (tui.textinput:textinput-blur (model-description-input model))))
       (values model nil))

      ;; Edit scheduled date (s)
      ((and (characterp key) (char= key #\s))
       (when (< (model-cursor model) (length todos))
         (let* ((todo (nth (model-cursor model) todos))
                (scheduled (todo-scheduled-date todo))
                (picker (model-date-picker model)))
           ;; Initialize datepicker with current scheduled date or today
           (cond (scheduled (let ((utime (lt:timestamp-to-universal scheduled)))
                 (tui.datepicker:datepicker-set-time picker utime)
                 (setf (tui.datepicker:datepicker-selected picker) utime)))
      (t
                 (tui.datepicker:datepicker-set-time picker (get-universal-time))
                 (tui.datepicker:datepicker-unselect picker)))
           (tui.datepicker:datepicker-focus picker)
           (setf (model-editing-date-type model) :scheduled)
           (setf (model-view-state model) :edit-date)))
       (values model nil))

      ;; Edit deadline date (d)
      ((and (characterp key) (char= key #\d))
       (when (< (model-cursor model) (length todos))
         (let* ((todo (nth (model-cursor model) todos))
                (deadline (todo-due-date todo))
                (picker (model-date-picker model)))
           ;; Initialize datepicker with current deadline or today
           (cond (deadline (let ((utime (lt:timestamp-to-universal deadline)))
                 (tui.datepicker:datepicker-set-time picker utime)
                 (setf (tui.datepicker:datepicker-selected picker) utime)))
      (t
                 (tui.datepicker:datepicker-set-time picker (get-universal-time))
                 (tui.datepicker:datepicker-unselect picker)))
           (tui.datepicker:datepicker-focus picker)
           (setf (model-editing-date-type model) :deadline)
           (setf (model-view-state model) :edit-date)))
       (values model nil))

      ;; Open URL with 'o'
      ((and (characterp key) (char= key #\o))
       (when (< (model-cursor model) (length todos))
         (let* ((todo (nth (model-cursor model) todos))
                (urls (get-todo-urls todo)))
           (when urls
             (open-url (first urls)))))
       (values model nil))

      ;; Open photo/attachment with 'p'
      ((and (characterp key) (char= key #\p))
       (when (< (model-cursor model) (length todos))
         (let* ((todo (nth (model-cursor model) todos))
                (hashes (todo-attachment-hashes todo)))
           (when hashes
             (open-attachment (first hashes)))))
       (values model nil))

      ;; Edit notes with 'n' - opens external editor
      ((and (characterp key) (char= key #\n))
       (when (< (model-cursor model) (length todos))
         (let* ((todo (nth (model-cursor model) todos))
                (current-notes (or (todo-description todo) "")))
           ;; Store the todo id so we know which todo to update
           (setf (model-edit-todo-id model) (todo-id todo))
           (values model (make-detail-notes-editor-cmd (todo-id todo) current-notes))))
       (values model nil))

      (t (values model nil)))))

;;── List Date Modal Key Handling ───────────────────────────────────────────────

(defun handle-list-date-keys (model msg)
  "Handle keyboard input in list view date modal."
  (let ((key (tui:key-msg-key msg))
        (ctrl (tui:key-msg-ctrl msg))
        (todos (get-visible-todos model))
        (picker (model-date-picker model)))
    (cond
      ;; Cancel with Escape or Ctrl+C - go back to list view
      ((or (eql key :escape)
           (and ctrl (characterp key) (char= key #\c)))
       (setf (model-view-state model) :list)
       (values model nil))

      ;; Confirm with Enter - save the current cursor date and go back to list
      ((eql key :enter)
       (when (< (model-cursor model) (length todos))
         (let* ((todo (nth (model-cursor model) todos))
                ;; Use cursor position (datepicker-time), not selection
                (current-date (tui.datepicker:datepicker-time picker))
                (timestamp (when current-date
                             (lt:universal-to-timestamp current-date))))
           ;; Save the date based on which type we're editing
           (case (model-editing-date-type model)
             (:scheduled
              (setf (todo-scheduled-date todo) timestamp))
             (:deadline
              (setf (todo-due-date todo) timestamp)))
           (save-todo todo)
           ;; Invalidate cache since dates affect grouping
           (invalidate-visible-todos-cache model)))
       (setf (model-view-state model) :list)
       (values model nil))

      ;; Clear date with Backspace or Delete
      ((or (eql key :backspace) (eql key :delete))
       (when (< (model-cursor model) (length todos))
         (let ((todo (nth (model-cursor model) todos)))
           (case (model-editing-date-type model)
             (:scheduled
              (setf (todo-scheduled-date todo) nil))
             (:deadline
              (setf (todo-due-date todo) nil)))
           (save-todo todo)
           ;; Invalidate cache since dates affect grouping
           (invalidate-visible-todos-cache model)))
       (setf (model-view-state model) :list)
       (values model nil))

      ;; Pass other keys to datepicker
      (t
       (multiple-value-bind (new-picker cmd)
           (tui.datepicker:datepicker-update picker msg)
         (setf (model-date-picker model) new-picker)
         (values model cmd))))))

;;── Date Edit View Key Handling ────────────────────────────────────────────────

(defun handle-date-edit-keys (model msg)
  "Handle keyboard input in date editing view."
  (let ((key (tui:key-msg-key msg))
        (ctrl (tui:key-msg-ctrl msg))
        (todos (get-visible-todos model))
        (picker (model-date-picker model)))
    (cond
      ;; Cancel with Escape or Ctrl+C - go back to detail view
      ((or (eql key :escape)
           (and ctrl (characterp key) (char= key #\c)))
       (setf (model-view-state model) :detail)
       (values model nil))

      ;; Confirm with Enter - save the cursor date and go back
      ((eql key :enter)
       (when (< (model-cursor model) (length todos))
         (let* ((todo (nth (model-cursor model) todos))
                (current-date (tui.datepicker:datepicker-time picker))
                (timestamp (when current-date
                             (lt:universal-to-timestamp current-date))))
           ;; Save the date based on which type we're editing
           (case (model-editing-date-type model)
             (:scheduled
              (setf (todo-scheduled-date todo) timestamp))
             (:deadline
              (setf (todo-due-date todo) timestamp)))
           (save-todo todo)))
       (setf (model-view-state model) :detail)
       (values model nil))

      ;; Clear date with Backspace or Delete
      ((or (eql key :backspace) (eql key :delete))
       (when (< (model-cursor model) (length todos))
         (let ((todo (nth (model-cursor model) todos)))
           (case (model-editing-date-type model)
             (:scheduled
              (setf (todo-scheduled-date todo) nil))
             (:deadline
              (setf (todo-due-date todo) nil)))
           (save-todo todo)))
       (setf (model-view-state model) :detail)
       (values model nil))

      ;; Pass other keys to datepicker
      (t
       (multiple-value-bind (new-picker cmd)
           (tui.datepicker:datepicker-update picker msg)
         (setf (model-date-picker model) new-picker)
         (values model cmd))))))

(defun handle-form-date-edit-keys (model msg)
  "Handle keyboard input in add/edit form date picker view."
  (let* ((key (tui:key-msg-key msg))
         (ctrl (tui:key-msg-ctrl msg))
         (picker (model-date-picker model))
         (return-view (if (model-edit-todo-id model) :edit :add))
         (date-type (model-editing-date-type model)))
    (cond
      ;; Cancel with Escape or Ctrl+C - go back to add/edit view without saving
      ((or (eql key :escape)
           (and ctrl (characterp key) (char= key #\c)))
       (setf (model-view-state model) return-view)
       (setf (model-active-field model) (if (eql date-type :scheduled) :scheduled :due))
       (values model nil))

      ;; Confirm with Enter - save the cursor date to form state and go back
      ((eql key :enter)
       (let* ((current-date (when picker (tui.datepicker:datepicker-time picker)))
              (timestamp (when current-date
                           (lt:universal-to-timestamp current-date))))
         ;; Save the date to form state based on which type we're editing
         (cond
           ((eql date-type :scheduled)
            (setf (model-edit-scheduled-date model) timestamp)
            (setf (model-active-field model) :scheduled))
           ((eql date-type :due)
            (setf (model-edit-due-date model) timestamp)
            (setf (model-active-field model) :due))))
       (setf (model-view-state model) return-view)
       (values model nil))

      ;; Clear date with Backspace or Delete
      ((or (eql key :backspace) (eql key :delete))
       (cond
         ((eql date-type :scheduled)
          (setf (model-edit-scheduled-date model) nil)
          (setf (model-active-field model) :scheduled))
         ((eql date-type :due)
          (setf (model-edit-due-date model) nil)
          (setf (model-active-field model) :due)))
       (setf (model-view-state model) return-view)
       (values model nil))

      ;; Pass other keys to datepicker
      (t
       (when picker
         (multiple-value-bind (new-picker cmd)
             (tui.datepicker:datepicker-update picker msg)
           (setf (model-date-picker model) new-picker)
           (return-from handle-form-date-edit-keys (values model cmd))))
       (values model nil)))))

;;── Help View Key Handling ─────────────────────────────────────────────────────

(defun handle-help-keys (model msg)
  "Handle keyboard input in help view."
  (declare (ignore msg))
  ;; Any key returns to list view
  (setf (model-view-state model) :list)
  (values model nil))

;;── Context Info View Key Handling ────────────────────────────────────────────

(defun handle-context-info-keys (model msg)
  "Handle keyboard input in context info view."
  (declare (ignore msg))
  ;; Any key returns to list view
  (setf (model-view-state model) :list)
  (values model nil))

;;── Lists Overview Key Handling ────────────────────────────────────────────────

(defun reload-lists-data (model)
  "Reload list definitions from DB into model."
  (setf (model-lists-data model) (db-load-list-definitions))
  (setf (model-lists-cursor model)
        (min (model-lists-cursor model)
             (max 0 (1- (length (model-lists-data model)))))))

(defun handle-lists-overview-keys (model msg)
  "Handle keyboard input in lists overview view."
  (let ((key (tui:key-msg-key msg))
        (lists (model-lists-data model)))
    (cond
      ;; Quit / back to main
      ((or (eql key :escape)
           (and (characterp key) (char= key #\q)))
       (setf (model-view-state model) :list)
       (values model nil))

      ;; Navigate up
      ((or (eql key :up) (and (characterp key) (member key '(#\k #\p))))
       (when (> (model-lists-cursor model) 0)
         (decf (model-lists-cursor model)))
       (values model nil))

      ;; Navigate down
      ((or (eql key :down) (and (characterp key) (member key '(#\j #\n))))
       (when (< (model-lists-cursor model) (1- (length lists)))
         (incf (model-lists-cursor model)))
       (values model nil))

      ;; Enter - open list detail
      ((eql key :enter)
       (when (and lists (< (model-lists-cursor model) (length lists)))
         (let ((list-def (nth (model-lists-cursor model) lists)))
           (setf (model-list-detail-def model) list-def)
           (setf (model-list-detail-items model) (db-load-list-items (list-def-id list-def)))
           (setf (model-list-detail-cursor model) 0)
           (setf (model-list-detail-scroll model) 0)
           (setf (model-view-state model) :list-detail)))
       (values model nil))

      ;; Create new list
      ((and (characterp key) (char= key #\a))
       (unless (model-list-form-name-input model)
         (setf (model-list-form-name-input model) (tui.textinput:make-textinput :placeholder "List name" :width 40))
         (setf (model-list-form-desc-input model) (tui.textinput:make-textinput :placeholder "Description (LLM hint)" :width 40))
         (setf (model-list-form-sections-input model) (tui.textinput:make-textinput :placeholder "Produce, Dairy, ..." :width 40)))
       (tui.textinput:textinput-set-value (model-list-form-name-input model) "")
       (tui.textinput:textinput-set-value (model-list-form-desc-input model) "")
       (tui.textinput:textinput-set-value (model-list-form-sections-input model) "")
       (tui.textinput:textinput-focus (model-list-form-name-input model))
       (setf (model-list-form-active-field model) :name)
       (setf (model-list-form-editing-id model) nil)
       (setf (model-view-state model) :list-create)
       (values model nil))

      ;; Edit selected list
      ((and (characterp key) (char= key #\e))
       (when (and lists (< (model-lists-cursor model) (length lists)))
         (let ((list-def (nth (model-lists-cursor model) lists)))
           (unless (model-list-form-name-input model)
             (setf (model-list-form-name-input model) (tui.textinput:make-textinput :placeholder "List name" :width 40))
             (setf (model-list-form-desc-input model) (tui.textinput:make-textinput :placeholder "Description" :width 40))
             (setf (model-list-form-sections-input model) (tui.textinput:make-textinput :placeholder "Sections" :width 40)))
           (tui.textinput:textinput-set-value (model-list-form-name-input model) (list-def-name list-def))
           (tui.textinput:textinput-set-value (model-list-form-desc-input model) (or (list-def-description list-def) ""))
           (tui.textinput:textinput-set-value (model-list-form-sections-input model)
                                               (format nil "~{~A~^, ~}" (or (list-def-sections list-def) '())))
           (tui.textinput:textinput-focus (model-list-form-name-input model))
           (setf (model-list-form-active-field model) :name)
           (setf (model-list-form-editing-id model) (list-def-id list-def))
           (setf (model-view-state model) :list-edit)))
       (values model nil))

      ;; Delete selected list
      ((or (eql key :delete) (and (characterp key) (char= key #\d)))
       (when (and lists (< (model-lists-cursor model) (length lists)))
         (setf (model-view-state model) :list-delete-confirm))
       (values model nil))

      (t (values model nil)))))

;;── List Detail Key Handling ──────────────────────────────────────────────────

(defun build-list-detail-flat-items (list-def items)
  "Build a flat list of (:section name) or (:item list-item) entries for rendering.
   Items are grouped by section in defined order, then unsectioned items at end."
  (let ((sections (list-def-sections list-def))
        (section-items (make-hash-table :test #'equal))
        (no-section nil)
        (result nil))
    (dolist (item items)
      (let ((sec (list-item-section item)))
        (if (and sec (> (length sec) 0))
            (push item (gethash sec section-items nil))
            (push item no-section))))
    ;; Add sections in defined order
    (dolist (sec sections)
      (let ((sec-items (nreverse (gethash sec section-items nil))))
        (when sec-items
          (push (list :section sec) result)
          (dolist (item sec-items)
            (push (list :item item) result)))))
    ;; Add unsectioned items
    (when no-section
      (push (list :section "Other") result)
      (dolist (item (nreverse no-section))
        (push (list :item item) result)))
    (nreverse result)))

(defun handle-list-detail-keys (model msg)
  "Handle keyboard input in list detail view."
  (let ((key (tui:key-msg-key msg))
        (items (model-list-detail-items model))
        (list-def (model-list-detail-def model)))
    (cond
      ;; Back to lists overview
      ((or (eql key :escape) (and (characterp key) (char= key #\q)))
       (reload-lists-data model)
       (setf (model-view-state model) :lists-overview)
       (values model nil))

      ;; Navigate up
      ((or (eql key :up) (and (characterp key) (member key '(#\k #\p))))
       (when (> (model-list-detail-cursor model) 0)
         (decf (model-list-detail-cursor model)))
       (values model nil))

      ;; Navigate down
      ((or (eql key :down) (and (characterp key) (member key '(#\j #\n))))
       (when (< (model-list-detail-cursor model) (1- (length items)))
         (incf (model-list-detail-cursor model)))
       (values model nil))

      ;; Toggle checked (Space)
      ((and (characterp key) (char= key #\Space))
       (when (and items (< (model-list-detail-cursor model) (length items)))
         (let ((item (nth (model-list-detail-cursor model) items)))
           (db-check-list-item (list-item-id item) (not (list-item-checked item)))
           (setf (model-list-detail-items model) (db-load-list-items (list-def-id list-def)))))
       (values model nil))

      ;; Add item
      ((and (characterp key) (char= key #\a))
       (unless (model-list-item-add-input model)
         (setf (model-list-item-add-input model) (tui.textinput:make-textinput :placeholder "Item title" :width 40)))
       (tui.textinput:textinput-set-value (model-list-item-add-input model) "")
       (tui.textinput:textinput-focus (model-list-item-add-input model))
       (setf (model-view-state model) :list-item-add)
       (values model nil))

      ;; Delete item
      ((or (eql key :delete) (and (characterp key) (char= key #\d)))
       (when (and items (< (model-list-detail-cursor model) (length items)))
         (let ((item (nth (model-list-detail-cursor model) items)))
           (db-delete-list-item (list-item-id item))
           (setf (model-list-detail-items model) (db-load-list-items (list-def-id list-def)))
           (setf (model-list-detail-cursor model)
                 (min (model-list-detail-cursor model)
                      (max 0 (1- (length (model-list-detail-items model))))))))
       (values model nil))

      ;; Share / export list
      ((and (characterp key) (char= key #\s))
       (when list-def
         (let ((text (with-output-to-string (s)
                       (export-list list-def items :stream s))))
           ;; Copy to clipboard if possible
           (handler-case
               (let ((proc (uiop:launch-program '("xclip" "-selection" "clipboard")
                                                 :input :stream)))
                 (write-string text (uiop:process-info-input proc))
                 (close (uiop:process-info-input proc))
                 (setf (model-status-message model) "List copied to clipboard"))
             (error ()
               (setf (model-status-message model) "Could not copy (xclip not found)")))))
       (values model nil))

      (t (values model nil)))))

;;── List Form Key Handling ────────────────────────────────────────────────────

(defun handle-list-form-keys (model msg)
  "Handle keyboard input in list create/edit form."
  (let ((key (tui:key-msg-key msg))
        (ctrl (tui:key-msg-ctrl msg)))
    (cond
      ;; Cancel
      ((or (and ctrl (characterp key) (char= key #\c))
           (eql key :escape))
       (reload-lists-data model)
       (setf (model-view-state model) :lists-overview)
       (values model nil))

      ;; Tab - cycle fields
      ((eql key :tab)
       (let ((fields '(:name :desc :sections :submit)))
         (setf (model-list-form-active-field model)
               (let* ((current (model-list-form-active-field model))
                      (pos (position current fields))
                      (next (mod (1+ (or pos 0)) (length fields))))
                 (nth next fields))))
       ;; Manage focus
       (case (model-list-form-active-field model)
         (:name (tui.textinput:textinput-focus (model-list-form-name-input model))
                (tui.textinput:textinput-blur (model-list-form-desc-input model))
                (tui.textinput:textinput-blur (model-list-form-sections-input model)))
         (:desc (tui.textinput:textinput-blur (model-list-form-name-input model))
                (tui.textinput:textinput-focus (model-list-form-desc-input model))
                (tui.textinput:textinput-blur (model-list-form-sections-input model)))
         (:sections (tui.textinput:textinput-blur (model-list-form-name-input model))
                    (tui.textinput:textinput-blur (model-list-form-desc-input model))
                    (tui.textinput:textinput-focus (model-list-form-sections-input model)))
         (:submit (tui.textinput:textinput-blur (model-list-form-name-input model))
                  (tui.textinput:textinput-blur (model-list-form-desc-input model))
                  (tui.textinput:textinput-blur (model-list-form-sections-input model))))
       (values model nil))

      ;; Enter on submit field - save
      ((and (eql key :enter) (eql (model-list-form-active-field model) :submit))
       (let* ((name (str:trim (tui.textinput:textinput-value (model-list-form-name-input model))))
              (desc (str:trim (tui.textinput:textinput-value (model-list-form-desc-input model))))
              (sections-str (tui.textinput:textinput-value (model-list-form-sections-input model)))
              (sections (when (> (length sections-str) 0)
                          (mapcar (lambda (s) (str:trim s))
                                  (str:split #\, sections-str)))))
         (when (> (length name) 0)
           (if (model-list-form-editing-id model)
               ;; Edit existing list
               (let* ((existing (db-find-list-by-id (model-list-form-editing-id model)))
                      (updated (make-instance 'list-definition
                                              :id (list-def-id existing)
                                              :name name
                                              :description (when (> (length desc) 0) desc)
                                              :sections sections
                                              :created-at (list-def-created-at existing))))
                 (db-save-list-definition updated)
                 (setf (model-status-message model)
                       (format nil "Updated list: ~A" name)))
               ;; Create new list
               (let ((list-def (make-list-definition name
                                                     :description (when (> (length desc) 0) desc)
                                                     :sections sections)))
                 (db-save-list-definition list-def)
                 (setf (model-status-message model)
                       (format nil "Created list: ~A" name))))))
       (reload-lists-data model)
       (setf (model-view-state model) :lists-overview)
       (values model nil))

      ;; Pass to active text input
      (t
       (case (model-list-form-active-field model)
         (:name (tui.textinput:textinput-update (model-list-form-name-input model) msg))
         (:desc (tui.textinput:textinput-update (model-list-form-desc-input model) msg))
         (:sections (tui.textinput:textinput-update (model-list-form-sections-input model) msg)))
       (values model nil)))))

;;── List Item Add Key Handling ────────────────────────────────────────────────

(defun handle-list-item-add-keys (model msg)
  "Handle keyboard input when adding an item to a list."
  (let ((key (tui:key-msg-key msg))
        (list-def (model-list-detail-def model)))
    (cond
      ;; Cancel
      ((eql key :escape)
       (setf (model-view-state model) :list-detail)
       (values model nil))

      ;; Submit
      ((eql key :enter)
       (let ((title (str:trim (tui.textinput:textinput-value (model-list-item-add-input model)))))
         (when (and (> (length title) 0) list-def)
           (let ((item (make-list-item (list-def-id list-def) title)))
             (db-save-list-item item)
             (setf (model-list-detail-items model) (db-load-list-items (list-def-id list-def))))))
       (setf (model-view-state model) :list-detail)
       (values model nil))

      ;; Pass to text input
      (t
       (tui.textinput:textinput-update (model-list-item-add-input model) msg)
       (values model nil)))))

;;── List Delete Confirm Key Handling ──────────────────────────────────────────

(defun handle-list-delete-confirm-keys (model msg)
  "Handle confirmation for deleting a list."
  (let ((key (tui:key-msg-key msg)))
    (when (and (characterp key) (char= key #\y))
      (let* ((lists (model-lists-data model))
             (list-def (nth (model-lists-cursor model) lists)))
        (when list-def
          (db-delete-list-definition (list-def-id list-def))
          (setf (model-status-message model)
                (format nil "Deleted list: ~A" (list-def-name list-def))))))
    (reload-lists-data model)
    (setf (model-view-state model) :lists-overview)
    (values model nil)))

;;── Main Update Dispatch ───────────────────────────────────────────────────────

(defmethod tui:update-message ((model app-model) (msg tui:key-msg))
  "Handle key messages based on current view state."
  (case (model-view-state model)
    (:list (handle-list-keys model msg))
    ((:add :edit) (handle-add-edit-keys model msg))
    (:search (handle-search-keys model msg))
    (:inline-tags (handle-inline-tags-keys model msg))
    (:import (handle-import-keys model msg))
    (:delete-confirm (handle-delete-confirm-keys model msg))
    (:delete-done-confirm (handle-delete-done-confirm-keys model msg))
    (:delete-tag-confirm (handle-delete-tag-confirm-keys model msg))
    (:detail (handle-detail-keys model msg))
    (:edit-date (handle-date-edit-keys model msg))
    (:list-set-date (handle-list-date-keys model msg))
    ((:add-scheduled-date :add-due-date) (handle-form-date-edit-keys model msg))
    (:help (handle-help-keys model msg))
    (:context-info (handle-context-info-keys model msg))
    (:lists-overview (handle-lists-overview-keys model msg))
    (:list-detail (handle-list-detail-keys model msg))
    ((:list-create :list-edit) (handle-list-form-keys model msg))
    (:list-item-add (handle-list-item-add-keys model msg))
    (:list-delete-confirm (handle-list-delete-confirm-keys model msg))
    (otherwise (values model nil))))

(defmethod tui:update-message ((model app-model) (msg tui:window-size-msg))
  "Handle window resize."
  (setf (model-term-width model) (tui:window-size-msg-width msg))
  (setf (model-term-height model) (tui:window-size-msg-height msg))
  (values model nil))

(defmethod tui:update-message ((model app-model) (msg sync-refresh-msg))
  "Handle sync refresh - model already updated, just trigger redraw."
  (values model nil))

(defmethod tui:update-message ((model app-model) (msg sync-reload-msg))
  "Handle sync reload - reload todos and lists from database and trigger redraw."
  (setf (model-todos model) (load-todos))
  (setf (model-visible-todos-dirty model) t)
  (reload-lists-data model)
  (values model nil))

(defmethod tui:update-message ((model app-model) (msg tui:mouse-scroll-event))
  "Handle mouse scroll by scrolling the viewport."
  (when (eql (model-view-state model) :list)
    (let* ((direction (tui:mouse-scroll-direction msg))
           (count (tui:mouse-scroll-count msg))
           (todos (get-visible-todos model))
           (groups (group-todos-by-date todos))
           (num-lines (max 1 (+ (length todos) (* (length groups) +header-lines+))))
           ;; Account for filter banner height
           (has-filters (has-active-filters-p model))
           (filter-banner-height (if has-filters 1 0))
           (viewport-height (max 5 (- (model-term-height model) 3 filter-banner-height)))
           (max-offset (max 0 (- num-lines viewport-height)))
           (offset (model-scroll-offset model))
           (new-offset (case direction
                         (:up (max 0 (- offset count)))
                         (:down (min max-offset (+ offset count)))
                         (otherwise offset))))
      (setf (model-scroll-offset model) new-offset)
      ;; Move cursor into visible viewport so adjust-scroll won't override
      (let* ((cursor (model-cursor model))
             (cursor-line (list-line-index-for-cursor groups cursor)))
        (cond
          ;; Cursor is above viewport - move to first visible todo
          ((< cursor-line new-offset)
           (loop for line from new-offset below (+ new-offset viewport-height)
                 for idx = (cursor-index-for-line groups line)
                 when idx do (setf (model-cursor model) idx)
                             (return)))
          ;; Cursor is below viewport - move to last visible todo
          ((>= cursor-line (+ new-offset viewport-height))
           (loop for line from (+ new-offset viewport-height -1) downto new-offset
                 for idx = (cursor-index-for-line groups line)
                 when idx do (setf (model-cursor model) idx)
                             (return)))))))
  (values model nil))

(defmethod tui:update-message ((model app-model) (msg tui:mouse-press-event))
  "Handle mouse clicks in list and detail views."
  (let ((view-state (model-view-state model))
        (button (tui:mouse-event-button msg)))
    (cond
      ;; List view click handling
      ((eql view-state :list)
       (let* ((screen-x (1- (tui:mouse-event-x msg)))  ; Convert to 0-based
              ;; Account for filter banner height (same as render-list-view)
              (has-filters (has-active-filters-p model))
              (filter-banner-height (if has-filters 1 0))
              (screen-line (- (tui:mouse-event-y msg) 2))
              (list-line (- screen-line filter-banner-height))
              (available-height (max 5 (- (model-term-height model) 3 filter-banner-height)))
              (term-width (model-term-width model))
              (scrollbar-col (1- term-width))  ; Rightmost column
              ;; Calculate sidebar width (same logic as render-list-view)
              (sidebar-visible (model-sidebar-visible model))
              (min-list-width 20)
              (raw-sidebar-width (if sidebar-visible 14 0))
              (sidebar-width (if sidebar-visible
                                 (max 0 (min raw-sidebar-width (- term-width min-list-width 2)))
                                 0))
              (sidebar-visible-effective (> sidebar-width 0)))
         (cond
           ;; Click in sidebar area
           ((and sidebar-visible-effective (< screen-x sidebar-width)
                 (eql button :left))
            ;; Sidebar rows: 0=header, 1=separator, 2="All", 3+=tags
            (let* ((sidebar-item (- screen-line 2))
                   (tags (model-all-tags-cache model))
                   (max-cursor (length tags)))
              (when (and (>= sidebar-item 0) (<= sidebar-item max-cursor))
                ;; Focus sidebar, set cursor, and toggle the tag
                (setf (model-sidebar-focused model) t
                      (model-sidebar-cursor model) sidebar-item)
                (let ((selected (model-selected-tags model)))
                  (if (zerop sidebar-item)
                      ;; "All" - clear all filters
                      (clrhash selected)
                      ;; Toggle specific tag
                      (let ((tag (nth (1- sidebar-item) tags)))
                        (when tag
                          (if (gethash tag selected)
                              (remhash tag selected)
                              (setf (gethash tag selected) t))))))
                (invalidate-visible-todos-cache model)
                (setf (model-cursor model) 0))))
           ;; Left-click on scrollbar: start dragging
           ((and (>= list-line 0) (< list-line available-height)
                 (eql button :left) (= screen-x scrollbar-col))
            (setf (model-scrollbar-dragging model) t)
            (scroll-to-y-position model list-line available-height))
           ;; Left-click on list: select item
           ((and (>= list-line 0) (< list-line available-height)
                 (eql button :left))
            (let* ((line-idx (+ (model-scroll-offset model) list-line))
                   (groups (get-visible-todos-grouped model))
                   (cursor (cursor-index-for-line groups line-idx)))
              (when cursor
                (setf (model-cursor model) cursor
                      (model-sidebar-focused model) nil))))
           ;; Right-click on list: select item and show details
           ((and (>= list-line 0) (< list-line available-height)
                 (eql button :right))
            (let* ((line-idx (+ (model-scroll-offset model) list-line))
                   (groups (get-visible-todos-grouped model))
                   (cursor (cursor-index-for-line groups line-idx)))
              (when cursor
                (setf (model-cursor model) cursor
                      (model-sidebar-focused model) nil
                      (model-view-state model) :detail)))))))
      ;; Detail view: click on URL to open it
      ;; URLs appear in the lower section; we check Y position and URL presence
      ((eql view-state :detail)
       (when (eql button :left)
         (let* ((screen-y (- (tui:mouse-event-y msg) 2))
                (todos (get-visible-todos model))
                (todo (when (< (model-cursor model) (length todos))
                        (nth (model-cursor model) todos))))
           (when todo
             (let ((urls (get-todo-urls todo)))
               ;; Only respond to clicks in the lower portion where URLs appear
               ;; (roughly after the basic info section, line 8+)
               (when (and urls (>= screen-y 8))
                 (open-url (first urls))))))))))
  (values model nil))

(defmethod tui:update-message ((model app-model) (msg tui:mouse-drag-event))
  "Handle mouse drag for scrollbar."
  (when (and (eql (model-view-state model) :list)
             (model-scrollbar-dragging model))
    (let* ((has-filters (has-active-filters-p model))
           (filter-banner-height (if has-filters 1 0))
           (screen-line (- (tui:mouse-event-y msg) 2))
           (list-line (- screen-line filter-banner-height))
           (available-height (max 5 (- (model-term-height model) 3 filter-banner-height))))
      (scroll-to-y-position model (max 0 (min (1- available-height) list-line)) available-height)))
  (values model nil))

(defmethod tui:update-message ((model app-model) (msg tui:mouse-release-event))
  "Handle mouse release to stop scrollbar dragging."
  (setf (model-scrollbar-dragging model) nil)
  (values model nil))

(defmethod tui:update-message ((model app-model) (msg tui.spinner:spinner-tick-msg))
  "Handle spinner tick messages to animate the spinner."
  (let ((spinner (model-enrichment-spinner model)))
    (when spinner
      (multiple-value-bind (updated-spinner cmd)
          (tui.spinner:spinner-update spinner msg)
        (setf (model-enrichment-spinner model) updated-spinner)
        ;; Only continue ticking if there are enriching items
        (if (some #'todo-enriching-p (model-todos model))
            (values model cmd)
            (values model nil))))))

;;── Enrichment Message ────────────────────────────────────────────────────────

(defclass enrichment-complete-msg ()
  ((todo-id
    :initarg :todo-id
    :accessor enrichment-msg-todo-id
    :documentation "ID of the TODO that was enriched.")
   (enriched-data
    :initarg :enriched-data
    :accessor enrichment-msg-data
    :documentation "Plist with enriched fields, or NIL if enrichment failed."))
  (:documentation "Message sent when async enrichment completes."))

(defmethod tui:update-message ((model app-model) (msg enrichment-complete-msg))
  "Handle enrichment completion by updating the TODO with enriched data."
  (let* ((todo-id (enrichment-msg-todo-id msg))
         (data (enrichment-msg-data msg))
         (todo (find todo-id (model-todos model) :key #'todo-id :test #'string=)))
    (llog:info "Enrichment complete message received"
               :todo-id todo-id
               :has-data (if data "yes" "no")
               :found-todo (if todo "yes" "no"))
    (when todo
      (when data
        ;; Check if LLM says this belongs on a list
        (let ((list-name (getf data :list-name))
              (list-section (getf data :list-section))
              (list-items (getf data :list-items)))
          (if (and list-name (stringp list-name) (> (length list-name) 0))
              ;; Redirect to list: create list item(s), delete TODO
              (let ((list-def (db-find-list-by-name list-name)))
                (if list-def
                    (let ((items-to-create
                            (if list-items
                                ;; Multiple items from LLM
                                (mapcar (lambda (li)
                                          (list :title (or (getf li :title) "")
                                                :section (getf li :section)))
                                        list-items)
                                ;; Single item
                                (list (list :title (or (getf data :title) (todo-title todo))
                                            :section list-section)))))
                      (dolist (item-spec items-to-create)
                        (let ((item (make-list-item
                                      (list-def-id list-def)
                                      (getf item-spec :title)
                                      :section (getf item-spec :section)
                                      :device-id (get-device-id))))
                          (db-save-list-item item)
                          (notify-sync-list-item-changed item)
                          (llog:info "Created list item from TODO"
                                     :title (getf item-spec :title)
                                     :list-name list-name
                                     :section (getf item-spec :section))))
                      ;; Remove TODO from model and database
                      (setf (model-todos model)
                            (remove todo-id (model-todos model) :key #'todo-id :test #'string=))
                      (db-delete-todo todo-id)
                      ;; Refresh list data
                      (setf (model-lists-data model) (db-load-list-definitions))
                      (notify-sync-todo-deleted todo-id)
                      (invalidate-visible-todos-cache model)
                      (llog:info "Redirected TODO to list"
                                 :todo-id todo-id
                                 :list-name list-name
                                 :item-count (length items-to-create)))
                    ;; List not found - fall through to normal enrichment
                    (progn
                      (llog:warn "List not found for redirect, treating as normal TODO"
                                 :list-name list-name)
                      (apply-enrichment-to-todo todo data))))
              ;; Normal enrichment (no list redirect)
              (apply-enrichment-to-todo todo data))))
      ;; If no data, just clear enriching flag
      (unless data
        (setf (todo-enriching-p todo) nil))
      ;; Refresh tags cache since enrichment may add tags
      (refresh-tags-cache model))
    (values model nil)))

(defun apply-enrichment-to-todo (todo data)
  "Apply enrichment data fields to a TODO and save it."
  (setf (todo-enriching-p todo) nil)
  (when (getf data :title)
    (setf (todo-title todo) (getf data :title)))
  (when (getf data :description)
    (setf (todo-description todo) (getf data :description)))
  (when (getf data :priority)
    (setf (todo-priority todo) (getf data :priority)))
  (when (getf data :category)
    (let ((tag (category-to-tag (getf data :category))))
      (when tag
        (setf (todo-tags todo) (list tag)))))
  (when (getf data :scheduled-date)
    (setf (todo-scheduled-date todo) (getf data :scheduled-date)))
  (when (getf data :due-date)
    (setf (todo-due-date todo) (getf data :due-date)))
  (when (getf data :location-info)
    (setf (todo-location-info todo) (getf data :location-info)))
  (when (getf data :repeat-interval)
    (setf (todo-repeat-interval todo) (getf data :repeat-interval)))
  (when (getf data :repeat-unit)
    (setf (todo-repeat-unit todo) (getf data :repeat-unit)))
  (when (and (getf data :url) (not (todo-url todo)))
    (setf (todo-url todo) (getf data :url)))
  (save-todo todo))

(defun make-enrichment-cmd (todo-id raw-title raw-notes)
  "Create a command that performs async enrichment and returns completion message.
   Tuition runs this in a separate thread."
  (lambda ()
    (llog:info "Starting async enrichment"
               :todo-id todo-id
               :raw-title raw-title)
    (let ((result (handler-case
                      (enrich-todo-input raw-title raw-notes)
                    (error (e)
                      (llog:error "Enrichment failed"
                                  :todo-id todo-id
                                  :error (format nil "~A" e))
                      nil))))
      (llog:info "Enrichment complete"
                 :todo-id todo-id
                 :has-result (if result "yes" "no"))
      ;; Return the completion message - tuition will send it
      (make-instance 'enrichment-complete-msg
                     :todo-id todo-id
                     :enriched-data result))))

(defun make-spinner-start-cmd (spinner)
  "Create a command to kick off spinner animation."
  (lambda ()
    (sleep (tui.spinner::spinner-fps spinner))
    (tui.spinner:make-spinner-tick-msg
     :id (tui.spinner::spinner-id spinner))))

;;── Import Message ─────────────────────────────────────────────────────────────

(defclass import-complete-msg ()
  ((imported-todos
    :initarg :imported-todos
    :accessor import-msg-todos
    :documentation "List of plists with TODO data, or NIL if import failed.")
   (filename
    :initarg :filename
    :accessor import-msg-filename
    :documentation "The file that was imported."))
  (:documentation "Message sent when async org-mode import completes."))

(defmethod tui:update-message ((model app-model) (msg import-complete-msg))
  "Handle import completion by creating TODOs from imported data."
  (let ((todos-data (import-msg-todos msg))
        (filename (import-msg-filename msg)))
    (llog:info "Import complete message received"
               :filename filename
               :num-todos (if todos-data (length todos-data) 0))
    (when todos-data
      (dolist (data todos-data)
        ;; Check if this item belongs on a list
        (let ((list-name (getf data :list-name))
              (list-section (getf data :list-section)))
          (if (and list-name (stringp list-name) (> (length list-name) 0))
              ;; Create as a list item
              (let ((list-def (db-find-list-by-name list-name)))
                (when list-def
                  (let ((item (make-list-item
                                (list-def-id list-def)
                                (or (getf data :title) "Imported item")
                                :section list-section
                                :notes (getf data :description)
                                :device-id (get-device-id))))
                    (db-save-list-item item)
                    (notify-sync-list-item-changed item)
                    (llog:info "Imported item to list"
                               :list-name list-name
                               :item-title (list-item-title item)))))
              ;; Create as a regular TODO
              (let ((new-todo (make-todo (or (getf data :title) "Imported item")
                                         :description (getf data :description)
                                         :priority (or (getf data :priority) :medium))))
                (when (getf data :category)
                  (let ((tag (category-to-tag (getf data :category))))
                    (when tag
                      (setf (todo-tags new-todo) (list tag)))))
                (when (getf data :scheduled-date)
                  (setf (todo-scheduled-date new-todo) (getf data :scheduled-date)))
                (when (getf data :due-date)
                  (setf (todo-due-date new-todo) (getf data :due-date)))
                (when (getf data :location-info)
                  (setf (todo-location-info new-todo) (getf data :location-info)))
                (push new-todo (model-todos model))
                (save-todo new-todo)))))
      ;; Refresh data
      (setf (model-lists-data model) (db-load-list-definitions))
      (invalidate-visible-todos-cache model)
      (refresh-tags-cache model)
      (llog:info "Imported todos saved" :count (length todos-data)))
    (values model nil)))

(defun make-import-cmd (model filename)
  "Create a command that performs async org-mode import and returns completion message."
  (declare (ignore model))
  (lambda ()
    (llog:info "Starting async org-mode import" :filename filename)
    (let ((result (handler-case
                      (import-org-file filename)
                    (error (e)
                      (llog:error "Import failed"
                                  :filename filename
                                  :error (format nil "~A" e))
                      nil))))
      (llog:info "Import complete"
                 :filename filename
                 :num-todos (if result (length result) 0))
      (make-instance 'import-complete-msg
                     :filename filename
                     :imported-todos result))))

;;── External Editor ───────────────────────────────────────────────────────────

(defclass editor-complete-msg ()
  ((new-text
    :initarg :new-text
    :accessor editor-msg-new-text
    :documentation "The edited text, or NIL if unchanged/cancelled."))
  (:documentation "Message sent when external editor completes."))

(defmethod tui:update-message ((model app-model) (msg editor-complete-msg))
  "Handle editor completion by updating the description field."
  (let ((new-text (editor-msg-new-text msg)))
    (when new-text
      (tui.textinput:textinput-set-value (model-description-input model) new-text))
    ;; Force a redraw by sending window size
    (values model nil)))

(defun make-editor-cmd (current-text)
  "Create an exec-cmd that opens an external editor for text editing.
   Uses exec-cmd for proper TUI suspension."
  (llog:info "Creating external editor command")
  ;; Create temp file before returning the command
  (ensure-cache-directory)
  (let* ((editor (get-editor))
         (temp-file (merge-pathnames
                     (format nil "cloodoo-edit-~A.md" (get-universal-time))
                     (cache-directory))))
    ;; Write current text to temp file
    (with-open-file (stream temp-file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (when current-text
        (write-string current-text stream)))
    ;; Return exec-cmd with callback to read result
    (tui:make-exec-cmd editor
                       :args (list (namestring temp-file))
                       :callback (lambda ()
                                   (llog:info "Editor callback - reading result")
                                   (handler-case
                                       (let ((result (uiop:read-file-string temp-file)))
                                         ;; Clean up temp file
                                         (when (probe-file temp-file)
                                           (delete-file temp-file))
                                         ;; Return trimmed result
                                         (let ((trimmed (string-trim '(#\Space #\Newline #\Tab #\Return) result)))
                                           (make-instance 'editor-complete-msg
                                                          :new-text (if (> (length trimmed) 0)
                                                                        trimmed
                                                                        nil))))
                                     (error (e)
                                       (llog:warn "Error reading editor result" :error (princ-to-string e))
                                       (when (probe-file temp-file)
                                         (delete-file temp-file))
                                       nil))))))

;;── Direct Notes Editing (from detail view) ───────────────────────────────────

(defclass notes-editor-complete-msg ()
  ((todo-id
    :initarg :todo-id
    :accessor notes-editor-msg-todo-id
    :documentation "ID of the todo being edited.")
   (new-text
    :initarg :new-text
    :accessor notes-editor-msg-new-text
    :documentation "The edited notes text, or NIL if unchanged/cancelled."))
  (:documentation "Message sent when external notes editor completes for a specific todo."))

(defmethod tui:update-message ((model app-model) (msg notes-editor-complete-msg))
  "Handle notes editor completion by updating the todo's description directly."
  (let ((todo-id (notes-editor-msg-todo-id msg))
        (new-text (notes-editor-msg-new-text msg)))
    (when new-text
      ;; Find and update the todo
      (let ((todo (find todo-id (model-todos model) :key #'todo-id :test #'string=)))
        (when todo
          (setf (todo-description todo) (if (string= new-text "") nil new-text))
          (save-todo todo)
          (llog:info "Updated todo notes" :todo-id todo-id))))
    (values model nil)))

(defclass user-context-editor-complete-msg ()
  ((new-text
    :initarg :new-text
    :accessor user-context-msg-new-text
    :documentation "The edited user context text."))
  (:documentation "Message sent when external user context editor completes."))

(defmethod tui:update-message ((model app-model) (msg user-context-editor-complete-msg))
  "Handle user context editor completion by saving to database."
  (let ((new-text (user-context-msg-new-text msg)))
    (when new-text
      (save-user-context new-text)
      (setf (model-status-message model) "User context saved")
      (llog:info "Updated user context"))
    ;; WORKAROUND: Clear renderer cache to force redraw after exec-cmd
    ;; The exec-cmd clears the screen but the renderer thinks nothing changed
    (when *tui-program-ref*
      (handler-case
          (let ((renderer (tui::program-renderer *tui-program-ref*)))
            (setf (tui::last-output renderer) ""))
        (error (e)
          (llog:warn "Failed to clear renderer cache" :error e))))
    (values model nil)))

(defun make-user-context-editor-cmd ()
  "Create an exec-cmd that opens an external editor for editing user context.
   Uses exec-cmd for proper TUI suspension."
  (llog:info "Creating user context editor command")
  (ensure-cache-directory)
  (let* ((editor (get-editor))
         (current-text (load-user-context))
         (temp-file (merge-pathnames
                     (format nil "cloodoo-context-~A.md" (get-universal-time))
                     (cache-directory))))
    ;; Write current context to temp file
    (with-open-file (stream temp-file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (when current-text
        (write-string current-text stream)))
    ;; Return exec-cmd with callback to read result
    (tui:make-exec-cmd editor
                       :args (list (namestring temp-file))
                       :callback (lambda ()
                                   (llog:info "User context editor callback - reading result")
                                   (handler-case
                                       (let ((result (uiop:read-file-string temp-file)))
                                         ;; Clean up temp file
                                         (when (probe-file temp-file)
                                           (delete-file temp-file))
                                         ;; Return result
                                         (make-instance 'user-context-editor-complete-msg
                                                        :new-text result))
                                     (error (e)
                                       (llog:warn "Error reading user context editor result" :error (princ-to-string e))
                                       (when (probe-file temp-file)
                                         (delete-file temp-file))
                                       nil))))))

(defun make-detail-notes-editor-cmd (todo-id current-text)
  "Create an exec-cmd that opens an external editor for editing a todo's notes directly.
   Uses exec-cmd for proper TUI suspension."
  (llog:info "Creating notes editor command" :todo-id todo-id)
  ;; Create temp file before returning the command
  (ensure-cache-directory)
  (let* ((editor (get-editor))
         (temp-file (merge-pathnames
                     (format nil "cloodoo-notes-~A.md" (get-universal-time))
                     (cache-directory))))
    ;; Write current text to temp file
    (with-open-file (stream temp-file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (when current-text
        (write-string current-text stream)))
    ;; Return exec-cmd with callback to read result
    (tui:make-exec-cmd editor
                       :args (list (namestring temp-file))
                       :callback (lambda ()
                                   (llog:info "Notes editor callback - reading result" :todo-id todo-id)
                                   (handler-case
                                       (let ((result (uiop:read-file-string temp-file)))
                                         ;; Clean up temp file
                                         (when (probe-file temp-file)
                                           (delete-file temp-file))
                                         ;; Return trimmed result
                                         (let ((trimmed (string-trim '(#\Space #\Newline #\Tab #\Return) result)))
                                           (make-instance 'notes-editor-complete-msg
                                                          :todo-id todo-id
                                                          :new-text (if (> (length trimmed) 0)
                                                                        trimmed
                                                                        nil))))
                                     (error (e)
                                       (llog:warn "Error reading notes editor result" :error (princ-to-string e))
                                       (when (probe-file temp-file)
                                         (delete-file temp-file))
                                       nil))))))
