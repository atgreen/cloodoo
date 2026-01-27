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
    :documentation "Current view: :list, :detail, :add, :edit, :help, :search, :delete-confirm, :delete-done-confirm, :import, :edit-date, :add-scheduled-date, :add-due-date.")
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
   (collapsed-ids
    :initform (make-hash-table :test #'equal)
    :accessor model-collapsed-ids
    :documentation "Hash table of todo IDs that are collapsed.")
   (pending-parent-id
    :initform nil
    :accessor model-pending-parent-id
    :documentation "Parent ID for new todo being added as child.")
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
    :documentation "Error message if sync failed."))
  (:documentation "The application model following TEA pattern."))

(defun make-initial-model ()
  "Create the initial application model."
  (let ((model (make-instance 'app-model
                              :todos (load-todos))))
    ;; Load persisted presets
    (setf (model-tag-presets model) (load-presets))
    model))

;;── Collapse State Management ─────────────────────────────────────────────────

(defun todo-collapsed-p (model todo)
  "Check if a todo item is collapsed."
  (gethash (todo-id todo) (model-collapsed-ids model)))

(defun toggle-collapse (model todo)
  "Toggle the collapse state of a todo."
  (let* ((id (todo-id todo))
         (collapsed-ids (model-collapsed-ids model)))
    (if (gethash id collapsed-ids)
        (remhash id collapsed-ids)
        (setf (gethash id collapsed-ids) t))))

(defun any-ancestor-collapsed-p (model todos todo)
  "Check if any ancestor of this todo is collapsed."
  (let ((parent-id (todo-parent-id todo)))
    (loop while parent-id
          do (when (gethash parent-id (model-collapsed-ids model))
               (return-from any-ancestor-collapsed-p t))
             (let ((parent (find parent-id todos :key #'todo-id :test #'equal)))
               (if parent
                   (setf parent-id (todo-parent-id parent))
                   (setf parent-id nil))))
    nil))

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

(defun filter-todos-by-tags (todos selected-tags all-todos)
  "Filter todos to those matching ANY selected tag (using effective tags for children).
   Empty selection = show all."
  (if (zerop (hash-table-count selected-tags))
      todos
      (remove-if-not
       (lambda (todo)
         (some (lambda (tag) (gethash tag selected-tags))
               (get-effective-tags todo all-todos)))
       todos)))

;;── Child Task Helpers ────────────────────────────────────────────────────────

(defun editing-child-p (model)
  "Return T if currently editing or adding a child task."
  (or (model-pending-parent-id model)
      (and (model-edit-todo-id model)
           (let ((todo (find (model-edit-todo-id model) (model-todos model)
                             :key #'todo-id :test #'equal)))
             (and todo (todo-parent-id todo))))))

(defun get-effective-tags (todo all-todos)
  "Get the effective tags for a todo. For children, returns parent's tags."
  (if (todo-parent-id todo)
      ;; Child task - get tags from parent (walking up if needed)
      (let ((parent (find (todo-parent-id todo) all-todos :key #'todo-id :test #'equal)))
        (if parent
            (get-effective-tags parent all-todos)
            nil))
      ;; Top-level task - return its own tags
      (todo-tags todo)))

(defun build-parent-context (parent-id all-todos)
  "Build a list of (title . description) pairs for the parent chain.
   Returns list from immediate parent to root, or NIL if no parent."
  (when parent-id
    (let ((result nil)
          (current-id parent-id))
      (loop while current-id
            for parent = (find current-id all-todos :key #'todo-id :test #'equal)
            while parent
            do (push (cons (todo-title parent) (todo-description parent)) result)
               (setf current-id (todo-parent-id parent)))
      (nreverse result))))

;;── Tag Autocomplete Helpers ──────────────────────────────────────────────────

(defun filter-tags-by-query (all-tags query already-selected)
  "Filter tags containing query substring (case-insensitive).
   Excludes tags that are already selected."
  (let ((q (string-downcase (or query ""))))
    (if (= (length q) 0)
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
                              (eq (todo-status todo) :deleted))
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

(defun order-todos-hierarchically (todos)
  "Order todos so that children appear immediately after their parents.
   Returns a flat list with proper parent-child ordering."
  (let ((result nil)
        (processed (make-hash-table :test #'equal)))
    (labels ((add-with-children (todo)
               (unless (gethash (todo-id todo) processed)
                 (setf (gethash (todo-id todo) processed) t)
                 (push todo result)
                 ;; Add all children immediately after
                 (dolist (child (get-children todos (todo-id todo)))
                   (add-with-children child)))))
      ;; Start with top-level items (no parent)
      (dolist (todo todos)
        (when (null (todo-parent-id todo))
          (add-with-children todo)))
      ;; Also add any orphans (parent not in list)
      (dolist (todo todos)
        (unless (gethash (todo-id todo) processed)
          (add-with-children todo))))
    (nreverse result)))

(defun invalidate-visible-todos-cache (model)
  "Mark the visible todos cache as dirty so it will be regenerated."
  (setf (model-visible-todos-dirty model) t))

(defun add-parent-chain-to-group (todo all-todos group-set)
  "Add the parent chain of TODO to GROUP-SET (a hash table of todo-ids).
   Returns list of parent todos added (for ordering purposes)."
  (let ((added nil)
        (parent-id (todo-parent-id todo)))
    (loop while parent-id
          for parent = (find parent-id all-todos :key #'todo-id :test #'equal)
          while parent
          unless (gethash (todo-id parent) group-set)
          do (setf (gethash (todo-id parent) group-set) parent)
             (push parent added)
          do (setf parent-id (todo-parent-id parent)))
    added))

(defun compute-visible-todos-grouped (model)
  "Compute the grouped list of visible todos after filtering, sorting, and collapsing.
   Returns an alist of (category . todo-ids) preserving display order.
   When children appear in a group, their parent chain is included for context."
  (let* ((all-todos (model-todos model))
         ;; Apply tag filter first (using all-todos for effective tags lookup)
         (tag-filtered (filter-todos-by-tags all-todos (model-selected-tags model) all-todos))
         (filtered (filter-todos tag-filtered
                                 :status (model-filter-status model)
                                 :priority (model-filter-priority model)
                                 :search-query (model-search-query model)))
         (groups (group-todos-by-date filtered))
         (result nil))
    ;; Build result with todo IDs (not objects) to allow status changes
    (dolist (group groups)
      (let* ((category (car group))
             (group-todos (cdr group))
             ;; Build a set of todos in this group, then add parent chains
             (group-set (make-hash-table :test #'equal)))
        ;; First add all todos that belong to this group by date
        (dolist (todo group-todos)
          (setf (gethash (todo-id todo) group-set) todo))
        ;; Then add parent chains for any children in the group
        (dolist (todo group-todos)
          (when (todo-parent-id todo)
            (add-parent-chain-to-group todo all-todos group-set)))
        ;; Collect all todos now in the group
        (let ((expanded-group nil))
          (maphash (lambda (id todo)
                     (declare (ignore id))
                     (push todo expanded-group))
                   group-set)
          ;; Sort top-level items by priority, then order hierarchically
          (let* ((top-level (remove-if #'todo-parent-id expanded-group))
                 (children (remove-if-not #'todo-parent-id expanded-group))
                 (sorted-top (sort-todos top-level :priority t))
                 (ordered (order-todos-hierarchically (append sorted-top children)))
                 (visible-ids nil))
            (dolist (todo ordered)
              ;; Skip if any ancestor is collapsed
              (unless (any-ancestor-collapsed-p model all-todos todo)
                (push (todo-id todo) visible-ids)))
            (when visible-ids
              (push (cons category (nreverse visible-ids)) result))))))
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
  "Reorder todos by date category, then sort within each group, preserving hierarchy."
  (let* ((groups (group-todos-by-date todos))
         (result nil))
    (dolist (group groups)
      (let* ((group-todos (cdr group))
             ;; Sort only top-level items in the group
             (top-level (remove-if #'todo-parent-id group-todos))
             (sorted-top (sort-todos top-level sort-by descending))
             ;; Order hierarchically to keep children with parents
             (ordered (order-todos-hierarchically
                       (append sorted-top
                               (remove-if-not #'todo-parent-id group-todos)))))
        (dolist (todo ordered)
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
      (dolist (todo (cdr group))
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
        (dolist (todo (cdr group))
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
      (dolist (todo (cdr group))
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

;;── TEA Methods ───────────────────────────────────────────────────────────────

(defmethod tui:init ((model app-model))
  "Initialize the application model."
  (tui:set-terminal-title "cloodoo")
  (let ((size (tui:get-terminal-size)))
    (when size
      (setf (model-term-width model) (car size))
      (setf (model-term-height model) (cdr size))))
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
      ((eq key :tab)
       (setf (model-sidebar-focused model) nil)
       (values model nil))

      ;; Move up
      ((or (eq key :up)
           (and (characterp key) (or (char= key #\k) (char= key #\p))))
       (when (> (model-sidebar-cursor model) 0)
         (decf (model-sidebar-cursor model)))
       (values model nil))

      ;; Move down
      ((or (eq key :down)
           (and (characterp key) (or (char= key #\j) (char= key #\n))))
       (when (< (model-sidebar-cursor model) max-cursor)
         (incf (model-sidebar-cursor model)))
       (values model nil))

      ;; Space - toggle tag selection
      ((and (characterp key) (char= key #\Space))
       (let ((cursor (model-sidebar-cursor model))
             (selected (model-selected-tags model)))
         (if (= cursor 0)
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
      ((or (eq key :escape) (and (characterp key) (char= key #\q)))
       (setf (model-sidebar-focused model) nil)
       (values model nil))

      ;; Delete - delete the selected tag (not available for "All")
      ((or (eq key :delete) (eq key :backspace)
           (and (characterp key) (char= key #\d)))
       (let ((cursor (model-sidebar-cursor model)))
         (if (= cursor 0)
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
      ((and (eq key :tab) (model-sidebar-visible model) (not (model-sidebar-focused model)))
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
      ((or (eq key :up)
           (and (characterp key) (or (char= key #\k) (char= key #\p))))
       (when (> (model-cursor model) 0)
         (decf (model-cursor model)))
       (values model nil))

      ;; Move down (n/j/down - org uses n for next)
      ((or (eq key :down)
           (and (characterp key) (or (char= key #\j) (char= key #\n))))
       (when (< (model-cursor model) (1- (length todos)))
         (incf (model-cursor model)))
       (values model nil))

      ;; Page up (Ctrl+U or Page Up key)
      ((or (eq key :page-up)
           (and ctrl (characterp key) (char= key #\u)))
       (let* ((page-size (max 1 (- (model-term-height model) 6)))
              (new-pos (max 0 (- (model-cursor model) page-size))))
         (setf (model-cursor model) new-pos))
       (values model nil))

      ;; Page down (Ctrl+D or Page Down key)
      ((or (eq key :page-down)
           (and ctrl (characterp key) (char= key #\d)))
       (let* ((page-size (max 1 (- (model-term-height model) 6)))
              (max-pos (max 0 (1- (length todos))))
              (new-pos (min max-pos (+ (model-cursor model) page-size))))
         (setf (model-cursor model) new-pos))
       (values model nil))

      ;; Increase priority (Shift+Up)
      ((eq key :shift-up)
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
      ((eq key :shift-down)
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
      ((or (eq key :home) (and (characterp key) (char= key #\g)))
       (setf (model-cursor model) 0)
       (values model nil))

      ;; Go to bottom
      ((or (eq key :end) (and (characterp key) (char-equal key #\G)))
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
              (if (and (todo-repeat-interval todo) (todo-repeat-unit todo))
                  ;; Auto-reschedule: calculate next occurrence and stay pending
                  (let ((next-date (calculate-next-occurrence todo)))
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
                                                       :format '(:short-month " " :day)))))
                  ;; Non-repeating: mark as completed
                  (progn
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
      ((eq key :enter)
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
       ;; Clear pending parent - this is a sibling add
       (setf (model-pending-parent-id model) nil)
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
      ((eq key :delete)
       (when (< (model-cursor model) (length todos))
         (setf (model-view-state model) :delete-confirm))
       (values model nil))

      ;; Delete all DONE items (Shift+D)
      ((and (characterp key) (char= key #\D))
       (when (find-if (lambda (todo) (eq (todo-status todo) +status-completed+))
                      (model-todos model))
         (setf (model-view-state model) :delete-done-confirm))
       (values model nil))

      ;; Toggle collapse (z)
      ((and (characterp key) (char= key #\z))
       (when (< (model-cursor model) (length todos))
         (let* ((todo (nth (model-cursor model) todos))
                (all-todos (model-todos model)))
           ;; Only toggle if has children
           (when (has-children-p all-todos todo)
             (toggle-collapse model todo)
             (invalidate-visible-todos-cache model))))
       (values model nil))

      ;; Indent: make child of previous sibling (Tab or >)
      ((or (eq key :tab) (and (characterp key) (char= key #\>)))
       (when (and (> (model-cursor model) 0)
                  (< (model-cursor model) (length todos)))
         (let* ((todo (nth (model-cursor model) todos))
                (prev-todo (nth (1- (model-cursor model)) todos))
                (all-todos (model-todos model)))
           ;; Can only indent if prev todo has same parent (sibling) or is potential parent
           (when (equal (todo-parent-id todo) (todo-parent-id prev-todo))
             ;; Make todo a child of prev-todo
             (setf (todo-parent-id todo) (todo-id prev-todo))
             (save-todo todo)
             (invalidate-visible-todos-cache model))))
       (values model nil))

      ;; Outdent: move to parent's level (Shift+Tab or <)
      ((or (eq key :backtab) (and (characterp key) (char= key #\<)))
       (when (< (model-cursor model) (length todos))
         (let* ((todo (nth (model-cursor model) todos))
                (all-todos (model-todos model))
                (parent-id (todo-parent-id todo)))
           (when parent-id
             ;; Find parent and get grandparent's id
             (let ((parent (find parent-id all-todos :key #'todo-id :test #'equal)))
               (when parent
                 (setf (todo-parent-id todo) (todo-parent-id parent))
                 (save-todo todo)
                 (invalidate-visible-todos-cache model))))))
       (values model nil))

      ;; Add child to selected item (Shift+A)
      ((and (characterp key) (char= key #\A))
       (when (< (model-cursor model) (length todos))
         (let ((parent-todo (nth (model-cursor model) todos)))
           (llog:info "Add child TODO triggered" :parent-id (todo-id parent-todo))
           (setf (model-view-state model) :add)
           (setf (model-edit-todo-id model) nil)
           (setf (model-edit-priority model) :medium)
           (setf (model-active-field model) :title)
           ;; Store parent-id for the new todo
           (setf (model-pending-parent-id model) (todo-id parent-todo))
           ;; Clear scheduled date but inherit due date from parent
           (setf (model-edit-scheduled-date model) nil)
           (setf (model-edit-due-date model) (todo-due-date parent-todo))
           (setf (model-edit-repeat-interval model) nil)
           (setf (model-edit-repeat-unit model) nil)
           ;; Child tasks don't have their own tags - they inherit from parent
           (setf (model-edit-tags model) nil)
           (tui.textinput:textinput-set-value (model-tags-input model) "")
           (setf (model-tag-dropdown-visible model) nil)
           (setf (model-tag-dropdown-cursor model) 0)
           ;; Reset inputs
           (tui.textinput:textinput-set-value (model-title-input model) "")
           (tui.textinput:textinput-set-value (model-description-input model) "")
           (tui.textinput:textinput-focus (model-title-input model))
           (tui.textinput:textinput-blur (model-description-input model))))
       (values model nil))

      ;; Search
      ((and (characterp key) (char= key #\/))
       (setf (model-view-state model) :search)
       (tui.textinput:textinput-set-value (model-search-input model) (model-search-query model))
       (tui.textinput:textinput-focus (model-search-input model))
       (values model nil))

      ;; Set priority B=Medium, C=Low (org-mode style)
      ;; Note: A is now used for add-child
      ((and (characterp key) (char-equal key #\B))
       (when (< (model-cursor model) (length todos))
         (let ((todo (nth (model-cursor model) todos)))
           (setf (todo-priority todo) :medium)
           (save-todo todo)))
       (values model nil))

      ((and (characterp key) (char-equal key #\C))
       (when (< (model-cursor model) (length todos))
         (let ((todo (nth (model-cursor model) todos)))
           (setf (todo-priority todo) :low)
           (save-todo todo)))
       (values model nil))

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
           (if scheduled
               (let ((utime (lt:timestamp-to-universal scheduled)))
                 (tui.datepicker:datepicker-set-time picker utime)
                 (setf (tui.datepicker:datepicker-selected picker) utime))
               (progn
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
           (if deadline
               (let ((utime (lt:timestamp-to-universal deadline)))
                 (tui.datepicker:datepicker-set-time picker utime)
                 (setf (tui.datepicker:datepicker-selected picker) utime))
               (progn
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
             (let ((parent-context (build-parent-context (todo-parent-id todo) (model-todos model))))
               (llog:info "Re-triggering enrichment"
                          :todo-id (todo-id todo)
                          :title (todo-title todo)
                          :has-parent-context (if parent-context "yes" "no"))
               (setf (todo-enriching-p todo) t)
               (save-todo todo)
               (return-from handle-list-keys
                 (values model (list (make-enrichment-cmd (todo-id todo)
                                                          (todo-title todo)
                                                          (todo-description todo)
                                                          parent-context)
                                     (make-spinner-start-cmd
                                      (model-enrichment-spinner model)))))))))
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
      ((eq key :escape)
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
      ((eq key :tab)
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
          ;; Skip tags for child tasks (they inherit from parent)
          (if (editing-child-p model)
              (progn
                (setf (model-active-field model) :title)
                (tui.textinput:textinput-focus (model-title-input model)))
              (progn
                (setf (model-active-field model) :tags)
                (tui.textinput:textinput-focus (model-tags-input model))
                (update-tag-dropdown model))))
         (:tags
          ;; Tab in tags field: complete tag if input present, else move to next field
          (let* ((query (tui.textinput:textinput-value (model-tags-input model)))
                 (filtered (model-tag-dropdown-filtered model))
                 (cursor (model-tag-dropdown-cursor model))
                 (has-completion (or (and (model-tag-dropdown-visible model)
                                          (> (length filtered) 0))
                                     (> (length query) 0))))
            (if has-completion
                ;; Complete the tag (same logic as Enter)
                (progn
                  (cond
                    ((and (model-tag-dropdown-visible model) (> (length filtered) 0))
                     (add-tag-to-edit-tags model (nth cursor filtered)))
                    ((> (length query) 0)
                     (add-tag-to-edit-tags model query)))
                  (update-tag-dropdown model))
                ;; No input - move to next field
                (progn
                  (setf (model-active-field model) :title)
                  (tui.textinput:textinput-blur (model-tags-input model))
                  (setf (model-tag-dropdown-visible model) nil)
                  (tui.textinput:textinput-focus (model-title-input model)))))))
       (values model nil))

      ;; Shift+Tab to previous field
      ((eq key :backtab)
       (case (model-active-field model)
         (:title
          ;; Skip tags for child tasks (they inherit from parent)
          (tui.textinput:textinput-blur (model-title-input model))
          (if (editing-child-p model)
              (setf (model-active-field model) :repeat)
              (progn
                (setf (model-active-field model) :tags)
                (tui.textinput:textinput-focus (model-tags-input model))
                (update-tag-dropdown model))))
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
      ((and (eq (model-active-field model) :priority)
            (characterp key)
            (member key '(#\a #\A #\b #\B #\c #\C) :test #'char=))
       (setf (model-edit-priority model)
             (case (char-upcase key)
               (#\A :high)
               (#\B :medium)
               (#\C :low)))
       (values model nil))

      ;; Space to open datepicker for scheduled date
      ((and (eq (model-active-field model) :scheduled)
            (characterp key) (char= key #\Space))
       (let ((picker (model-date-picker model))
             (current (model-edit-scheduled-date model)))
         (if current
             (let ((utime (lt:timestamp-to-universal current)))
               (tui.datepicker:datepicker-set-time picker utime)
               (setf (tui.datepicker:datepicker-selected picker) utime))
             (progn
               (tui.datepicker:datepicker-set-time picker (get-universal-time))
               (tui.datepicker:datepicker-unselect picker)))
         (tui.datepicker:datepicker-focus picker)
         (setf (model-editing-date-type model) :scheduled)
         (setf (model-view-state model) :add-scheduled-date))
       (values model nil))

      ;; Space to open datepicker for due date
      ((and (eq (model-active-field model) :due)
            (characterp key) (char= key #\Space))
       (let ((picker (model-date-picker model))
             (current (model-edit-due-date model)))
         (if current
             (let ((utime (lt:timestamp-to-universal current)))
               (tui.datepicker:datepicker-set-time picker utime)
               (setf (tui.datepicker:datepicker-selected picker) utime))
             (progn
               (tui.datepicker:datepicker-set-time picker (get-universal-time))
               (tui.datepicker:datepicker-unselect picker)))
         (tui.datepicker:datepicker-focus picker)
         (setf (model-editing-date-type model) :due)
         (setf (model-view-state model) :add-due-date))
       (values model nil))

      ;; Backspace to clear dates
      ((and (member (model-active-field model) '(:scheduled :due))
            (eq key :backspace))
       (case (model-active-field model)
         (:scheduled (setf (model-edit-scheduled-date model) nil))
         (:due (setf (model-edit-due-date model) nil)))
       (values model nil))

      ;; Repeat field: Left/h for previous preset, Right/l for next preset
      ((and (eq (model-active-field model) :repeat)
            (or (eq key :left) (eq key :right)
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
              (direction (if (or (eq key :left)
                                 (and (characterp key) (char= key #\h)))
                             -1 1))
              (new-idx (mod (+ idx direction) (length presets)))
              (new-val (nth new-idx presets)))
         (if new-val
             (progn
               (setf (model-edit-repeat-interval model) (car new-val))
               (setf (model-edit-repeat-unit model) (cdr new-val)))
             (progn
               (setf (model-edit-repeat-interval model) nil)
               (setf (model-edit-repeat-unit model) nil))))
       (values model nil))

      ;; Tags field: Up/Down to navigate dropdown
      ((and (eq (model-active-field model) :tags)
            (or (eq key :up) (eq key :down)))
       (let* ((filtered (model-tag-dropdown-filtered model))
              (max-idx (max 0 (1- (length filtered)))))
         (when (and (model-tag-dropdown-visible model) (> (length filtered) 0))
           (if (eq key :up)
               (setf (model-tag-dropdown-cursor model)
                     (max 0 (1- (model-tag-dropdown-cursor model))))
               (setf (model-tag-dropdown-cursor model)
                     (min max-idx (1+ (model-tag-dropdown-cursor model)))))))
       (values model nil))

      ;; Tags field: Enter to select from dropdown OR create new tag
      ;; If nothing to complete, fall through to form submit
      ((and (eq (model-active-field model) :tags)
            (eq key :enter)
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
      ((and (eq (model-active-field model) :tags)
            (eq key :backspace))
       (let ((query (tui.textinput:textinput-value (model-tags-input model))))
         (if (= (length query) 0)
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
      ((eq (model-active-field model) :tags)
       (multiple-value-bind (new-input cmd)
           (tui.textinput:textinput-update (model-tags-input model) msg)
         (declare (ignore cmd))
         (setf (model-tags-input model) new-input)
         (update-tag-dropdown model))
       (values model nil))

      ;; Save on Enter (when not in text field or when in priority field)
      ((eq key :enter)
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
                   ;; Save tags (but not for child tasks - they inherit from parent)
                   (unless (todo-parent-id todo)
                     (setf (todo-tags todo) (reverse (model-edit-tags model))))
                   (save-todo todo)
                   ;; Refresh tags cache since tags may have changed
                   (refresh-tags-cache model))
                 (setf (model-view-state model) :list)
                 (return-from handle-add-edit-keys (values model nil)))
               ;; Create new TODO with async LLM enrichment
               (let* ((desc-input (tui.textinput:textinput-value (model-description-input model)))
                      (desc-value (when (> (length desc-input) 0) desc-input))
                      (parent-id (model-pending-parent-id model))
                      (tags (reverse (model-edit-tags model)))
                      ;; Create todo immediately with raw data, mark as enriching
                      (new-todo (make-todo title
                                          :description desc-value
                                          :priority (model-edit-priority model)
                                          :scheduled-date (model-edit-scheduled-date model)
                                          :due-date (model-edit-due-date model)
                                          :repeat-interval (model-edit-repeat-interval model)
                                          :repeat-unit (model-edit-repeat-unit model)
                                          :parent-id parent-id)))
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
                 ;; Clear pending parent-id
                 (setf (model-pending-parent-id model) nil)
                 (setf (model-view-state model) :list)
                 ;; Build parent context for enrichment
                 (let ((parent-context (build-parent-context parent-id (model-todos model))))
                   (llog:info "Created todo, starting async enrichment"
                              :todo-id (todo-id new-todo)
                              :title title
                              :parent-id parent-id
                              :has-parent-context (if parent-context "yes" "no"))
                   ;; Return command for async enrichment
                   (return-from handle-add-edit-keys
                     (values model (list (make-enrichment-cmd (todo-id new-todo)
                                                              title
                                                              desc-input
                                                              parent-context)
                                         (make-spinner-start-cmd
                                          (model-enrichment-spinner model))))))))))
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
      ((eq key :escape)
       (setf (model-view-state model) :list)
       (values model nil))

      ;; Apply search with Enter
      ((eq key :enter)
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
      ((eq key :escape)
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
      ((or (eq key :up) (eq key :down))
       (let* ((filtered (model-tag-dropdown-filtered model))
              (max-idx (max 0 (1- (length filtered)))))
         (when (and (model-tag-dropdown-visible model) (> (length filtered) 0))
           (if (eq key :up)
               (setf (model-tag-dropdown-cursor model)
                     (max 0 (1- (model-tag-dropdown-cursor model))))
               (setf (model-tag-dropdown-cursor model)
                     (min max-idx (1+ (model-tag-dropdown-cursor model)))))))
       (values model nil))

      ;; Enter - add tag from dropdown or input, or save and close if empty
      ((eq key :enter)
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
      ((eq key :backspace)
       (let ((query (tui.textinput:textinput-value (model-tags-input model))))
         (if (= (length query) 0)
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
      ((eq key :escape)
       (setf (model-view-state model) :list)
       (values model nil))

      ;; Start import with Enter
      ((eq key :enter)
       (let ((filename (tui.textinput:textinput-value (model-import-input model))))
         (if (and (> (length filename) 0)
                  (probe-file filename))
             (progn
               (llog:info "Starting org-mode import" :filename filename)
               (setf (model-view-state model) :list)
               ;; Return command to perform async import
               (values model (list (make-import-cmd model filename)
                                   (make-spinner-start-cmd
                                    (model-enrichment-spinner model)))))
             (progn
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
      ((or (eq key :escape)
           (and (characterp key) (char= key #\q))
           (eq key :enter))
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
           (if scheduled
               (let ((utime (lt:timestamp-to-universal scheduled)))
                 (tui.datepicker:datepicker-set-time picker utime)
                 (setf (tui.datepicker:datepicker-selected picker) utime))
               (progn
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
           (if deadline
               (let ((utime (lt:timestamp-to-universal deadline)))
                 (tui.datepicker:datepicker-set-time picker utime)
                 (setf (tui.datepicker:datepicker-selected picker) utime))
               (progn
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
      ((or (eq key :escape)
           (and ctrl (characterp key) (char= key #\c)))
       (setf (model-view-state model) :list)
       (values model nil))

      ;; Confirm with Enter - save the current cursor date and go back to list
      ((eq key :enter)
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
      ((or (eq key :backspace) (eq key :delete))
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
      ((or (eq key :escape)
           (and ctrl (characterp key) (char= key #\c)))
       (setf (model-view-state model) :detail)
       (values model nil))

      ;; Confirm with Enter - save the cursor date and go back
      ((eq key :enter)
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
      ((or (eq key :backspace) (eq key :delete))
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
      ((or (eq key :escape)
           (and ctrl (characterp key) (char= key #\c)))
       (setf (model-view-state model) return-view)
       (setf (model-active-field model) (if (eq date-type :scheduled) :scheduled :due))
       (values model nil))

      ;; Confirm with Enter - save the cursor date to form state and go back
      ((eq key :enter)
       (let* ((current-date (when picker (tui.datepicker:datepicker-time picker)))
              (timestamp (when current-date
                           (lt:universal-to-timestamp current-date))))
         ;; Save the date to form state based on which type we're editing
         (cond
           ((eq date-type :scheduled)
            (setf (model-edit-scheduled-date model) timestamp)
            (setf (model-active-field model) :scheduled))
           ((eq date-type :due)
            (setf (model-edit-due-date model) timestamp)
            (setf (model-active-field model) :due))))
       (setf (model-view-state model) return-view)
       (values model nil))

      ;; Clear date with Backspace or Delete
      ((or (eq key :backspace) (eq key :delete))
       (cond
         ((eq date-type :scheduled)
          (setf (model-edit-scheduled-date model) nil)
          (setf (model-active-field model) :scheduled))
         ((eq date-type :due)
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
    (t (values model nil))))

(defmethod tui:update-message ((model app-model) (msg tui:window-size-msg))
  "Handle window resize."
  (setf (model-term-width model) (tui:window-size-msg-width msg))
  (setf (model-term-height model) (tui:window-size-msg-height msg))
  (values model nil))

(defmethod tui:update-message ((model app-model) (msg sync-refresh-msg))
  "Handle sync refresh - model already updated, just trigger redraw."
  (values model nil))

(defmethod tui:update-message ((model app-model) (msg tui:mouse-scroll-event))
  "Handle mouse scroll by scrolling the viewport."
  (when (eq (model-view-state model) :list)
    (let* ((direction (tui:mouse-scroll-direction msg))
           (count (tui:mouse-scroll-count msg))
           (todos (get-visible-todos model))
           (groups (group-todos-by-date todos))
           (num-lines (max 1 (+ (length todos) (* (length groups) +header-lines+))))
           (viewport-height (max 5 (- (model-term-height model) 3)))
           (max-offset (max 0 (- num-lines viewport-height)))
           (offset (model-scroll-offset model))
           (new-offset (case direction
                         (:up (max 0 (- offset count)))
                         (:down (min max-offset (+ offset count)))
                         (t offset))))
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
      ((eq view-state :list)
       (let* ((screen-x (1- (tui:mouse-event-x msg)))  ; Convert to 0-based
              (screen-line (- (tui:mouse-event-y msg) 2))
              (list-line (- screen-line +list-top-lines+))
              (available-height (max 5 (- (model-term-height model) 3)))
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
                 (eq button :left))
            ;; Sidebar rows: 0=header, 1=separator, 2="All", 3+=tags
            (let* ((sidebar-item (- screen-line 2))
                   (tags (model-all-tags-cache model))
                   (max-cursor (length tags)))
              (when (and (>= sidebar-item 0) (<= sidebar-item max-cursor))
                ;; Focus sidebar, set cursor, and toggle the tag
                (setf (model-sidebar-focused model) t
                      (model-sidebar-cursor model) sidebar-item)
                (let ((selected (model-selected-tags model)))
                  (if (= sidebar-item 0)
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
                 (eq button :left) (= screen-x scrollbar-col))
            (setf (model-scrollbar-dragging model) t)
            (scroll-to-y-position model list-line available-height))
           ;; Left-click on list: select item
           ((and (>= list-line 0) (< list-line available-height)
                 (eq button :left))
            (let* ((line-idx (+ (model-scroll-offset model) list-line))
                   (groups (get-visible-todos-grouped model))
                   (cursor (cursor-index-for-line groups line-idx)))
              (when cursor
                (setf (model-cursor model) cursor
                      (model-sidebar-focused model) nil))))
           ;; Right-click on list: select item and show details
           ((and (>= list-line 0) (< list-line available-height)
                 (eq button :right))
            (let* ((line-idx (+ (model-scroll-offset model) list-line))
                   (groups (get-visible-todos-grouped model))
                   (cursor (cursor-index-for-line groups line-idx)))
              (when cursor
                (setf (model-cursor model) cursor
                      (model-sidebar-focused model) nil
                      (model-view-state model) :detail)))))))
      ;; Detail view: click on URL to open it
      ;; URLs appear in the lower section; we check Y position and URL presence
      ((eq view-state :detail)
       (when (eq button :left)
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
  (when (and (eq (model-view-state model) :list)
             (model-scrollbar-dragging model))
    (let* ((screen-line (- (tui:mouse-event-y msg) 2))
           (list-line (- screen-line +list-top-lines+))
           (available-height (max 5 (- (model-term-height model) 3))))
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
      ;; Clear enriching flag
      (setf (todo-enriching-p todo) nil)
      ;; Apply enriched data if available
      (when data
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
        (when (getf data :estimated-minutes)
          (setf (todo-estimated-minutes todo) (getf data :estimated-minutes)))
        (when (getf data :scheduled-date)
          (setf (todo-scheduled-date todo) (getf data :scheduled-date)))
        (when (getf data :due-date)
          (setf (todo-due-date todo) (getf data :due-date)))
        (when (getf data :location-info)
          (setf (todo-location-info todo) (getf data :location-info))))
      ;; Save updated todo
      (save-todo todo)
      ;; Refresh tags cache since enrichment may add tags
      (refresh-tags-cache model))
    (values model nil)))

(defun make-enrichment-cmd (todo-id raw-title raw-notes &optional parent-context)
  "Create a command that performs async enrichment and returns completion message.
   PARENT-CONTEXT is an optional list of (title . description) pairs for parent tasks.
   Tuition runs this in a separate thread."
  (lambda ()
    (llog:info "Starting async enrichment"
               :todo-id todo-id
               :raw-title raw-title
               :has-parent-context (if parent-context "yes" "no"))
    (let ((result (handler-case
                      (enrich-todo-input raw-title raw-notes parent-context)
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
        (let ((new-todo (make-todo (or (getf data :title) "Imported item")
                                   :description (getf data :description)
                                   :priority (or (getf data :priority) :medium))))
          ;; Apply enriched fields
          (when (getf data :category)
            (let ((tag (category-to-tag (getf data :category))))
              (when tag
                (setf (todo-tags new-todo) (list tag)))))
          (when (getf data :estimated-minutes)
            (setf (todo-estimated-minutes new-todo) (getf data :estimated-minutes)))
          (when (getf data :scheduled-date)
            (setf (todo-scheduled-date new-todo) (getf data :scheduled-date)))
          (when (getf data :due-date)
            (setf (todo-due-date new-todo) (getf data :due-date)))
          (when (getf data :location-info)
            (setf (todo-location-info new-todo) (getf data :location-info)))
          ;; Add to model and save
          (push new-todo (model-todos model))
          (save-todo new-todo)))
      ;; Invalidate cache so new todos appear
      (invalidate-visible-todos-cache model)
      ;; Refresh tags cache after import
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
