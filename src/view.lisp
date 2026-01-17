;;; view.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cloodoo)

;;── View Functions ─────────────────────────────────────────────────────────────

(defun overlay-modal (background modal term-width term-height modal-width modal-height)
  "Overlay a modal dialog centered on a background view.
   Returns the composited result."
  (let* ((bg-lines (uiop:split-string background :separator '(#\Newline)))
         (modal-lines (uiop:split-string modal :separator '(#\Newline)))
         ;; Center the modal vertically and horizontally
         (start-row (max 0 (floor (- term-height modal-height) 2)))
         (start-col (max 0 (floor (- term-width modal-width) 2))))
    (with-output-to-string (s)
      (loop for row from 0 below (length bg-lines)
            for bg-line = (nth row bg-lines)
            do (when (> row 0) (format s "~%"))
               (if (and (>= row start-row)
                        (< row (+ start-row modal-height))
                        (< (- row start-row) (length modal-lines)))
                   ;; This row has modal content
                   (let* ((modal-row (- row start-row))
                          (modal-line (nth modal-row modal-lines)))
                     ;; Build: prefix from bg + modal
                     (if (> start-col 0)
                         ;; Need to extract prefix from background
                         (let ((prefix (subseq bg-line 0 (min start-col (length bg-line)))))
                           (format s "~A~A" prefix modal-line))
                         (format s "~A" modal-line)))
                   ;; Just use background line
                   (format s "~A" bg-line))))))

(defun pad-content-to-width (content target-width)
  "Pad each line of content to target-width (accounting for border chars).
   The target-width should be the inner width (term-width - 2 for borders)."
  (when (null content)
    (return-from pad-content-to-width (make-string target-width :initial-element #\Space)))
  (let ((lines (uiop:split-string content :separator '(#\Newline))))
    (with-output-to-string (s)
      (loop for line in lines
            for i from 0
            do (when (> i 0) (format s "~%"))
               (let* ((visible-len (tui:visible-length (or line "")))
                      (padding (max 0 (- target-width visible-len))))
                 (format s "~A~A" (or line "") (make-string padding :initial-element #\Space)))))))

(defun render-app-title-bar (model)
  "Render the application title bar."
  (let* ((width (model-term-width model))
         (title "Week-agenda:")
         (right "")
         (padding (max 0 (- width (length title)))))
    (tui:bold
     (tui:colored
      (format nil "~A~A~A" title (make-string padding :initial-element #\Space) right)
      :fg tui:*fg-blue*))))

(defun render-help-bar-line (width)
  "Render the help bar."
  (let* ((help "F1:Help | jk:Nav | Enter:View | Space:Done | a:Add | e:Edit | d:Del | /:Search | q:Quit")
         (pad (max 0 (- width (length help)))))
    (tui:colored
     (format nil "~A~A" help (make-string pad :initial-element #\Space))
     :fg tui:*fg-yellow* :bg tui:*bg-blue*)))

(defun render-list-view (model)
  "Render the main agenda view."
  (let* ((term-height (model-term-height model))
         (term-width (model-term-width model))
         (available-height (max 5 (- term-height 3)))
         (todos (get-visible-todos model))
         (groups (group-todos-by-date todos)))

    (adjust-scroll model available-height)

    (with-output-to-string (s)
      ;; Title + date header (org-agenda style)
      (format s "~A~%" (render-app-title-bar model))
      (format s "~A~%"
              (tui:bold
               (tui:colored
                (lt:format-timestring nil (lt:today)
                                      :format '(:long-weekday " " :day " " :long-month " " :year))
                :fg tui:*fg-blue*)))

      ;; Agenda content (no border)
      (let ((content
              (with-output-to-string (c)
                (if (null todos)
                    (format c "No items. Press 'a' to add one.")
                    (let ((current-idx 0)
                          (lines-rendered 0)
                          (offset (model-scroll-offset model))
                          (first-line t))
                      (dolist (group groups)
                        (let ((category (car group))
                              (group-todos (cdr group)))
                          (when (and (>= lines-rendered offset)
                                     (< (- lines-rendered offset) available-height))
                            (unless first-line (format c "~%"))
                            (setf first-line nil)
                            (format c "~A" (date-category-colored category)))
                          (incf lines-rendered)

                          (dolist (todo group-todos)
                            (when (and (>= lines-rendered offset)
                                       (< (- lines-rendered offset) available-height))
                              (format c "~%")
                              (let* ((all-todos (model-todos model))
                                     (selected-p (= current-idx (model-cursor model)))
                                     ;; Calculate depth for indentation (2 spaces per level)
                                     (depth (get-todo-depth all-todos todo))
                                     (depth-indent (make-string (* 2 depth) :initial-element #\Space))
                                     ;; Check if this todo has children and its collapse state
                                     (has-kids (has-children-p all-todos todo))
                                     (is-collapsed (todo-collapsed-p model todo))
                                     (collapse-ind (format-collapse-indicator is-collapsed has-kids))
                                     ;; Progress indicator for items with children
                                     (progress (count-subtask-progress all-todos (todo-id todo)))
                                     (progress-str (format-subtask-progress progress))
                                     (indent "  ")
                                     (status-indicator (if (todo-enriching-p todo)
                                                          (tui:colored
                                                           (tui.spinner:spinner-view
                                                            (model-enrichment-spinner model))
                                                           :fg tui:*fg-yellow*)
                                                          (org-status-colored (todo-status todo))))
                                     (schedule-info (format-schedule-info
                                                     (todo-scheduled-date todo)
                                                     (todo-due-date todo)))
                                     (priority-str (org-priority-colored (todo-priority todo)))
                                     (tags-str (org-tags-string (todo-tags todo)))
                                     (tags-colored (if (> (length tags-str) 0)
                                                      (tui:colored tags-str :fg tui:*fg-yellow*)
                                                      ""))
                                     ;; Build prefix with depth indentation and collapse indicator
                                     (prefix (format nil "~A~A~A~A~A ~A "
                                                     depth-indent
                                                     indent
                                                     (or collapse-ind " ")
                                                     schedule-info
                                                     status-indicator
                                                     priority-str))
                                     (prefix-len (tui:visible-length prefix))
                                     (tags-len (tui:visible-length tags-colored))
                                     (progress-len (if progress-str (+ 1 (tui:visible-length progress-str)) 0))
                                     (suffix-len (+ tags-len progress-len (if (and (> tags-len 0) (> progress-len 0)) 1 0)))
                                     (avail (max 0 (- term-width prefix-len (if (> suffix-len 0) (+ suffix-len 1) 0))))
                                     (title-text (todo-title todo))
                                     (trunc-title (if (> (length title-text) avail)
                                                     (concatenate 'string (subseq title-text 0 (max 0 (- avail 2))) "..")
                                                     title-text))
                                     (base (format nil "~A~A" prefix trunc-title))
                                     (base-len (tui:visible-length base))
                                     ;; Build suffix with progress and tags
                                     (suffix (cond
                                               ((and progress-str (> tags-len 0))
                                                (format nil "~A ~A" progress-str tags-colored))
                                               (progress-str progress-str)
                                               ((> tags-len 0) tags-colored)
                                               (t "")))
                                     (suffix-actual-len (tui:visible-length suffix))
                                     (pad (if (> suffix-actual-len 0)
                                              (max 1 (- term-width base-len suffix-actual-len))
                                              0))
                                     (line-content (if (> suffix-actual-len 0)
                                                       (format nil "~A~A~A" base (make-string pad :initial-element #\Space) suffix)
                                                       base)))
                                (if selected-p
                                    (format c "~A"
                                            (tui:colored line-content
                                                        :bg tui:*bg-blue*
                                                        :fg tui:*fg-white*))
                                    (format c "~A" line-content))))
                            (incf lines-rendered)
                            (incf current-idx)))))))))

        (format s "~A~%" content))

      ;; Help bar
      (let* ((help "Keys: jk/↑↓ move  Enter view  SPC cycle  a add  e edit  d del  D del DONE  r refresh  / search  q quit")
             (pad (max 0 (- term-width (length help)))))
        (format s "~A"
                (tui:colored
                 (format nil "~A~A" help (make-string pad :initial-element #\Space))
                 :fg tui:*fg-bright-black*))))))

(defun render-detail-view (model)
  "Render the detail view for a single TODO."
  (handler-case
      (let* ((term-width (model-term-width model))
             (todos (get-visible-todos model))
             (todo (when (< (model-cursor model) (length todos))
                    (nth (model-cursor model) todos))))
        (llog:debug "Rendering detail view"
                    :cursor (model-cursor model)
                    :num-todos (length todos)
                    :has-todo (if todo "yes" "no")
                    :todo-title (when todo (todo-title todo))
                    :has-scheduled (when todo (if (todo-scheduled-date todo) "yes" "no"))
                    :has-due (when todo (if (todo-due-date todo) "yes" "no"))
                    :has-desc (when todo (if (todo-description todo) "yes" "no"))
                    :has-tags (when todo (if (todo-tags todo) "yes" "no"))
                    :has-location (when todo (if (todo-location-info todo) "yes" "no")))
        (if (null todo)
            "No item selected."
        (with-output-to-string (s)
          ;; Title bar
          (let* ((title " ITEM DETAILS ")
                 (pad (max 0 (- term-width (length title)))))
            (format s "~A~%"
                    (tui:bold (tui:colored
                              (format nil "~A~A" title (make-string pad :initial-element #\─))
                              :fg tui:*fg-white* :bg tui:*bg-blue*))))

          ;; Build content
          (let ((content
                  (with-output-to-string (c)
                    ;; Status and Priority
                    (format c "~A ~A~%"
                            (org-status-colored (todo-status todo))
                            (org-priority-colored (todo-priority todo)))

                    ;; Title
                    (format c "~A"
                            (if (eq (todo-status todo) :completed)
                                (tui:colored (todo-title todo) :fg tui:*fg-bright-black*)
                                (tui:bold (todo-title todo))))

                    ;; Scheduled date (when work begins)
                    (when (todo-scheduled-date todo)
                      (format c "~%~%~A <~A>"
                              (tui:bold "SCHEDULED:")
                              (tui:colored
                               (lt:format-timestring nil (todo-scheduled-date todo)
                                                    :format '(:long-weekday " " :short-month " " :day " " :year))
                               :fg tui:*fg-cyan*)))

                    ;; Due date / Deadline (when work must be done)
                    (when (todo-due-date todo)
                      (format c "~%~A <~A>"
                              (tui:bold (tui:colored "DEADLINE:" :fg tui:*fg-red*))
                              (tui:colored
                               (lt:format-timestring nil (todo-due-date todo)
                                                    :format '(:long-weekday " " :short-month " " :day " " :year))
                               :fg tui:*fg-red*)))

                    ;; Description
                    (when (todo-description todo)
                      (format c "~%~%~A" (todo-description todo)))

                    ;; Tags
                    (when (todo-tags todo)
                      (format c "~%~%~A"
                              (tui:colored (org-tags-string (todo-tags todo)) :fg tui:*fg-magenta*)))

                    ;; Estimated time
                    (when (todo-estimated-minutes todo)
                      (format c "~%~%~A"
                              (tui:colored
                               (format nil "Estimated: ~A min" (todo-estimated-minutes todo))
                               :fg tui:*fg-yellow*)))

                    ;; Location info
                    (when (todo-location-info todo)
                      (let ((loc (todo-location-info todo)))
                        (format c "~%~%~A"
                                (tui:bold (tui:colored "📍 Location" :fg tui:*fg-cyan*)))
                        (when (getf loc :name)
                          (format c "~%  ~A" (getf loc :name)))
                        (when (getf loc :address)
                          (format c "~%  ~A"
                                  (tui:colored (getf loc :address) :fg tui:*fg-bright-black*)))
                        (when (getf loc :phone)
                          (format c "~%  Tel: ~A"
                                  (tui:colored (getf loc :phone) :fg tui:*fg-green*)))
                        (when (getf loc :map-url)
                          (format c "~%  ~A"
                                  (tui:colored (getf loc :map-url) :fg tui:*fg-blue*)))
                        (when (getf loc :website)
                          (format c "~%  ~A"
                                  (tui:colored (getf loc :website) :fg tui:*fg-blue*)))))

                    ;; Metadata
                    (format c "~%~%~A"
                            (tui:colored
                             (format nil "Created: ~A"
                                    (lt:format-timestring nil (todo-created-at todo)
                                                         :format '(:short-month " " :day ", " :year)))
                             :fg tui:*fg-bright-black*))

                    (when (todo-completed-at todo)
                      (format c "~%~A"
                              (tui:colored
                               (format nil "Closed:  ~A"
                                      (lt:format-timestring nil (todo-completed-at todo)
                                                           :format '(:short-month " " :day ", " :year)))
                               :fg tui:*fg-bright-black*))))))

            ;; Pad to full width
            (let ((inner-width (- term-width 2)))
              (format s "~A~%"
                      (tui:render-border (pad-content-to-width content inner-width)
                                         tui:*border-double*))))

          ;; Help bar
          (let* ((help " RET/q:back  e:edit  SPC:toggle ")
                 (pad (max 0 (- term-width (length help)))))
            (format s "~A"
                    (tui:colored
                     (format nil "~A~A" help (make-string pad :initial-element #\Space))
                     :fg tui:*fg-yellow* :bg tui:*bg-blue*))))))
    (error (e)
      (llog:error "Error rendering detail view"
                  :error-type (type-of e)
                  :error-message (format nil "~A" e))
      (format nil "Error: ~A" e))))

(defun render-modal-dialog (title content help-text modal-width &key (title-bg tui:*bg-blue*))
  "Render a modal dialog box with title bar, content, and help bar."
  (let ((inner-width (- modal-width 2)))
    (with-output-to-string (s)
      ;; Title bar
      (let ((title-pad (max 0 (- modal-width (length title)))))
        (format s "~A~%"
                (tui:bold (tui:colored
                          (format nil "~A~A" title (make-string title-pad :initial-element #\─))
                          :fg tui:*fg-white* :bg title-bg))))
      ;; Content with border
      (format s "~A~%"
              (tui:render-border (pad-content-to-width content inner-width)
                                 tui:*border-double*))
      ;; Help bar
      (let ((help-pad (max 0 (- modal-width (length help-text)))))
        (format s "~A"
                (tui:colored
                 (format nil "~A~A" help-text (make-string help-pad :initial-element #\Space))
                 :fg tui:*fg-yellow* :bg title-bg))))))

(defun render-add-edit-view (model)
  "Render the add/edit TODO form."
  (llog:info "render-add-edit-view called" :is-edit (model-edit-todo-id model))
  (let* ((term-width (model-term-width model))
         (is-edit (model-edit-todo-id model))
         (title (if is-edit " EDIT ITEM " " NEW ITEM ")))
    (with-output-to-string (s)
      ;; Title bar - using green background to make it obvious
      (let ((pad (max 0 (- term-width (length title)))))
        (format s "~A~%"
                (tui:bold (tui:colored
                          (format nil "~A~A" title (make-string pad :initial-element #\─))
                          :fg tui:*fg-white* :bg tui:*bg-green*))))

      ;; Build content
      (let ((content
              (with-output-to-string (c)
                ;; Title input
                (format c "~A Title: ~A"
                        (if (eq (model-active-field model) :title)
                            (tui:colored ">" :fg tui:*fg-cyan*)
                            " ")
                        (tui.textinput:textinput-view (model-title-input model)))

                ;; Notes input
                (format c "~%~A Notes: ~A"
                        (if (eq (model-active-field model) :description)
                            (tui:colored ">" :fg tui:*fg-cyan*)
                            " ")
                        (tui.textinput:textinput-view (model-description-input model)))

                ;; Priority
                (format c "~%~%~A Priority: ~A ~A ~A"
                        (if (eq (model-active-field model) :priority)
                            (tui:colored ">" :fg tui:*fg-cyan*)
                            " ")
                        (if (eq (model-edit-priority model) :high)
                            (tui:bold (tui:colored "[#A]" :fg tui:*fg-red*))
                            (tui:colored " #A " :fg tui:*fg-bright-black*))
                        (if (eq (model-edit-priority model) :medium)
                            (tui:bold (tui:colored "[#B]" :fg tui:*fg-yellow*))
                            (tui:colored " #B " :fg tui:*fg-bright-black*))
                        (if (eq (model-edit-priority model) :low)
                            (tui:bold (tui:colored "[#C]" :fg tui:*fg-green*))
                            (tui:colored " #C " :fg tui:*fg-bright-black*))))))

        ;; Pad to full width and render with border
        (let ((inner-width (- term-width 2)))
          (format s "~A~%"
                  (tui:render-border (pad-content-to-width content inner-width)
                                     tui:*border-double*))))

      ;; Help bar
      (let* ((help " TAB:next  S-TAB:prev  RET:save  ESC:cancel ")
             (pad (max 0 (- term-width (length help)))))
        (format s "~A"
                (tui:colored
                 (format nil "~A~A" help (make-string pad :initial-element #\Space))
                 :fg tui:*fg-yellow* :bg tui:*bg-blue*))))))

(defun render-search-view (model)
  "Render the search view."
  (let* ((term-width (model-term-width model))
         (todos (get-visible-todos model)))
    (with-output-to-string (s)
      ;; Title bar
      (let* ((title " SEARCH ")
             (pad (max 0 (- term-width (length title)))))
        (format s "~A~%"
                (tui:bold (tui:colored
                          (format nil "~A~A" title (make-string pad :initial-element #\─))
                          :fg tui:*fg-white* :bg tui:*bg-blue*))))

      ;; Build content
      (let ((content
              (with-output-to-string (c)
                (format c "~A" (tui.textinput:textinput-view (model-search-input model)))
                (format c "~%~%~A"
                        (tui:colored (format nil "~D match~:P" (length todos))
                                    :fg tui:*fg-bright-black*))

                (when todos
                  (loop for todo in (subseq todos 0 (min 5 (length todos)))
                        do (format c "~%  ~A ~A ~A"
                                  (org-status-colored (todo-status todo))
                                  (org-priority-colored (todo-priority todo))
                                  (todo-title todo)))
                  (when (> (length todos) 5)
                    (format c "~%  ~A"
                            (tui:colored (format nil "... and ~D more" (- (length todos) 5))
                                        :fg tui:*fg-bright-black*)))))))

        ;; Pad to full width
        (let ((inner-width (- term-width 2)))
          (format s "~A~%"
                  (tui:render-border (pad-content-to-width content inner-width)
                                     tui:*border-double*))))

      ;; Help bar
      (let* ((help " RET:apply  ESC:cancel ")
             (pad (max 0 (- term-width (length help)))))
        (format s "~A"
                (tui:colored
                 (format nil "~A~A" help (make-string pad :initial-element #\Space))
                 :fg tui:*fg-yellow* :bg tui:*bg-blue*))))))

(defun render-delete-confirm-view (model)
  "Render the delete confirmation dialog."
  (let* ((term-width (model-term-width model))
         (todos (get-visible-todos model))
         (todo (when (< (model-cursor model) (length todos))
                (nth (model-cursor model) todos))))
    (with-output-to-string (s)
      ;; Title bar (red for danger)
      (let* ((title " DELETE ITEM ")
             (pad (max 0 (- term-width (length title)))))
        (format s "~A~%"
                (tui:bold (tui:colored
                          (format nil "~A~A" title (make-string pad :initial-element #\─))
                          :fg tui:*fg-white* :bg tui:*bg-red*))))

      (when todo
        (let ((content
                (with-output-to-string (c)
                  (format c "~A~%~%  ~A ~A ~A"
                          (tui:bold "Delete this item?")
                          (org-status-colored (todo-status todo))
                          (org-priority-colored (todo-priority todo))
                          (todo-title todo)))))
          ;; Pad to full width
          (let ((inner-width (- term-width 2)))
            (format s "~A~%"
                    (tui:render-border (pad-content-to-width content inner-width)
                                       tui:*border-double*)))))

      ;; Help bar
      (let* ((help " y:confirm  any other key:cancel ")
             (pad (max 0 (- term-width (length help)))))
        (format s "~A"
                (tui:colored
                 (format nil "~A~A" help (make-string pad :initial-element #\Space))
                 :fg tui:*fg-yellow* :bg tui:*bg-red*))))))

(defun render-delete-done-confirm-view (model)
  "Render the delete-done confirmation dialog as an overlay."
  (let* ((term-width (model-term-width model))
         (term-height (model-term-height model))
         (done-count (count-if (lambda (todo) (eq (todo-status todo) +status-completed+))
                               (model-todos model)))
         (background (render-list-view model))
         (content (with-output-to-string (c)
                    (format c "~A~%~%"
                            (tui:bold (format nil "Delete ~D DONE item~:P?" done-count)))
                    (format c "This will remove completed items from the list.")))
         (max-width (max 20 (- term-width 2)))
         (modal-width (min 72 max-width))
         (modal (render-modal-dialog " DELETE DONE ITEMS " content
                                     " y:confirm  any other key:cancel "
                                     modal-width
                                     :title-bg tui:*bg-red*))
         (modal-height (length (uiop:split-string modal :separator '(#\Newline)))))
    (overlay-modal background modal term-width term-height modal-width modal-height)))

(defun render-context-info-view (model)
  "Render the context info view showing where to edit user context."
  (let* ((term-width (model-term-width model))
         (context-file (namestring (user-context-file)))
         (has-context (load-user-context)))
    (with-output-to-string (s)
      ;; Title bar
      (let* ((title " USER CONTEXT ")
             (pad (max 0 (- term-width (length title)))))
        (format s "~A~%"
                (tui:bold (tui:colored
                          (format nil "~A~A" title (make-string pad :initial-element #\─))
                          :fg tui:*fg-white* :bg tui:*bg-magenta*))))

      ;; Content
      (let ((content
              (with-output-to-string (c)
                (format c "~A~%~%"
                        (tui:bold "User context provides personal information to help"))
                (format c "the AI better understand and enrich your TODOs.~%~%")
                (format c "~A~%"
                        (tui:colored "Context File:" :fg tui:*fg-cyan*))
                (format c "  ~A~%~%"
                        (tui:colored context-file :fg tui:*fg-yellow*))
                (format c "~A~%~%"
                        (tui:colored "To edit, open this file in your favorite editor:" :fg tui:*fg-cyan*))
                (format c "  ~A~%~%"
                        (tui:colored (format nil "  $EDITOR ~A" context-file) :fg tui:*fg-green*))
                (if has-context
                    (format c "~A"
                            (tui:colored "✓ Context file has content" :fg tui:*fg-green*))
                    (format c "~A"
                            (tui:colored "○ Context file is empty - add your info!" :fg tui:*fg-yellow*))))))
        (let ((inner-width (- term-width 2)))
          (format s "~A~%"
                  (tui:render-border (pad-content-to-width content inner-width)
                                     tui:*border-double*))))

      ;; Help bar
      (let* ((help " Press any key to return ")
             (pad (max 0 (- term-width (length help)))))
        (format s "~A"
                (tui:colored
                 (format nil "~A~A" help (make-string pad :initial-element #\Space))
                 :fg tui:*fg-yellow* :bg tui:*bg-magenta*))))))

(defun render-import-view (model)
  "Render the org-mode import view."
  (let* ((term-width (model-term-width model))
         (filename (tui.textinput:textinput-value (model-import-input model)))
         (file-exists (and (> (length filename) 0) (probe-file filename))))
    (with-output-to-string (s)
      ;; Title bar
      (let* ((title " IMPORT ORG-MODE FILE ")
             (pad (max 0 (- term-width (length title)))))
        (format s "~A~%"
                (tui:bold (tui:colored
                          (format nil "~A~A" title (make-string pad :initial-element #\─))
                          :fg tui:*fg-white* :bg tui:*bg-magenta*))))

      ;; Build content
      (let ((content
              (with-output-to-string (c)
                (format c "Enter the path to an org-mode file to import:~%~%")
                (format c "~A~%~%"
                        (tui.textinput:textinput-view (model-import-input model)))
                (cond
                  ((= (length filename) 0)
                   (format c "~A"
                           (tui:colored "Enter a file path above" :fg tui:*fg-bright-black*)))
                  (file-exists
                   (format c "~A"
                           (tui:colored "✓ File found - press Enter to import" :fg tui:*fg-green*)))
                  (t
                   (format c "~A"
                           (tui:colored "✗ File not found" :fg tui:*fg-red*))))
                (format c "~%~%~A"
                        (tui:colored "Non-DONE items will be imported and enriched." :fg tui:*fg-bright-black*)))))

        ;; Pad to full width
        (let ((inner-width (- term-width 2)))
          (format s "~A~%"
                  (tui:render-border (pad-content-to-width content inner-width)
                                     tui:*border-double*))))

      ;; Help bar
      (let* ((help " RET:import  ESC:cancel ")
             (pad (max 0 (- term-width (length help)))))
        (format s "~A"
                (tui:colored
                 (format nil "~A~A" help (make-string pad :initial-element #\Space))
                 :fg tui:*fg-yellow* :bg tui:*bg-magenta*))))))

(defun render-help-view ()
  "Render the help view with keyboard shortcuts."
  (with-output-to-string (s)
    (format s "~A~%~%"
            (tui:bold (tui:colored
                      " KEYBOARD SHORTCUTS                                                    "
                      :fg tui:*fg-white* :bg tui:*bg-blue*)))
    (format s "  ~A~%" (tui:bold (tui:colored "NAVIGATION" :fg tui:*fg-cyan*)))
    (format s "    n / j / Down    Next item~%")
    (format s "    p / k / Up      Previous item~%")
    (format s "    g / Home        First item~%")
    (format s "    G / End         Last item~%")
    (format s "    Enter           View details~%")
    (format s "~%")
    (format s "  ~A~%" (tui:bold (tui:colored "ACTIONS" :fg tui:*fg-cyan*)))
    (format s "    Space           Cycle TODO/DONE/WAITING~%")
    (format s "    a               Add sibling item~%")
    (format s "    A               Add child to item~%")
    (format s "    e               Edit item~%")
    (format s "    d               Delete item~%")
    (format s "    D               Delete DONE items~%")
    (format s "    B/C             Set priority (medium/low)~%")
    (format s "    Shift+Up/Down   Increase/decrease priority~%")
    (format s "    r               Refresh ordering~%")
    (format s "    &               Re-enrich item~%")
    (format s "~%")
    (format s "  ~A~%" (tui:bold (tui:colored "HIERARCHY" :fg tui:*fg-cyan*)))
    (format s "    Tab / >         Indent (make child of prev)~%")
    (format s "    Shift+Tab / <   Outdent (move to parent level)~%")
    (format s "    z               Toggle collapse/expand~%")
    (format s "~%")
    (format s "  ~A~%" (tui:bold (tui:colored "FILTER & SEARCH" :fg tui:*fg-cyan*)))
    (format s "    /               Search~%")
    (format s "    f               Cycle status filter~%")
    (format s "    s               Cycle sort (apply with r)~%")
    (format s "    c               Clear filters~%")
    (format s "~%")
    (format s "  ~A~%" (tui:bold (tui:colored "OTHER" :fg tui:*fg-cyan*)))
    (format s "    i               Import org-mode file~%")
    (format s "    u               Edit user context~%")
    (format s "    ?               This help~%")
    (format s "    q               Quit~%")
    (format s "~%")
    (format s "  ~A~%" (tui:colored "Press any key to return" :fg tui:*fg-bright-black*))))

;;── Main View Method ───────────────────────────────────────────────────────────

(defmethod tui:view ((model app-model))
  "Main view dispatcher based on current view state."
  (llog:debug "Rendering view" :view-state (model-view-state model))
  (case (model-view-state model)
    (:list (render-list-view model))
    (:detail (render-detail-view model))
    ((:add :edit)
     (llog:debug "Rendering add/edit view")
     (render-add-edit-view model))
    (:search (render-search-view model))
    (:import (render-import-view model))
    (:delete-confirm (render-delete-confirm-view model))
    (:delete-done-confirm (render-delete-done-confirm-view model))
    (:help (render-help-view))
    (:context-info (render-context-info-view model))
    (otherwise (render-list-view model))))
