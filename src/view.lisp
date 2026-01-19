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
                     ;; Build: prefix from bg + modal, then pad to full width.
                     (if (> start-col 0)
                         ;; Need to extract prefix from background
                         (let ((prefix (subseq bg-line 0 (min start-col (length bg-line)))))
                           (let* ((line (format nil "~A~A" prefix modal-line))
                                  (pad (max 0 (- term-width (tui:visible-length line)))))
                             (format s "~A~A" line (make-string pad :initial-element #\Space))))
                         (let* ((pad (max 0 (- term-width (tui:visible-length modal-line)))))
                           (format s "~A~A" modal-line (make-string pad :initial-element #\Space)))))
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
  (let ((help "F1:Help | jk:Nav | Enter:View | Space:Done | a:Add | e:Edit | d:Del | /:Search | q:Quit"))
    (render-help-line help width :fg tui:*fg-yellow* :bg tui:*bg-blue*)))

(defun fit-to-width (text width)
  "Truncate or pad text to exactly width characters."
  (let ((safe-width (max 0 width)))
    (if (> (length text) safe-width)
        (subseq text 0 safe-width)
        (format nil "~A~A" text (make-string (- safe-width (length text)) :initial-element #\Space)))))

(defun fit-visible-to-width (text width)
  "Truncate or pad text to exactly width characters using visible length."
  (let* ((safe-width (max 0 width))
         (trimmed (if (> (tui:visible-length text) safe-width)
                      (tui:truncate-text text safe-width :ellipsis "")
                      text))
         (pad (max 0 (- safe-width (tui:visible-length trimmed)))))
    (format nil "~A~A" trimmed (make-string pad :initial-element #\Space))))

(defun render-help-line (text width &key (fg tui:*fg-yellow*) bg)
  "Render a help bar line with truncation and padding."
  (tui:colored (fit-to-width text width) :fg fg :bg bg))

(defun render-scrollbar-lines (viewport height)
  "Render a 1-column scrollbar for a viewport."
  (let* ((total (tui.viewport:viewport-total-lines viewport))
         (visible height))
    (if (<= total visible)
        (make-list height :initial-element " ")
        (let* ((track "│")
               (thumb "█")
               (thumb-size (max 1 (floor (* visible (/ (float visible) total)))))
               (max-top (max 0 (- visible thumb-size)))
               (top (if (> max-top 0)
                        (floor (* max-top (tui.viewport:viewport-scroll-percent viewport)))
                        0)))
          (loop for i from 0 below height
                collect (if (and (>= i top) (< i (+ top thumb-size)))
                            thumb
                            track))))))

(defun pad-to-width (text width)
  "Pad text to exactly width characters using spaces."
  (let ((visible-len (tui:visible-length text)))
    (if (>= visible-len width)
        text
        (format nil "~A~A" text (make-string (- width visible-len) :initial-element #\Space)))))

(defun render-sidebar (model sidebar-width height)
  "Render the tag sidebar. Returns a list of lines, each padded to sidebar-width."
  (let* ((tags (model-all-tags-cache model))
         (selected (model-selected-tags model))
         (cursor (model-sidebar-cursor model))
         (focused (model-sidebar-focused model))
         (presets (model-tag-presets model))
         (lines nil)
         (w (1- sidebar-width)))  ; Leave room for the vertical divider
    ;; Header
    (push (pad-to-width (tui:bold "LABELS") w) lines)
    (push (make-string w :initial-element #\─) lines)

    ;; "All" option (row 0) - selected when no tags are filtered
    (let* ((all-selected (zerop (hash-table-count selected)))
           (is-cursor (and focused (= cursor 0)))
           (checkbox (if all-selected "[x]" "[ ]"))
           (text (format nil "~A All" checkbox))
           (padded (pad-to-width text w)))
      (push (if is-cursor
                (tui:colored padded :bg tui:*bg-blue* :fg tui:*fg-white*)
                padded)
            lines))

    ;; Tag rows
    (loop for tag in tags
          for idx from 1
          do (let* ((is-selected (gethash tag selected))
                    (is-cursor (and focused (= cursor idx)))
                    (checkbox (if is-selected "[x]" "[ ]"))
                    ;; Truncate tag name if too long
                    (max-tag-len (- w 5))
                    (display-tag (if (> (length tag) max-tag-len)
                                     (concatenate 'string
                                                  (subseq tag 0 (max 0 (- max-tag-len 2)))
                                                  "..")
                                     tag))
                    (text (format nil "~A ~A" checkbox display-tag))
                    (padded (pad-to-width text w)))
               (push (cond
                       (is-cursor
                        (tui:colored padded :bg tui:*bg-blue* :fg tui:*fg-white*))
                       (is-selected
                        (tui:colored padded :fg tui:*fg-cyan*))
                       (t padded))
                     lines)))

    ;; Preset section separator
    (push (make-string w :initial-element #\Space) lines)
    (push (make-string w :initial-element #\─) lines)

    ;; Show presets (keys 1-9, 0)
    (when (and presets (arrayp presets))
      (loop for i from 0 below (min 10 (length presets))
            for preset = (aref presets i)
            when (and preset (listp preset))
            do (let* ((key-char (if (= i 9) "0" (format nil "~D" (1+ i))))
                      (tags-preview (format nil "~{~A~^,~}"
                                           (subseq preset 0 (min 2 (length preset)))))
                      (more (if (> (length preset) 2)
                                (format nil "+~D" (- (length preset) 2))
                                ""))
                      (text (format nil "~A: ~A~A" key-char tags-preview more))
                      (display-text (if (> (length text) w)
                                        (subseq text 0 w)
                                        text))
                      (padded (pad-to-width display-text w)))
                 (push (tui:colored padded :fg tui:*fg-bright-black*) lines))))

    ;; Pad to height with empty lines
    (loop while (< (length lines) height)
          do (push (make-string w :initial-element #\Space) lines))

    ;; Return lines in correct order (reversed since we pushed)
    (nreverse lines)))

(defun render-list-content (model list-width)
  "Render just the list content (without sidebar) for the given width."
  (let* ((groups (get-visible-todos-grouped model))
         (todos (loop for (cat . ts) in groups append ts)))
    (with-output-to-string (c)
      (if (null todos)
          (format c "~A" (fit-visible-to-width "No items. Press 'a' to add one." list-width))
          (let ((current-idx 0)
                (first-line t))
            (dolist (group groups)
              (let ((category (car group))
                    (group-todos (cdr group)))
                (unless first-line (format c "~%"))
                (setf first-line nil)
                ;; Render pager-style section header (multi-line)
                (format c "~A" (date-category-colored category list-width))
                (dolist (todo group-todos)
                  (format c "~%")
                  (let* ((all-todos (model-todos model))
                         (selected-p (and (not (model-sidebar-focused model))
                                          (= current-idx (model-cursor model))))
                         ;; Calculate depth for indentation (2 spaces per level)
                         (depth (get-todo-depth all-todos todo))
                         (depth-indent (make-string (* 2 depth) :initial-element #\Space))
                         ;; Check if this todo has children and its collapse state
                         (has-kids (has-children-p all-todos todo))
                         (is-collapsed (todo-collapsed-p model todo))
                         ;; Progress indicator for items with children
                         (progress (count-subtask-progress all-todos (todo-id todo)))
                         (indent "  ")
                         (tags-str (org-tags-string (todo-tags todo))))
                    ;; Build line differently based on selection state
                    (if selected-p
                        ;; SELECTED: Plain text with strong contrast highlight
                        (let* ((collapse-ind (if has-kids (if is-collapsed "▸" "▾") " "))
                               (status-text (if (todo-enriching-p todo)
                                                (tui.spinner:spinner-view
                                                 (model-enrichment-spinner model))
                                                (case (todo-status todo)
                                                  (:completed "DONE")
                                                  (:in-progress "STRT")
                                                  (:pending "TODO")
                                                  (:waiting "WAIT")
                                                  (:cancelled "CNCL")
                                                  (otherwise "    "))))
                               (priority-text (case (todo-priority todo)
                                                (:high "[#A]")
                                                (:medium "[#B]")
                                                (:low "[#C]")
                                                (otherwise "    ")))
                               (schedule-text (format-schedule-info-plain
                                               (todo-scheduled-date todo)
                                               (todo-due-date todo)))
                               (progress-str (when progress
                                               (format nil "~D/~D" (car progress) (cdr progress))))
                               (prefix (format nil "~A~A~A~A~A ~A "
                                               depth-indent indent collapse-ind
                                               schedule-text status-text priority-text))
                               (prefix-len (length prefix))
                               (tags-len (length tags-str))
                               (progress-len (if progress-str (+ 1 (length progress-str)) 0))
                               (suffix-len (+ tags-len progress-len
                                              (if (and (> tags-len 0) (> progress-len 0)) 1 0)))
                               (avail (max 0 (- list-width prefix-len
                                                (if (> suffix-len 0) (+ suffix-len 1) 0))))
                               (title-text (todo-title todo))
                               (trunc-title (if (> (length title-text) avail)
                                                (concatenate 'string
                                                             (subseq title-text 0 (max 0 (- avail 2)))
                                                             "..")
                                                title-text))
                               (base (format nil "~A~A" prefix trunc-title))
                               (base-len (length base))
                               (suffix (cond
                                         ((and progress-str (> tags-len 0))
                                          (format nil "~A ~A" progress-str tags-str))
                                         (progress-str progress-str)
                                         ((> tags-len 0) tags-str)
                                         (t "")))
                               (suffix-actual-len (length suffix))
                               (pad (max 0 (- list-width base-len suffix-actual-len)))
                               (line-content (format nil "~A~A~A" base
                                                     (make-string pad :initial-element #\Space)
                                                     suffix))
                               ;; Pad to exact width for full-row highlight
                               (final-len (length line-content))
                               (final-pad (max 0 (- list-width final-len)))
                               (padded-line (format nil "~A~A" line-content
                                                    (make-string final-pad :initial-element #\Space)))
                               ;; Use dimmer colors for completed/cancelled items
                               (is-done (member (todo-status todo) '(:completed :cancelled))))
                          (format c "~A"
                                  (if is-done
                                      ;; Dimmed selection for done items
                                      (tui:colored padded-line
                                                   :bg tui:*bg-bright-black*
                                                   :fg tui:*fg-white*)
                                      ;; Normal bright selection
                                      (tui:bold
                                       (tui:colored padded-line
                                                    :bg tui:*bg-cyan*
                                                    :fg tui:*fg-black*)))))
                        ;; NOT SELECTED: Normal colored rendering
                        (let* ((collapse-ind (format-collapse-indicator is-collapsed has-kids))
                               (progress-str (format-subtask-progress progress))
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
                               (tags-colored (if (> (length tags-str) 0)
                                                 (tui:colored tags-str :fg tui:*fg-yellow*)
                                                 ""))
                               (prefix (format nil "~A~A~A~A~A ~A "
                                               depth-indent indent (or collapse-ind " ")
                                               schedule-info status-indicator priority-str))
                               (prefix-len (tui:visible-length prefix))
                               (tags-len (tui:visible-length tags-colored))
                               (progress-len (if progress-str (+ 1 (tui:visible-length progress-str)) 0))
                               (suffix-len (+ tags-len progress-len
                                              (if (and (> tags-len 0) (> progress-len 0)) 1 0)))
                               (avail (max 0 (- list-width prefix-len
                                                (if (> suffix-len 0) (+ suffix-len 1) 0))))
                               (title-text (todo-title todo))
                               (trunc-title (if (> (length title-text) avail)
                                                (concatenate 'string
                                                             (subseq title-text 0 (max 0 (- avail 2)))
                                                             "..")
                                                title-text))
                               ;; Dim title for completed/cancelled items (before resort)
                               (styled-title (if (member (todo-status todo) '(:completed :cancelled))
                                                 (tui:colored trunc-title :fg tui:*fg-bright-black*)
                                                 trunc-title))
                               (base (format nil "~A~A" prefix styled-title))
                               (base-len (tui:visible-length base))
                               (suffix (cond
                                         ((and progress-str (> tags-len 0))
                                          (format nil "~A ~A" progress-str tags-colored))
                                         (progress-str progress-str)
                                         ((> tags-len 0) tags-colored)
                                         (t "")))
                               (suffix-actual-len (tui:visible-length suffix))
                               (pad (if (> suffix-actual-len 0)
                                        (max 1 (- list-width base-len suffix-actual-len))
                                        0))
                               (line-content (if (> suffix-actual-len 0)
                                                 (format nil "~A~A~A" base
                                                         (make-string pad :initial-element #\Space)
                                                         suffix)
                                                 base))
                               (clamped-content (fit-visible-to-width line-content list-width)))
                          (format c "~A" clamped-content))))
                  (incf current-idx)))))))))

(defun render-list-view (model)
  "Render the main agenda view."
  (let* ((term-height (model-term-height model))
         (term-width (model-term-width model))
         (sidebar-visible (model-sidebar-visible model))
         (min-list-width 20)
         (raw-sidebar-width (if sidebar-visible 14 0))
         (sidebar-width (if sidebar-visible
                            (max 0 (min raw-sidebar-width (- term-width min-list-width 2)))
                            0))
         (sidebar-visible-effective (> sidebar-width 0))
         (divider-width (if sidebar-visible-effective 1 0))
         (list-width (- term-width sidebar-width divider-width))
         (available-height (max 5 (- term-height 3))))

    (adjust-scroll model available-height)

    (with-output-to-string (s)
      ;; Title + date header (org-agenda style)
      (format s "~A~%" (render-app-title-bar model))
      (format s "~A~%"
              (tui:bold
               (tui:colored
                (lt:format-timestring nil (local-today)
                                      :format '(:long-weekday " " :day " " :long-month " " :year))
                :fg tui:*fg-blue*)))

      ;; Main content area with optional sidebar
      (if sidebar-visible-effective
          ;; Render sidebar and list side by side
          (let* ((sidebar-lines (render-sidebar model sidebar-width available-height))
                 (list-content (render-list-content model list-width))
                 (viewport (tui.viewport:make-viewport
                            :width list-width
                            :height available-height
                            :content list-content))
                 (scrollbar-visible (> (tui.viewport:viewport-total-lines viewport) available-height))
                 (final-list-width (if scrollbar-visible (max 0 (1- list-width)) list-width))
                 (final-content (if scrollbar-visible
                                    (render-list-content model final-list-width)
                                    list-content))
                 (final-viewport (tui.viewport:make-viewport
                                  :width final-list-width
                                  :height available-height
                                  :content final-content))
                 (max-offset (max 0 (- (tui.viewport:viewport-total-lines final-viewport)
                                       available-height)))
                 (offset (min (model-scroll-offset model) max-offset))
                 (list-view (progn
                              (setf (model-scroll-offset model) offset)
                              (setf (tui.viewport:viewport-y-offset final-viewport) offset)
                              (tui.viewport:viewport-view final-viewport)))
                 (list-lines (uiop:split-string list-view :separator '(#\Newline)))
                 (scrollbar-lines (if scrollbar-visible
                                      (render-scrollbar-lines final-viewport available-height)
                                      (make-list available-height :initial-element ""))))
            ;; Combine lines horizontally
            (loop for sidebar-line in sidebar-lines
                  for list-line in list-lines
                  for bar-line in scrollbar-lines
                  for i from 0
                  do (when (> i 0) (format s "~%"))
                     (format s "~A│~A~A" sidebar-line list-line bar-line)))
          ;; No sidebar - just render list content
          (let* ((list-content (render-list-content model list-width))
                 (viewport (tui.viewport:make-viewport
                            :width list-width
                            :height available-height
                            :content list-content))
                 (scrollbar-visible (> (tui.viewport:viewport-total-lines viewport) available-height))
                 (final-list-width (if scrollbar-visible (max 0 (1- list-width)) list-width))
                 (final-content (if scrollbar-visible
                                    (render-list-content model final-list-width)
                                    list-content))
                 (final-viewport (tui.viewport:make-viewport
                                  :width final-list-width
                                  :height available-height
                                  :content final-content))
                 (max-offset (max 0 (- (tui.viewport:viewport-total-lines final-viewport)
                                       available-height)))
                 (offset (min (model-scroll-offset model) max-offset)))
            (setf (model-scroll-offset model) offset)
            (setf (tui.viewport:viewport-y-offset final-viewport) offset)
            (let* ((list-view (tui.viewport:viewport-view final-viewport))
                   (list-lines (uiop:split-string list-view :separator '(#\Newline)))
                   (scrollbar-lines (if scrollbar-visible
                                        (render-scrollbar-lines final-viewport available-height)
                                        (make-list available-height :initial-element ""))))
              (format s "~{~A~^~%~}"
                      (loop for list-line in list-lines
                            for bar-line in scrollbar-lines
                            collect (format nil "~A~A" list-line bar-line))))))

      (format s "~%")

      ;; Help bar
      (let* ((base-help "Keys: jk/↑↓ move  SPC toggle  l sidebar  Tab focus")
             (more-help "  a add  e edit  d del  / search  q quit")
             (help (concatenate 'string base-help more-help)))
        (format s "~A"
                (render-help-line help term-width :fg tui:*fg-bright-black*))))))

(defun render-detail-view (model)
  "Render the detail view for a single TODO."
  (handler-case
      (let* ((term-width (model-term-width model))
             (content-width (- term-width 2))  ; Small margin
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

          ;; Status and Priority line
          (format s "~%~A ~A~%"
                  (org-status-colored (todo-status todo))
                  (org-priority-colored (todo-priority todo)))

          ;; Title (wrapped if needed)
          (let ((title-text (if (eq (todo-status todo) :completed)
                                (tui:colored (todo-title todo) :fg tui:*fg-bright-black*)
                                (tui:bold (todo-title todo)))))
            (format s "~A~%" (tui:wrap-text title-text content-width)))

          ;; Scheduled date
          (when (todo-scheduled-date todo)
            (format s "~%~A <~A>~%"
                    (tui:bold "SCHEDULED:")
                    (tui:colored
                     (lt:format-timestring nil (todo-scheduled-date todo)
                                          :format '(:long-weekday " " :short-month " " :day " " :year))
                     :fg tui:*fg-cyan*)))

          ;; Deadline
          (when (todo-due-date todo)
            (format s "~A <~A>~%"
                    (tui:bold (tui:colored "DEADLINE:" :fg tui:*fg-red*))
                    (tui:colored
                     (lt:format-timestring nil (todo-due-date todo)
                                          :format '(:long-weekday " " :short-month " " :day " " :year))
                     :fg tui:*fg-red*)))

          ;; Repeat info
          (when (and (todo-repeat-interval todo) (todo-repeat-unit todo))
            (format s "~A ~A~%"
                    (tui:bold (tui:colored "REPEAT:" :fg tui:*fg-magenta*))
                    (tui:colored
                     (format-repeat-preset (todo-repeat-interval todo)
                                           (todo-repeat-unit todo))
                     :fg tui:*fg-magenta*)))

          ;; Description (wrapped)
          (when (todo-description todo)
            (format s "~%~A~%"
                    (tui:wrap-text (todo-description todo) content-width)))

          ;; Tags
          (when (todo-tags todo)
            (format s "~%~A~%"
                    (tui:colored (org-tags-string (todo-tags todo)) :fg tui:*fg-magenta*)))

          ;; Estimated time
          (when (todo-estimated-minutes todo)
            (format s "~%~A~%"
                    (tui:colored
                     (format nil "Estimated: ~A min" (todo-estimated-minutes todo))
                     :fg tui:*fg-yellow*)))

          ;; Location info
          (when (todo-location-info todo)
            (let ((loc (todo-location-info todo)))
              (format s "~%~A~%"
                      (tui:bold (tui:colored "📍 Location" :fg tui:*fg-cyan*)))
              (when (getf loc :name)
                (format s "  ~A~%" (getf loc :name)))
              (when (getf loc :address)
                (format s "  ~A~%"
                        (tui:colored (getf loc :address) :fg tui:*fg-bright-black*)))
              (when (getf loc :phone)
                (format s "  Tel: ~A~%"
                        (tui:colored (getf loc :phone) :fg tui:*fg-green*)))
              (when (getf loc :map-url)
                (format s "  ~A~%"
                        (tui:wrap-text (tui:colored (getf loc :map-url) :fg tui:*fg-blue*)
                                       content-width :indent 2 :continuation-indent 4)))
              (when (getf loc :website)
                (format s "  ~A~%"
                        (tui:wrap-text (tui:colored (getf loc :website) :fg tui:*fg-blue*)
                                       content-width :indent 2 :continuation-indent 4)))))

          ;; URL (wrapped)
          (when (todo-url todo)
            (format s "~%~A~%~A~%"
                    (tui:bold (tui:colored "Link:" :fg tui:*fg-cyan*))
                    (tui:wrap-text (tui:colored (todo-url todo) :fg tui:*fg-blue*)
                                   content-width :indent 2 :continuation-indent 2)))

          ;; Metadata
          (format s "~%~A~%"
                  (tui:colored
                   (format nil "Created: ~A"
                          (lt:format-timestring nil (todo-created-at todo)
                                               :format '(:short-month " " :day ", " :year)))
                   :fg tui:*fg-bright-black*))

          (when (todo-completed-at todo)
            (format s "~A~%"
                    (tui:colored
                     (format nil "Closed:  ~A"
                            (lt:format-timestring nil (todo-completed-at todo)
                                                 :format '(:short-month " " :day ", " :year)))
                     :fg tui:*fg-bright-black*)))

          ;; Help bar
          (let ((help " RET/q:back  e:edit  s:schedule  d:deadline  SPC:toggle "))
            (format s "~%~A"
                    (render-help-line help term-width :fg tui:*fg-yellow* :bg tui:*bg-blue*))))))
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
      (format s "~A"
              (render-help-line help-text modal-width :fg tui:*fg-yellow* :bg title-bg)))))

(defun format-repeat-preset (interval unit)
  "Format repeat interval and unit as a display string."
  (cond
    ((or (null interval) (null unit)) "None")
    ((and (= interval 1) (eq unit :day)) "Daily")
    ((and (= interval 1) (eq unit :week)) "Weekly")
    ((and (= interval 2) (eq unit :week)) "Biweekly")
    ((and (= interval 1) (eq unit :month)) "Monthly")
    ((and (= interval 1) (eq unit :year)) "Yearly")
    (t (format nil "Every ~D ~A~P" interval
               (string-downcase (symbol-name unit)) interval))))

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
                (format c "~A ~A"
                        (if (eq (model-active-field model) :title)
                            (tui:colored ">" :fg tui:*fg-cyan*)
                            " ")
                        (tui.textinput:textinput-view (model-title-input model)))

                ;; Notes input
                (format c "~%~A ~A"
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
                            (tui:colored " #C " :fg tui:*fg-bright-black*)))

                ;; Scheduled date
                (let* ((sched-date (model-edit-scheduled-date model))
                       (sched-str (if sched-date
                                      (lt:format-timestring nil sched-date
                                                          :format '(:short-month " " :day ", " :year))
                                      "Not set")))
                  (format c "~%~A Scheduled: ~A~A"
                          (if (eq (model-active-field model) :scheduled)
                              (tui:colored ">" :fg tui:*fg-cyan*)
                              " ")
                          (if sched-date
                              (tui:colored (format nil "[~A]" sched-str) :fg tui:*fg-cyan*)
                              (tui:colored (format nil "[~A]" sched-str) :fg tui:*fg-bright-black*))
                          (if (eq (model-active-field model) :scheduled)
                              (tui:colored "  (Space to pick)" :fg tui:*fg-bright-black*)
                              "")))

                ;; Due date
                (let* ((due-date (model-edit-due-date model))
                       (due-str (if due-date
                                    (lt:format-timestring nil due-date
                                                        :format '(:short-month " " :day ", " :year))
                                    "Not set")))
                  (format c "~%~A Due:       ~A~A"
                          (if (eq (model-active-field model) :due)
                              (tui:colored ">" :fg tui:*fg-cyan*)
                              " ")
                          (if due-date
                              (tui:colored (format nil "[~A]" due-str) :fg tui:*fg-red*)
                              (tui:colored (format nil "[~A]" due-str) :fg tui:*fg-bright-black*))
                          (if (eq (model-active-field model) :due)
                              (tui:colored "  (Space to pick)" :fg tui:*fg-bright-black*)
                              "")))

                ;; Repeat
                (let ((repeat-str (format-repeat-preset
                                   (model-edit-repeat-interval model)
                                   (model-edit-repeat-unit model))))
                  (format c "~%~A Repeat:    ~A~A"
                          (if (eq (model-active-field model) :repeat)
                              (tui:colored ">" :fg tui:*fg-cyan*)
                              " ")
                          (if (model-edit-repeat-interval model)
                              (tui:colored (format nil "[~A]" repeat-str) :fg tui:*fg-magenta*)
                              (tui:colored (format nil "[~A]" repeat-str) :fg tui:*fg-bright-black*))
                          (if (eq (model-active-field model) :repeat)
                              (tui:colored "  (←/→ to change)" :fg tui:*fg-bright-black*)
                              "")))

                ;; Labels/Tags
                (let* ((edit-tags (model-edit-tags model))
                       (tags-focused (eq (model-active-field model) :tags)))
                  (format c "~%~A Labels:    "
                          (if tags-focused
                              (tui:colored ">" :fg tui:*fg-cyan*)
                              " "))
                  ;; Show current tags as chips
                  (if edit-tags
                      (format c "~{~A~^ ~}"
                              (mapcar (lambda (tag)
                                        (tui:colored (format nil "[~A]" tag) :fg tui:*fg-magenta*))
                                      (reverse edit-tags)))
                      (format c "~A" (tui:colored "(none)" :fg tui:*fg-bright-black*)))
                  ;; Show input when field is active
                  (when tags-focused
                    (format c " ~A" (tui.textinput:textinput-view (model-tags-input model)))
                    ;; Show dropdown if visible
                    (when (model-tag-dropdown-visible model)
                      (let* ((filtered (model-tag-dropdown-filtered model))
                             (cursor (model-tag-dropdown-cursor model))
                             (max-items 5)
                             (display-items (subseq filtered 0 (min max-items (length filtered)))))
                        (format c "~%             ")  ; Indent to align with input
                        (loop for tag in display-items
                              for idx from 0
                              do (format c "~%             ~A"
                                         (if (= idx cursor)
                                             (tui:colored (format nil "> ~A" tag) :bg tui:*bg-cyan* :fg tui:*fg-black*)
                                             (format nil "  ~A" tag)))))))))))

        ;; Pad to full width and render with border
        (let ((inner-width (- term-width 2)))
          (format s "~A~%"
                  (tui:render-border (pad-content-to-width content inner-width)
                                     tui:*border-double*))))

      ;; Help bar
      (let ((help " TAB:next  S-TAB:prev  SPC:pick date  RET:save  ESC:cancel "))
        (format s "~A"
                (render-help-line help term-width :fg tui:*fg-yellow* :bg tui:*bg-blue*))))))

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
      (let ((help " RET:apply  ESC:cancel "))
        (format s "~A"
                (render-help-line help term-width :fg tui:*fg-yellow* :bg tui:*bg-blue*))))))

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
      (let ((help " y:confirm  any other key:cancel "))
        (format s "~A"
                (render-help-line help term-width :fg tui:*fg-yellow* :bg tui:*bg-red*))))))

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
      (let ((help " Press any key to return "))
        (format s "~A"
                (render-help-line help term-width :fg tui:*fg-yellow* :bg tui:*bg-magenta*))))))

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
      (let ((help " RET:import  ESC:cancel "))
        (format s "~A"
                (render-help-line help term-width :fg tui:*fg-yellow* :bg tui:*bg-magenta*))))))

(defun render-date-edit-view (model)
  "Render the date picker view for editing scheduled/deadline dates."
  (let* ((term-width (model-term-width model))
         (picker (model-date-picker model))
         (date-type (model-editing-date-type model))
         (todos (get-visible-todos model))
         (todo (when (< (model-cursor model) (length todos))
                 (nth (model-cursor model) todos)))
         (title (if (eq date-type :scheduled) " SET SCHEDULED DATE " " SET DEADLINE "))
         (current-date (case date-type
                         (:scheduled (when todo (todo-scheduled-date todo)))
                         (:deadline (when todo (todo-due-date todo)))))
         (selected (tui.datepicker:datepicker-selected picker)))
    (with-output-to-string (s)
      ;; Title bar
      (let* ((pad (max 0 (- term-width (length title)))))
        (format s "~A~%"
                (tui:bold (tui:colored
                          (format nil "~A~A" title (make-string pad :initial-element #\─))
                          :fg tui:*fg-white* :bg tui:*bg-magenta*))))

      ;; Show todo title context
      (when todo
        (format s "~%~A~%"
                (tui:colored (todo-title todo) :fg tui:*fg-bright-black*)))

      ;; Current date status
      (format s "~%~A ~A~%"
              (tui:bold (if (eq date-type :scheduled) "SCHEDULED:" "DEADLINE:"))
              (if current-date
                  (tui:colored
                   (lt:format-timestring nil current-date
                                        :format '(:long-weekday " " :short-month " " :day " " :year))
                   :fg tui:*fg-cyan*)
                  (tui:colored "Not set" :fg tui:*fg-bright-black*)))

      ;; Datepicker calendar
      (format s "~%~A~%" (tui.datepicker:datepicker-view picker))

      ;; Selected date
      (format s "~%~A ~A~%"
              (tui:bold "Selected:")
              (if selected
                  (multiple-value-bind (sec min hour day month year)
                      (decode-universal-time selected)
                    (declare (ignore sec min hour))
                    (tui:colored (format nil "~A ~D, ~D"
                                         (aref #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                                                "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                                               (1- month))
                                         day year)
                                :fg tui:*fg-green*))
                  (tui:colored "None" :fg tui:*fg-bright-black*)))

      ;; Navigation help
      (format s "~%~A~%"
              (tui:colored "Navigation: ←→ or h/l = day  ↑↓ or j/k = week  [/] = month  {/} = year  Home = today"
                          :fg tui:*fg-bright-black*))
      (format s "~A~%"
              (tui:colored "Actions:    Space = select date  Enter = save & close  Backspace = clear  Esc = cancel"
                          :fg tui:*fg-bright-black*))

      ;; Help bar
      (let ((help " ←↑↓→/hjkl:day/week  []:month  {}:year  Home:today  Space:select  Enter:save  Del:clear  Esc:cancel "))
        (format s "~%~A"
                (render-help-line help term-width :fg tui:*fg-yellow* :bg tui:*bg-magenta*))))))

(defun render-help-column (title entries &optional (key-width 10))
  "Render a help column with TITLE and list of (key . description) ENTRIES."
  (with-output-to-string (s)
    (format s "~A~%" (tui:bold (tui:colored title :fg tui:*fg-cyan*)))
    (dolist (entry entries)
      (let ((key (car entry))
            (desc (cdr entry)))
        (if (string= key "")
            ;; Empty key = section header or blank line
            (if (string= desc "")
                (format s "~%")
                (format s "~A~%" (tui:colored desc :fg tui:*fg-bright-black*)))
            ;; Normal key-description pair
            (format s "~A  ~A~%"
                    (tui:colored (format nil "~vA" key-width key) :fg tui:*fg-yellow*)
                    desc))))))

(defun render-help-modal-content ()
  "Render the help modal content with multiple columns using join-horizontal."
  (let* ((col1 (render-help-column "NAVIGATION"
                '(("j/k ↑↓" . "Navigate")
                  ("g/G" . "First/Last")
                  ("PgUp/Dn" . "Page up/down")
                  ("Enter" . "View details")
                  ("Space" . "Cycle status")
                  ("a/A" . "Add/Add child")
                  ("e" . "Edit item")
                  ("d/D" . "Delete item/done")
                  ("B/C" . "Priority B/C")
                  ("S-↑/↓" . "Priority +/-"))))
         (col2 (render-help-column "ORGANIZE"
                '((">" . "Indent (child)")
                  ("<" . "Outdent")
                  ("z" . "Collapse/expand")
                  ("t" . "Edit tags")
                  ("/" . "Search")
                  ("f" . "Filter status")
                  ("s" . "Cycle sort")
                  ("c" . "Clear filters")
                  ("S/L" . "Set sched/due")
                  ("&" . "Re-enrich"))))
         (col3 (render-help-column "SIDEBAR"
                '(("l" . "Toggle sidebar")
                  ("Tab" . "Focus sidebar")
                  ("1-0" . "Apply preset")
                  ("" . "When focused:")
                  ("j/k" . "Navigate tags")
                  ("Space" . "Toggle tag")
                  ("a" . "Select all")
                  ("!@#..." . "Save preset")
                  ("Esc" . "Return to list")
                  ("" . ""))))
         (col4 (render-help-column "OTHER"
                '(("i" . "Import org")
                  ("u" . "User context")
                  ("?" . "This help")
                  ("q" . "Quit")
                  ("" . "Datepicker:")
                  ("hjkl" . "Navigate")
                  ("[/]" . "Month +/-")
                  ("{/}" . "Year +/-")
                  ("Home" . "Today")
                  ("Del" . "Clear date")))))
    ;; Create vertical separator spanning all rows
    (let* ((num-rows 11)  ; title + 10 entries
           (sep-line (tui:colored " │ " :fg tui:*fg-bright-black*))
           (sep (format nil "~{~A~^~%~}" (loop repeat num-rows collect sep-line))))
      (tui:join-horizontal :top col1 sep col2 sep col3 sep col4))))

(defun render-box-with-title (title content)
  "Render a box with a title label on the top border line.
   ╭──┤ TITLE ├─────────────────────────╮
   │ content                            │
   ╰────────────────────────────────────╯"
  (let* ((content-lines (tui:split-string-by-newline content))
         (content-width (if content-lines
                            (apply #'max (mapcar #'tui:visible-length content-lines))
                            40))
         (indent 2)  ; Title starts 2 chars from left
         (padded-title (format nil " ~A " title))
         (title-len (length padded-title))
         ;; Box inner width = content + 2 for padding
         (box-inner-width (+ content-width 2))
         ;; Width after title to right edge
         (right-width (max 0 (- box-inner-width indent title-len 2)))  ; -2 for ┤ and ├
         (reset (format nil "~C[0m" #\Escape)))  ; ANSI reset to prevent color bleed
    (with-output-to-string (s)
      ;; Top border with embedded title
      (format s "~A╭~A┤~A├~A╮~%"
              reset
              (make-string indent :initial-element #\─)
              (tui:bold padded-title)
              (make-string right-width :initial-element #\─))
      ;; Content rows
      (dolist (line content-lines)
        (let ((padding (- content-width (tui:visible-length line))))
          (format s "~A│ ~A~A │~%"
                  reset
                  line
                  (make-string padding :initial-element #\Space))))
      ;; Bottom border
      (format s "~A╰~A╯"
              reset
              (make-string box-inner-width :initial-element #\─)))))

(defun render-help-view (model)
  "Render the help view as an overlay dialog on the list view."
  (let* ((background (render-list-view model))
         (content (render-help-modal-content))
         (footer (tui:colored "Press any key to close" :fg tui:*fg-bright-black*))
         (inner (format nil "~%~A~%~A" content footer))
         (modal (render-box-with-title "KEYBOARD SHORTCUTS" inner)))
    (tui:overlay-centered modal background)))

(defun render-inline-tag-editor (model)
  "Render the inline tag editor as an overlay on the list view."
  (let* ((background (render-list-view model))
         (todos (get-visible-todos model))
         (todo (when (and (model-edit-todo-id model)
                         (< (model-cursor model) (length todos)))
                 (find (model-edit-todo-id model) todos :key #'todo-id :test #'string=)))
         (edit-tags (model-edit-tags model))
         (title (if todo
                    (let ((t-title (todo-title todo)))
                      (if (> (length t-title) 40)
                          (concatenate 'string (subseq t-title 0 38) "..")
                          t-title))
                    "(no task selected)"))
         (content
           (with-output-to-string (c)
             ;; Todo title (truncated)
             (format c "~A~%" (tui:colored title :fg tui:*fg-bright-black*))
             ;; Separator
             (format c "~A~%" (make-string 42 :initial-element #\─))
             ;; Current tags
             (format c "Tags: ")
             (if edit-tags
                 (format c "~{~A~^ ~}~%"
                         (mapcar (lambda (tag)
                                   (tui:colored (format nil "[~A]" tag) :fg tui:*fg-magenta*))
                                 (reverse edit-tags)))
                 (format c "~A~%" (tui:colored "(none)" :fg tui:*fg-bright-black*)))
             ;; Blank line
             (format c "~%")
             ;; Input field
             (format c "Add: ~A~%" (tui.textinput:textinput-view (model-tags-input model)))
             ;; Dropdown if visible
             (when (model-tag-dropdown-visible model)
               (let* ((filtered (model-tag-dropdown-filtered model))
                      (cursor (model-tag-dropdown-cursor model))
                      (max-items 5)
                      (display-items (subseq filtered 0 (min max-items (length filtered)))))
                 (loop for tag in display-items
                       for idx from 0
                       do (format c "      ~A~%"
                                  (if (= idx cursor)
                                      (tui:colored (format nil "> ~A" tag) :bg tui:*bg-cyan* :fg tui:*fg-black*)
                                      (format nil "  ~A" tag))))))
             ;; Blank line before help
             (format c "~%")
             ;; Help line
             (format c "~A"
                     (tui:colored "Enter:add/save  Backspace:remove  Esc:save" :fg tui:*fg-bright-black*))))
         (modal (render-box-with-title "EDIT TAGS" content)))
    (tui:overlay-centered modal background)))

(defun render-form-date-edit-view (model)
  "Render the date picker view for add/edit form scheduled/due dates."
  (let* ((term-width (model-term-width model))
         (picker (model-date-picker model))
         (date-type (model-editing-date-type model))
         (is-edit (model-edit-todo-id model))
         (title (format nil " SET ~A DATE (~A) "
                        (if (eq date-type :scheduled) "SCHEDULED" "DUE")
                        (if is-edit "Edit" "New")))
         (current-date (case date-type
                         (:scheduled (model-edit-scheduled-date model))
                         (:due (model-edit-due-date model))))
         (selected (tui.datepicker:datepicker-selected picker)))
    (with-output-to-string (s)
      ;; Title bar
      (let* ((pad (max 0 (- term-width (length title)))))
        (format s "~A~%"
                (tui:bold (tui:colored
                          (format nil "~A~A" title (make-string pad :initial-element #\─))
                          :fg tui:*fg-white* :bg tui:*bg-magenta*))))

      ;; Current date status
      (format s "~%~A ~A~%"
              (tui:bold (if (eq date-type :scheduled) "SCHEDULED:" "DUE:"))
              (if current-date
                  (tui:colored
                   (lt:format-timestring nil current-date
                                        :format '(:long-weekday " " :short-month " " :day " " :year))
                   :fg tui:*fg-cyan*)
                  (tui:colored "Not set" :fg tui:*fg-bright-black*)))

      ;; Datepicker calendar
      (format s "~%~A~%" (tui.datepicker:datepicker-view picker))

      ;; Selected date
      (format s "~%~A ~A~%"
              (tui:bold "Selected:")
              (if selected
                  (multiple-value-bind (sec min hour day month year)
                      (decode-universal-time selected)
                    (declare (ignore sec min hour))
                    (tui:colored (format nil "~A ~D, ~D"
                                         (aref #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                                                "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                                               (1- month))
                                         day year)
                                :fg tui:*fg-green*))
                  (tui:colored "None" :fg tui:*fg-bright-black*)))

      ;; Navigation help
      (format s "~%~A~%"
              (tui:colored "Navigation: ←→ or h/l = day  ↑↓ or j/k = week  [/] = month  {/} = year  Home = today"
                          :fg tui:*fg-bright-black*))
      (format s "~A~%"
              (tui:colored "Actions:    Space = select date  Enter = save & close  Backspace = clear  Esc = cancel"
                          :fg tui:*fg-bright-black*))

      ;; Help bar
      (let ((help " ←↑↓→/hjkl:day/week  []:month  {}:year  Home:today  Space:select  Enter:save  Del:clear  Esc:cancel "))
        (format s "~%~A"
                (render-help-line help term-width :fg tui:*fg-yellow* :bg tui:*bg-magenta*))))))

;;── Modal Overlay Helpers ──────────────────────────────────────────────────────

(defun render-date-modal-box (model)
  "Render just the datepicker modal box content."
  (let* ((picker (model-date-picker model))
         (date-type (model-editing-date-type model))
         (todos (get-visible-todos model))
         (todo (when (< (model-cursor model) (length todos))
                 (nth (model-cursor model) todos)))
         (current-date (when todo
                         (case date-type
                           (:scheduled (todo-scheduled-date todo))
                           (:deadline (todo-due-date todo)))))
         (cursor-date (tui.datepicker:datepicker-time picker))
         (modal-width 50)
         (title (if (eq date-type :scheduled) "SET SCHEDULED DATE" "SET DEADLINE"))
         (todo-title (if todo
                        (let ((t-title (todo-title todo)))
                          (subseq t-title 0 (min (length t-title) (- modal-width 6))))
                        "(no task selected)"))
         (current-str (if current-date
                         (lt:format-timestring nil current-date
                                              :format '(:short-month " " :day ", " :year))
                         "Not set"))
         (cursor-str (when cursor-date
                       (multiple-value-bind (sec min hour day month year)
                           (decode-universal-time cursor-date)
                         (declare (ignore sec min hour))
                         (format nil "~A ~D, ~D"
                                (aref #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                                       "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                                      (1- month))
                                day year))))
         (picker-view (tui.datepicker:datepicker-view picker))
         (picker-lines (uiop:split-string picker-view :separator '(#\Newline)))
         (inner-width (- modal-width 4)))
    (with-output-to-string (s)
      ;; Title bar (magenta background)
      (format s "~A~%"
              (tui:colored (format nil " ~A~A "
                                  title
                                  (make-string (- inner-width (length title)) :initial-element #\Space))
                          :bg tui:*bg-magenta* :fg tui:*fg-white*))

      ;; Todo title (dimmed)
      (format s "~A~%"
              (tui:colored (format nil " ~A~A "
                                  todo-title
                                  (make-string (max 0 (- inner-width (length todo-title))) :initial-element #\Space))
                          :fg tui:*fg-bright-black*))

      ;; Separator
      (format s "~A~%" (make-string (+ inner-width 2) :initial-element #\─))

      ;; Current value
      (format s " Current: ~A~%"
              (tui:colored current-str :fg tui:*fg-cyan*))

      ;; Blank line
      (format s "~%")

      ;; Datepicker calendar (center each line - variable rows)
      (loop for picker-line in picker-lines
            for content = (string-trim '(#\Space #\Newline) picker-line)
            when (> (length content) 0)
            do (let* ((vis-len (tui:visible-length content))
                      (pad-left (max 0 (floor (- inner-width vis-len) 2))))
                 (format s "~A~A~%"
                         (make-string pad-left :initial-element #\Space)
                         content)))

      ;; Blank line
      (format s "~%")

      ;; Cursor position (what Enter will save)
      (format s " New: ~A~%"
              (tui:colored (or cursor-str "None") :fg tui:*fg-green*))

      ;; Help line
      (format s "~A~%"
              (tui:colored " ←↑↓→:nav  Home:today  RET:confirm  DEL:clear  ESC:cancel"
                          :fg tui:*fg-bright-black*)))))

(defun render-list-date-modal (model)
  "Render the list view with a datepicker modal overlay."
  (let* (;; Render the list view as background
         (background (render-list-view model))
         ;; Build the modal box with border
         (modal-content (render-date-modal-box model))
         (modal-bordered (tui:render-border modal-content tui:*border-rounded*
                                           :fg-color tui:*fg-blue*)))
    ;; Use the new overlay-centered to composite the modal on top
    (tui:overlay-centered modal-bordered background)))

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
    (:edit-date (render-date-edit-view model))
    (:list-set-date (render-list-date-modal model))
    ((:add-scheduled-date :add-due-date) (render-form-date-edit-view model))
    (:help (render-help-view model))
    (:inline-tags (render-inline-tag-editor model))
    (:context-info (render-context-info-view model))
    (otherwise (render-list-view model))))
