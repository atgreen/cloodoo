;;; view.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cloodoo)

;;── Terminal Size Constants ────────────────────────────────────────────────────

(defparameter *min-terminal-width* 60
  "Minimum terminal width required for the UI to render properly.")

(defparameter *dialog-width-percent* 0.7
  "Dialog width as a percentage of terminal width.")

(defparameter *dialog-max-width* 65
  "Maximum dialog width in characters.")

(defparameter *dialog-min-width* 50
  "Minimum dialog width in characters.")

(defun render-terminal-too-narrow (model)
  "Render a warning that the terminal is too narrow."
  (let* ((term-width (model-term-width model))
         (term-height (model-term-height model))
         (msg1 "Terminal too narrow")
         (msg2 (format nil "Need ~D cols, have ~D" *min-terminal-width* term-width)))
    (with-output-to-string (s)
      (let ((blank-lines (floor (- term-height 2) 2)))
        ;; Fill with blank lines to center vertically
        (dotimes (i blank-lines)
          (format s "~%"))
        ;; Centered warning
        (let ((pad1 (max 0 (floor (- term-width (length msg1)) 2)))
              (pad2 (max 0 (floor (- term-width (length msg2)) 2))))
          (format s "~A~A~%"
                  (make-string pad1 :initial-element #\Space)
                  (tui:bold (tui:colored msg1 :fg tui:*fg-red*)))
          (format s "~A~A"
                  (make-string pad2 :initial-element #\Space)
                  (tui:colored msg2 :fg tui:*fg-bright-black*)))))))

(defun calculate-dialog-width (term-width)
  "Calculate the dialog width based on terminal width."
  (let ((percent-width (floor (* term-width *dialog-width-percent*))))
    (max *dialog-min-width*
         (min *dialog-max-width* percent-width))))

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
                         (let ((pad (max 0 (- term-width (tui:visible-length modal-line)))))
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

(defun format-sync-status (model)
  "Format the sync status for display in the header."
  (let ((status (model-sync-status model)))
    (case status
      (:disconnected "")
      (:connecting
       (tui:colored "⟳ Connecting" :fg tui:*fg-yellow*))
      (:connected
       (tui:colored "● Synced" :fg tui:*fg-green*))
      (:error
       (tui:colored "✗ Sync Error" :fg tui:*fg-red*))
      (otherwise ""))))

(defun count-active-todos (model)
  "Count the number of non-completed, non-cancelled todos."
  (count-if (lambda (todo)
              (not (member (todo-status todo) '(:completed :cancelled))))
            (model-todos model)))

(defun render-app-title-bar (model)
  "Render the application title bar with version, date, sync status, and task count."
  (let* ((width (model-term-width model))
         ;; Left: version
         (version-str (format nil "cloodoo ~A" +version+))
         ;; Middle: date - use (lt:now) for correct weekday calculation
         (date-str (lt:format-timestring nil (lt:now)
                                         :format '(:short-weekday " " :short-month " " :day ", " :year)))
         ;; Right: sync status + task count
         (sync-str (format-sync-status model))
         (sync-visible-len (tui:visible-length sync-str))
         (task-count (count-active-todos model))
         (task-str (format nil "~D task~:P" task-count))
         ;; Build the right side with sync status and task count
         (right-str (if (plusp sync-visible-len)
                        (format nil "~A │ ~A" sync-str task-str)
                        task-str))
         (right-visible-len (tui:visible-length right-str))
         ;; Calculate spacing
         (left-len (length version-str))
         (date-len (length date-str))
         ;; We want: [version] ... [date] ... [right]
         ;; Total width = left-len + gap1 + date-len + gap2 + right-len
         ;; Center the date: gap1 = gap2 ideally
         (total-content (+ left-len date-len right-visible-len))
         (total-gaps (max 0 (- width total-content)))
         (gap1 (max 1 (floor total-gaps 2)))
         (gap2 (max 1 (- total-gaps gap1))))
    ;; Dark background with light text for header bar
    (tui:colored
     (format nil "~A~A~A~A~A"
             version-str
             (make-string gap1 :initial-element #\Space)
             date-str
             (make-string gap2 :initial-element #\Space)
             right-str)
     :fg tui:*fg-white* :bg tui:*bg-blue*)))

(defun render-help-bar-line (width)
  "Render the help bar."
  (let ((help "F1:Help | jk:Nav | Enter:View | Space:Done | a:Add | e:Edit | DEL:Del | /:Search | q:Quit"))
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
         (lines nil)
         (w (1- sidebar-width)))  ; Leave room for the vertical divider
    ;; Header
    (push (pad-to-width (tui:bold "LABELS") w) lines)
    (push (make-string w :initial-element #\─) lines)

    ;; "All" option (row 0) - selected when no tags are filtered
    (let* ((all-selected (zerop (hash-table-count selected)))
           (is-cursor (and focused (zerop cursor)))
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
              (let ((category (first group))
                    (group-todos (rest group)))
                (unless first-line (format c "~%"))
                (setf first-line nil)
                ;; Render pager-style section header (multi-line)
                (format c "~A" (date-category-colored category list-width))
                (dolist (todo group-todos)
                  (format c "~%")
                  (let* ((selected-p (and (not (model-sidebar-focused model))
                                          (= current-idx (model-cursor model))))
                         (indent "  ")
                         (tags-str (org-tags-string (todo-tags todo))))
                    ;; Build line differently based on selection state
                    (if selected-p
                        ;; SELECTED: Plain text with strong contrast highlight
                        (let* ((status-text (if (todo-enriching-p todo)
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
                               (prefix (format nil "~A~A~A ~A "
                                               indent schedule-text status-text priority-text))
                               (prefix-len (tui:visible-length prefix))
                               (tags-len (tui:visible-length tags-str))
                               (suffix-len tags-len)
                               (avail (max 0 (- list-width prefix-len
                                                (if (> suffix-len 0) (1+ suffix-len) 0))))
                               (title-text (sanitize-title-for-display (todo-title todo)))
                               (trunc-title (if (> (tui:visible-length title-text) avail)
                                                (concatenate 'string
                                                             (subseq title-text 0 (max 0 (- avail 2)))
                                                             "..")
                                                title-text))
                               (base (format nil "~A~A" prefix trunc-title))
                               (base-len (tui:visible-length base))
                               (suffix (if (> tags-len 0) tags-str ""))
                               (suffix-actual-len (tui:visible-length suffix))
                               (pad (max 0 (- list-width base-len suffix-actual-len)))
                               (line-content (format nil "~A~A~A" base
                                                     (make-string pad :initial-element #\Space)
                                                     suffix))
                               ;; Pad to exact width for full-row highlight
                               (final-len (tui:visible-length line-content))
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
                        (let* ((status-indicator (if (todo-enriching-p todo)
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
                               (prefix (format nil "~A~A~A ~A "
                                               indent schedule-info status-indicator priority-str))
                               (prefix-len (tui:visible-length prefix))
                               (tags-len (tui:visible-length tags-colored))
                               (suffix-len tags-len)
                               (avail (max 0 (- list-width prefix-len
                                                (if (> suffix-len 0) (1+ suffix-len) 0))))
                               (title-text (sanitize-title-for-display (todo-title todo)))
                               (trunc-title (if (> (length title-text) avail)
                                                (concatenate 'string
                                                             (subseq title-text 0 (max 0 (- avail 2)))
                                                             "..")
                                                title-text))
                               ;; Dim and strikethrough title for completed/cancelled items
                               (styled-title (if (member (todo-status todo) '(:completed :cancelled))
                                                 (tui:render-styled
                                                  (tui:make-style :strikethrough t
                                                                  :foreground tui:*fg-bright-black*)
                                                  trunc-title)
                                                 trunc-title))
                               (base (format nil "~A~A" prefix styled-title))
                               (base-len (tui:visible-length base))
                               (suffix (if (> tags-len 0) tags-colored ""))
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

(defun has-active-filters-p (model)
  "Check if any filters are currently active."
  (or (plusp (length (model-search-query model)))
      (model-filter-status model)
      (model-filter-priority model)
      (plusp (hash-table-count (model-selected-tags model)))))

(defun render-filter-banner (model term-width)
  "Render a banner showing active filters."
  (let ((search (model-search-query model))
        (status (model-filter-status model))
        (priority (model-filter-priority model))
        (tags (model-selected-tags model))
        (parts nil))
    ;; Build filter description parts
    (when (plusp (length search))
      (push (format nil "Search: \"~A\"" search) parts))
    (when status
      (push (format nil "Status: ~A" (string-capitalize (symbol-name status))) parts))
    (when priority
      (push (format nil "Priority: ~A" (string-capitalize (symbol-name priority))) parts))
    (when (plusp (hash-table-count tags))
      (push (format nil "Labels: ~D selected" (hash-table-count tags)) parts))

    (when parts
      (let* ((filter-text (format nil "Filtered: ~{~A~^, ~}" (nreverse parts)))
             (clear-hint " (press 'c' to clear)")
             (full-text (concatenate 'string filter-text clear-hint))
             (padding (max 0 (- term-width (length full-text)))))
        (tui:colored
         (format nil "~A~A~A"
                 filter-text
                 (tui:colored clear-hint :fg tui:*fg-bright-black*)
                 (make-string padding :initial-element #\Space))
         :fg tui:*fg-black* :bg tui:*bg-yellow*)))))

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
         (has-filters (has-active-filters-p model))
         (filter-banner-height (if has-filters 1 0))
         (available-height (max 5 (- term-height 3 filter-banner-height))))

    (adjust-scroll model available-height)

    (with-output-to-string (s)
      ;; Header bar with version, date, sync status, task count
      (format s "~A~%" (render-app-title-bar model))

      ;; Filter banner (if any filters are active)
      (when has-filters
        (format s "~A~%" (render-filter-banner model term-width)))

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
             (filter-help (if has-filters "  c clear" ""))
             (more-help "  a add  e edit  d del  / search  q quit")
             (help (concatenate 'string base-help filter-help more-help)))
        (format s "~A"
                (render-help-line help term-width :fg tui:*fg-bright-black*))))))

(defun render-detail-view (model)
  "Render the detail view for a single TODO as an overlay dialog."
  (handler-case
      (let* ((term-width (model-term-width model))
             (todos (get-visible-todos model))
             (todo (when (< (model-cursor model) (length todos))
                    (nth (model-cursor model) todos)))
             (background (render-list-view model)))
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
            background  ; Just show list if no item selected
            (let* ((modal-width (min 70 (max 50 (- term-width 10))))
                   (content-width (- modal-width 6))  ; Account for border and padding
                   (content
                     (with-output-to-string (s)
                       ;; Status and Priority line
                       (format s "~A ~A~%"
                               (org-status-colored (todo-status todo))
                               (org-priority-colored (todo-priority todo)))

                       ;; Title (wrapped if needed)
                       (let ((title-text (if (member (todo-status todo) '(:completed :cancelled))
                                             (tui:render-styled
                                              (tui:make-style :strikethrough t
                                                              :foreground tui:*fg-bright-black*)
                                              (sanitize-title-for-display (todo-title todo)))
                                             (tui:bold (sanitize-title-for-display (todo-title todo))))))
                         (format s "~A" (tui:wrap-text title-text content-width)))

                       ;; Scheduled date
                       (when (todo-scheduled-date todo)
                         (format s "~%~%~A <~A>"
                                 (tui:bold "SCHEDULED:")
                                 (tui:colored
                                  (lt:format-timestring nil (todo-scheduled-date todo)
                                                       :format '(:long-weekday " " :short-month " " :day " " :year))
                                  :fg tui:*fg-cyan*)))

                       ;; Deadline
                       (when (todo-due-date todo)
                         (format s "~%~A <~A>"
                                 (tui:bold (tui:colored "DEADLINE:" :fg tui:*fg-red*))
                                 (tui:colored
                                  (lt:format-timestring nil (todo-due-date todo)
                                                       :format '(:long-weekday " " :short-month " " :day " " :year))
                                  :fg tui:*fg-red*)))

                       ;; Repeat info
                       (when (and (todo-repeat-interval todo) (todo-repeat-unit todo))
                         (format s "~%~A ~A"
                                 (tui:bold (tui:colored "REPEAT:" :fg tui:*fg-magenta*))
                                 (tui:colored
                                  (format-repeat-preset (todo-repeat-interval todo)
                                                        (todo-repeat-unit todo))
                                  :fg tui:*fg-magenta*)))

                       ;; Description (wrapped)
                       (when (todo-description todo)
                         (format s "~%~%~A"
                                 (tui:wrap-text (todo-description todo) content-width)))

                       ;; Tags
                       (when (todo-tags todo)
                         (format s "~%~%~A"
                                 (tui:colored (org-tags-string (todo-tags todo)) :fg tui:*fg-magenta*)))

                       ;; Estimated time
                       (when (todo-estimated-minutes todo)
                         (format s "~%~%~A"
                                 (tui:colored
                                  (format nil "Estimated: ~A min" (todo-estimated-minutes todo))
                                  :fg tui:*fg-yellow*)))

                       ;; Location info
                       (when (todo-location-info todo)
                         (let ((loc (todo-location-info todo)))
                           (format s "~%~%~A"
                                   (tui:bold (tui:colored "Location" :fg tui:*fg-cyan*)))
                           (when (getf loc :name)
                             (format s "~%  ~A" (getf loc :name)))
                           (when (getf loc :address)
                             (format s "~%  ~A"
                                     (tui:colored (getf loc :address) :fg tui:*fg-bright-black*)))
                           (when (getf loc :phone)
                             (format s "~%  Tel: ~A"
                                     (tui:colored (getf loc :phone) :fg tui:*fg-green*)))
                           (when (getf loc :map-url)
                             (format s "~%  ~A"
                                     (tui:wrap-text (tui:colored (getf loc :map-url) :fg tui:*fg-blue*)
                                                    content-width :indent 2 :continuation-indent 4)))
                           (when (getf loc :website)
                             (format s "~%  ~A"
                                     (tui:wrap-text (tui:colored (getf loc :website) :fg tui:*fg-blue*)
                                                    content-width :indent 2 :continuation-indent 4)))))

                       ;; URL (wrapped)
                       (when (todo-url todo)
                         (format s "~%~%~A~%  ~A"
                                 (tui:bold (tui:colored "Link:" :fg tui:*fg-cyan*))
                                 (tui:wrap-text (tui:colored (todo-url todo) :fg tui:*fg-blue*)
                                                content-width :indent 2 :continuation-indent 4)))

                       ;; Attachments
                       (when (todo-attachment-hashes todo)
                         (let ((count (length (todo-attachment-hashes todo))))
                           (format s "~%~%~A ~A"
                                   (tui:bold (tui:colored "Attachments:" :fg tui:*fg-cyan*))
                                   (tui:colored (format nil "~D photo~:P (press p to view)"
                                                        count)
                                               :fg tui:*fg-yellow*))))

                       ;; Metadata
                       (format s "~%~%~A"
                               (tui:colored
                                (format nil "Created: ~A"
                                       (lt:format-timestring nil (todo-created-at todo)
                                                            :format '(:short-month " " :day ", " :year)))
                                :fg tui:*fg-bright-black*))

                       (when (todo-completed-at todo)
                         (format s "~%~A"
                                 (tui:colored
                                  (format nil "Closed:  ~A"
                                         (lt:format-timestring nil (todo-completed-at todo)
                                                              :format '(:short-month " " :day ", " :year)))
                                  :fg tui:*fg-bright-black*)))

                       ;; Help line at bottom
                       (format s "~%~%~A"
                               (tui:colored "RET/q:back  e:edit  n:notes  s:sched  d:deadline  o:url  p:photo"
                                           :fg tui:*fg-bright-black*))))
                   (modal (render-box-with-title "ITEM DETAILS" content :min-width modal-width)))
              (tui:composite modal background
                            :x-position tui:+center+
                            :y-position tui:+middle+))))
    (error (e)
      (llog:error "Error rendering detail view"
                  :error-type (type-of e)
                  :error-message (format nil "~A" e))
      (format nil "Error: ~A" e))))


(defun format-repeat-preset (interval unit)
  "Format repeat interval and unit as a display string."
  (cond
    ((or (null interval) (null unit)) "None")
    ((and (= interval 1) (eql unit :day)) "Daily")
    ((and (= interval 1) (eql unit :week)) "Weekly")
    ((and (= interval 2) (eql unit :week)) "Biweekly")
    ((and (= interval 1) (eql unit :month)) "Monthly")
    ((and (= interval 1) (eql unit :year)) "Yearly")
    (t (format nil "Every ~D ~A~P" interval
               (string-downcase (symbol-name unit)) interval))))

(defun render-add-edit-view (model)
  "Render the add/edit TODO form as an overlay dialog on the list view."
  (let* ((background (render-list-view model))
         (term-width (model-term-width model))
         (dialog-width (calculate-dialog-width term-width))
         (is-edit (model-edit-todo-id model))
         (title (if is-edit "EDIT ITEM" "NEW ITEM"))
         (title-input-view (tui.textinput:textinput-view (model-title-input model)))
         (content
           (with-output-to-string (c)
             ;; Title input
             (format c "~A ~A"
                     (if (eql (model-active-field model) :title)
                         (tui:colored ">" :fg tui:*fg-cyan*)
                         " ")
                     title-input-view)

             ;; Notes input
             (format c "~%~A ~A"
                     (if (eql (model-active-field model) :description)
                         (tui:colored ">" :fg tui:*fg-cyan*)
                         " ")
                     (tui.textinput:textinput-view (model-description-input model)))

             ;; Priority
             (format c "~%~%~A Priority: ~A ~A ~A"
                     (if (eql (model-active-field model) :priority)
                         (tui:colored ">" :fg tui:*fg-cyan*)
                         " ")
                     (if (eql (model-edit-priority model) :high)
                         (tui:bold (tui:colored "[#A]" :fg tui:*fg-red*))
                         (tui:colored " #A " :fg tui:*fg-bright-black*))
                     (if (eql (model-edit-priority model) :medium)
                         (tui:bold (tui:colored "[#B]" :fg tui:*fg-yellow*))
                         (tui:colored " #B " :fg tui:*fg-bright-black*))
                     (if (eql (model-edit-priority model) :low)
                         (tui:bold (tui:colored "[#C]" :fg tui:*fg-green*))
                         (tui:colored " #C " :fg tui:*fg-bright-black*)))

             ;; Scheduled date
             (let* ((sched-date (model-edit-scheduled-date model))
                    (sched-str (if sched-date
                                   (lt:format-timestring nil sched-date
                                                         :format '(:short-month " " :day ", " :year))
                                   "Not set")))
               (format c "~%~A Scheduled: ~A~A"
                       (if (eql (model-active-field model) :scheduled)
                           (tui:colored ">" :fg tui:*fg-cyan*)
                           " ")
                       (if sched-date
                           (tui:colored (format nil "[~A]" sched-str) :fg tui:*fg-cyan*)
                           (tui:colored (format nil "[~A]" sched-str) :fg tui:*fg-bright-black*))
                       (if (eql (model-active-field model) :scheduled)
                           (tui:colored "  (Space)" :fg tui:*fg-bright-black*)
                           "")))

             ;; Due date
             (let* ((due-date (model-edit-due-date model))
                    (due-str (if due-date
                                 (lt:format-timestring nil due-date
                                                       :format '(:short-month " " :day ", " :year))
                                 "Not set")))
               (format c "~%~A Due:       ~A~A"
                       (if (eql (model-active-field model) :due)
                           (tui:colored ">" :fg tui:*fg-cyan*)
                           " ")
                       (if due-date
                           (tui:colored (format nil "[~A]" due-str) :fg tui:*fg-red*)
                           (tui:colored (format nil "[~A]" due-str) :fg tui:*fg-bright-black*))
                       (if (eql (model-active-field model) :due)
                           (tui:colored "  (Space)" :fg tui:*fg-bright-black*)
                           "")))

             ;; Repeat
             (let ((repeat-str (format-repeat-preset
                                (model-edit-repeat-interval model)
                                (model-edit-repeat-unit model))))
               (format c "~%~A Repeat:    ~A~A"
                       (if (eql (model-active-field model) :repeat)
                           (tui:colored ">" :fg tui:*fg-cyan*)
                           " ")
                       (if (model-edit-repeat-interval model)
                           (tui:colored (format nil "[~A]" repeat-str) :fg tui:*fg-magenta*)
                           (tui:colored (format nil "[~A]" repeat-str) :fg tui:*fg-bright-black*))
                       (if (eql (model-active-field model) :repeat)
                           (tui:colored "  (←/→)" :fg tui:*fg-bright-black*)
                           "")))

             ;; Labels/Tags
             (let* ((edit-tags (model-edit-tags model))
                        (tags-focused (eql (model-active-field model) :tags)))
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
                                              (format nil "  ~A" tag))))))))

             ;; Help line at bottom
             (format c "~%~%~A"
                     (tui:colored "Tab:next  ^E:edit notes  Enter:save  Esc:cancel" :fg tui:*fg-bright-black*))))
         (modal (render-box-with-title title content :min-width dialog-width)))
    (tui:composite-with-shadow modal background
                               :x-position tui:+center+
                               :y-position tui:+middle+)))

(defun render-search-view (model)
  "Render the search view as an overlay dialog on the list view."
  (let* ((background (render-list-view model))
         (term-width (model-term-width model))
         (todos (get-visible-todos model))
         (modal-width (min 60 (max 40 (- term-width 10))))
         (content
           (with-output-to-string (c)
             ;; Search input
             (format c "~A~%~%" (tui.textinput:textinput-view (model-search-input model)))

             ;; Match count
             (format c "~A~%"
                     (tui:colored (format nil "~D match~:P" (length todos))
                                 :fg tui:*fg-bright-black*))

             ;; Preview of results
             (when todos
               (format c "~%")
               (loop for todo in (subseq todos 0 (min 5 (length todos)))
                     do (format c "~A ~A ~A~%"
                               (org-status-colored (todo-status todo))
                               (org-priority-colored (todo-priority todo))
                               (let* ((title (sanitize-title-for-display (todo-title todo)))
                                      (max-len (- modal-width 20)))
                                 (if (> (length title) max-len)
                                     (concatenate 'string (subseq title 0 (max 0 (- max-len 2))) "..")
                                     title))))
               (when (> (length todos) 5)
                 (format c "~%~A"
                         (tui:colored (format nil "... and ~D more" (- (length todos) 5))
                                     :fg tui:*fg-bright-black*))))

             ;; Help line
             (format c "~%~%~A"
                     (tui:colored "RET:apply  ESC:cancel" :fg tui:*fg-bright-black*))))
         (modal (render-box-with-title "SEARCH" content :min-width modal-width)))
    (tui:composite-with-shadow modal background
                               :x-position tui:+center+
                               :y-position tui:+middle+)))

(defun render-delete-confirm-view (model)
  "Render the delete confirmation dialog as an overlay."
  (let* ((term-width (model-term-width model))
         (todos (get-visible-todos model))
         (todo (when (< (model-cursor model) (length todos))
                 (nth (model-cursor model) todos)))
         (background (render-list-view model))
         (max-width (max 20 (- term-width 2)))
         (modal-width (min 50 max-width))
         (content (if todo
                      (with-output-to-string (c)
                        (format c "~A~%~%"
                                (tui:bold "Delete this item?"))
                        (format c "~A~%~%"
                                (sanitize-title-for-display (todo-title todo)))
                        (format c "~A"
                                (tui:colored "y:confirm  any other key:cancel"
                                            :fg tui:*fg-bright-black*)))
                      "No item selected"))
         (modal (render-box-with-title "DELETE ITEM" content :min-width modal-width)))
    (tui:composite-with-shadow modal background
                               :x-position tui:+center+
                               :y-position tui:+middle+)))

(defun render-delete-done-confirm-view (model)
  "Render the delete-done confirmation dialog as an overlay."
  (let* ((term-width (model-term-width model))
         (done-count (count-if (lambda (todo) (eq (todo-status todo) +status-completed+))
                               (model-todos model)))
         (background (render-list-view model))
         (max-width (max 20 (- term-width 2)))
         (modal-width (min 60 max-width))
         (content (with-output-to-string (c)
                    (format c "~A~%~%"
                            (tui:bold (format nil "Delete ~D DONE item~:P?" done-count)))
                    (format c "This will remove completed items from the list.~%~%")
                    (format c "~A"
                            (tui:colored "y:confirm  any other key:cancel"
                                        :fg tui:*fg-bright-black*))))
         (modal (render-box-with-title "DELETE DONE ITEMS" content :min-width modal-width)))
    (tui:composite-with-shadow modal background
                               :x-position tui:+center+
                               :y-position tui:+middle+)))

(defun render-delete-tag-confirm-view (model)
  "Render the delete tag confirmation dialog as an overlay."
  (let* ((term-width (model-term-width model))
         (tag (model-deleting-tag model))
         (background (render-list-view model))
         ;; Count how many todos have this tag
         (count (count-if (lambda (todo)
                            (member tag (todo-tags todo) :test #'string=))
                          (model-todos model)))
         (max-width (max 20 (- term-width 2)))
         (modal-width (min 55 max-width))
         (content (with-output-to-string (c)
                    (format c "~A~%~%"
                            (tui:bold (format nil "Delete label \"~A\"?" tag)))
                    (format c "This will remove the label from ~D item~:P.~%~%"
                            count)
                    (format c "~A"
                            (tui:colored "y:confirm  any other key:cancel"
                                        :fg tui:*fg-bright-black*))))
         (modal (render-box-with-title "DELETE LABEL" content :min-width modal-width)))
    (tui:composite-with-shadow modal background
                               :x-position tui:+center+
                               :y-position tui:+middle+)))

(defun render-context-info-view (model)
  "Render the context info view as an overlay showing where to edit user context."
  (let* ((term-width (model-term-width model))
         (context-file (namestring (user-context-file)))
         (has-context (load-user-context))
         (background (render-list-view model))
         (modal-width (min 65 (max 50 (- term-width 6))))
         (content
           (with-output-to-string (c)
             (format c "User context provides personal information to help~%")
             (format c "the AI better understand and enrich your TODOs.~%~%")
             (format c "~A~%"
                     (tui:colored "Context File:" :fg tui:*fg-cyan*))
             (format c "  ~A~%~%"
                     (tui:colored context-file :fg tui:*fg-yellow*))
             (format c "~A~%"
                     (tui:colored "To edit, open in your editor:" :fg tui:*fg-cyan*))
             (format c "  ~A~%~%"
                     (tui:colored (format nil "$EDITOR ~A" context-file) :fg tui:*fg-green*))
             (if has-context
                 (format c "~A~%"
                         (tui:colored "✓ Context file has content" :fg tui:*fg-green*))
                 (format c "~A~%"
                         (tui:colored "○ Context file is empty - add your info!" :fg tui:*fg-yellow*)))
             (format c "~%~A"
                     (tui:colored "Press any key to close" :fg tui:*fg-bright-black*))))
         (modal (render-box-with-title "USER CONTEXT" content :min-width modal-width)))
    (tui:composite-with-shadow modal background
                               :x-position tui:+center+
                               :y-position tui:+middle+)))

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
                  ((zerop (length filename))
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
  "Render the date picker view for editing scheduled/deadline dates as an overlay."
  (let* ((term-width (model-term-width model))
         (picker (model-date-picker model))
         (date-type (model-editing-date-type model))
         (todos (get-visible-todos model))
         (todo (when (< (model-cursor model) (length todos))
                 (nth (model-cursor model) todos)))
         (background (render-list-view model))
         (title (if (eql date-type :scheduled) "SET SCHEDULED DATE" "SET DEADLINE"))
         (current-date (case date-type
                         (:scheduled (when todo (todo-scheduled-date todo)))
                         (:deadline (when todo (todo-due-date todo)))))
         (selected (tui.datepicker:datepicker-selected picker))
         (modal-width (min 56 (max 44 (- term-width 8))))
         (content
           (with-output-to-string (c)
             ;; Show todo title context
             (when todo
               (let ((todo-title (sanitize-title-for-display (todo-title todo))))
                 (format c "~A~%~%"
                         (tui:colored
                          (if (> (length todo-title) (- modal-width 6))
                              (concatenate 'string (subseq todo-title 0 (- modal-width 8)) "..")
                              todo-title)
                          :fg tui:*fg-bright-black*))))

             ;; Current date status
             (format c "~A ~A~%"
                     (tui:bold (if (eql date-type :scheduled) "Current:" "Current:"))
                     (if current-date
                         (tui:colored
                          (lt:format-timestring nil current-date
                                               :format '(:short-month " " :day ", " :year))
                          :fg tui:*fg-cyan*)
                         (tui:colored "Not set" :fg tui:*fg-bright-black*)))

             ;; Datepicker calendar
             (format c "~%~A~%" (tui.datepicker:datepicker-view picker))

             ;; Selected date
             (format c "~%~A ~A~%"
                     (tui:bold "New:")
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
             (format c "~%~A"
                     (tui:colored "hjkl:nav  []:month  {}:year  Home:today  RET:save  DEL:clear  ESC:cancel"
                                 :fg tui:*fg-bright-black*))))
         (modal (render-box-with-title title content :min-width modal-width)))
    (tui:composite-with-shadow modal background
                               :x-position tui:+center+
                               :y-position tui:+middle+)))

(defun render-help-column (title entries &optional (key-width 10))
  "Render a help column with TITLE and list of (key . description) ENTRIES."
  (with-output-to-string (s)
    (format s "~A~%" (tui:bold (tui:colored title :fg tui:*fg-cyan*)))
    (dolist (entry entries)
      (let ((key (first entry))
            (desc (rest entry)))
        (if (string= key "")
            ;; Empty key = section header or blank line
            (if (string= desc "")
                (format s "~%")
                (format s "~A~%" (tui:colored desc :fg tui:*fg-bright-black*)))
            ;; Normal key-description pair
            (format s "~A  ~A~%"
                    (tui:colored (format nil "~vA" key-width key) :fg tui:*fg-yellow*)
                    desc))))))

(defun valid-preset-p (p)
  "Check if preset P is a valid non-empty list of string tags."
  (and p
       (listp p)
       (not (eq p 'null))
       (not (eql p :null))
       (> (length p) 0)
       (stringp (first p))))

(defun format-preset-tags (preset)
  "Format preset tags for display, or return placeholder if empty."
  (if (valid-preset-p preset)
      (let ((tags (format nil "~{~A~^,~}" preset)))
        (if (> (length tags) 20)
            (concatenate 'string (subseq tags 0 18) "..")
            tags))
      (tui:colored "_______" :fg tui:*fg-bright-black*)))

(defun render-presets-row (presets)
  "Render presets in two columns below the keyboard shortcuts."
  (let ((col-width 28))
    (with-output-to-string (s)
      ;; Header spanning both columns
      (format s "~A~%" (tui:bold (tui:colored "LABEL PRESETS (1-0 to apply, !@# to save)" :fg tui:*fg-cyan*)))
      ;; Two columns: 1-5 on left, 6-0 on right
      (loop for row from 0 below 5
            for left-idx = row
            for right-idx = (+ row 5)
            for left-key = (format nil "~D" (1+ left-idx))
            for right-key = (if (= right-idx 9) "0" (format nil "~D" (1+ right-idx)))
            for left-preset = (when (and presets (arrayp presets) (< left-idx (length presets)))
                                (aref presets left-idx))
            for right-preset = (when (and presets (arrayp presets) (< right-idx (length presets)))
                                 (aref presets right-idx))
            for left-str = (format-preset-tags left-preset)
            for right-str = (format-preset-tags right-preset)
            do (format s "~A ~A~A~A ~A~%"
                       (tui:colored left-key :fg tui:*fg-yellow*)
                       left-str
                       (make-string (max 1 (- col-width 2 (tui:visible-length left-str))) :initial-element #\Space)
                       (tui:colored right-key :fg tui:*fg-yellow*)
                       right-str)))))

(defun render-help-modal-content (model)
  "Render the help modal content with multiple columns using join-horizontal."
  (let* ((col1 (render-help-column "NAVIGATION"
                '(("j/k ↑↓" . "Navigate")
                  ("p/n" . "Prev/Next")
                  ("g/G" . "First/Last")
                  ("PgUp/Dn" . "Page up/down")
                  ("Enter" . "View details")
                  ("Space" . "Cycle status")
                  ("a" . "Add item")
                  ("e" . "Edit item")
                  ("DEL/D" . "Delete item/done")
                  ("S-↑/↓" . "Priority +/-"))))
         (col2 (render-help-column "ORGANIZE"
                '(("t" . "Edit tags")
                  ("/" . "Search")
                  ("f" . "Filter status")
                  ("s" . "Cycle sort")
                  ("c" . "Clear filters")
                  ("r" . "Refresh list")
                  ("S/L" . "Set sched/due")
                  ("&" . "Re-enrich")
                  ("" . "")
                  ("" . ""))))
         (col3 (render-help-column "SIDEBAR"
                '(("l" . "Toggle sidebar")
                  ("Tab" . "Focus sidebar")
                  ("" . "When focused:")
                  ("j/k" . "Navigate tags")
                  ("Space" . "Toggle tag")
                  ("a" . "Select all")
                  ("Esc" . "Return to list")
                  ("" . "")
                  ("" . "")
                  ("" . ""))))
         (col4 (render-help-column "OTHER"
                '(("i" . "Import org")
                  ("u/U" . "Edit context")
                  ("?" . "This help")
                  ("q" . "Quit")
                  ("" . "Datepicker:")
                  ("hjkl" . "Navigate")
                  ("[/]" . "Month +/-")
                  ("{/}" . "Year +/-")
                  ("Home" . "Today")
                  ("Del" . "Clear date"))))
         (presets (model-tag-presets model)))
    ;; Create vertical separator spanning all rows
    (let* ((num-rows 11)  ; title + 10 entries
           (sep-line (tui:colored " │ " :fg tui:*fg-bright-black*))
           (sep (format nil "~{~A~^~%~}" (loop repeat num-rows collect sep-line)))
           (keys-section (tui:join-horizontal :top col1 sep col2 sep col3 sep col4))
           (presets-section (render-presets-row presets)))
      (format nil "~A~%~%~A" keys-section presets-section))))

(defun render-box-with-title (title content &key min-width)
  "Render a box with a title label on the top border line using double-line borders.
   ╔══╡ TITLE ╞═════════════════════════╗
   ║ content                            ║
   ╚════════════════════════════════════╝
   If MIN-WIDTH is specified, the box will be at least that wide (inner content area).
   Shadow should be added at composition time using composite-with-shadow."
  (let* ((content-lines (tui:split-string-by-newline content))
         (natural-width (if content-lines
                            (apply #'max (mapcar #'tui:visible-length content-lines))
                            40))
         (content-width (if min-width
                            (max natural-width (- min-width 4))  ; -4 for border and padding
                            natural-width))
         (indent 2)  ; Title starts 2 chars from left
         (padded-title (format nil " ~A " title))
         (title-len (length padded-title))
         ;; Box inner width = content + 2 for padding
         (box-inner-width (+ content-width 2))
         ;; Width after title to right edge
         (right-width (max 0 (- box-inner-width indent title-len 2)))  ; -2 for ╡ and ╞
         (reset (format nil "~C[0m" #\Escape)))  ; ANSI reset to prevent color bleed
    (with-output-to-string (s)
      ;; Top border with embedded title (double-line)
      (format s "~A╔~A╡~A╞~A╗~%"
              reset
              (make-string indent :initial-element #\═)
              (tui:bold padded-title)
              (make-string right-width :initial-element #\═))
      ;; Content rows (double-line)
      (dolist (line content-lines)
        (let ((padding (max 0 (- content-width (tui:visible-length line)))))
          (format s "~A║ ~A~A ║~%"
                  reset
                  line
                  (make-string padding :initial-element #\Space))))
      ;; Bottom border (double-line)
      (format s "~A╚~A╝"
              reset
              (make-string box-inner-width :initial-element #\═)))))

(defun render-help-view (model)
  "Render the help view as an overlay dialog on the list view."
  (let* ((background (render-list-view model))
         (content (render-help-modal-content model))
         (footer (tui:colored "Press any key to close" :fg tui:*fg-bright-black*))
         (inner (format nil "~%~A~%~A" content footer))
         (modal (render-box-with-title "KEYBOARD SHORTCUTS" inner)))
    (tui:composite-with-shadow modal background
                               :x-position tui:+center+
                               :y-position tui:+middle+)))

(defun render-inline-tag-editor (model)
  "Render the inline tag editor as an overlay on the list view."
  (let* ((background (render-list-view model))
         (todos (get-visible-todos model))
         (todo (when (and (model-edit-todo-id model)
                         (< (model-cursor model) (length todos)))
                 (find (model-edit-todo-id model) todos :key #'todo-id :test #'string=)))
         (edit-tags (model-edit-tags model))
         (title (if todo
                    (let ((t-title (sanitize-title-for-display (todo-title todo))))
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
    (tui:composite-with-shadow modal background
                               :x-position tui:+center+
                               :y-position tui:+middle+)))

(defun render-form-date-edit-view (model)
  "Render the date picker view for add/edit form as an overlay on the form."
  (let* ((background (render-add-edit-view model))
         (term-width (model-term-width model))
         (picker (model-date-picker model))
         (date-type (model-editing-date-type model))
         (modal-width (min 56 (max 44 (- term-width 8))))
         (title (if (eql date-type :scheduled) "SET SCHEDULED DATE" "SET DEADLINE"))
         (current-date (case date-type
                         (:scheduled (model-edit-scheduled-date model))
                         (:due (model-edit-due-date model))))
         (cursor-date (tui.datepicker:datepicker-time picker))
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
         (content
           (with-output-to-string (c)
             ;; Current value
             (format c "Current: ~A~%~%"
                     (tui:colored current-str :fg tui:*fg-cyan*))

             ;; Datepicker calendar (centered)
             (loop for picker-line in picker-lines
                   for line-content = (string-trim '(#\Space #\Newline) picker-line)
                   when (> (length line-content) 0)
                   do (let* ((vis-len (tui:visible-length line-content))
                             (pad-left (max 0 (floor (- (- modal-width 4) vis-len) 2))))
                        (format c "~A~A~%"
                                (make-string pad-left :initial-element #\Space)
                                line-content)))

             ;; Blank line
             (format c "~%")

             ;; New/selected date
             (format c "New: ~A~%~%"
                     (tui:colored (or cursor-str "None") :fg tui:*fg-green*))

             ;; Help line
             (format c "~A"
                     (tui:colored "hjkl:nav  []:month  {}:year  Home:today  RET:save  DEL:clear  ESC:cancel"
                                 :fg tui:*fg-bright-black*))))
         (modal (render-box-with-title title content :min-width modal-width)))
    (tui:composite-with-shadow modal background
                               :x-position tui:+center+
                               :y-position tui:+middle+)))

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
         (title (if (eql date-type :scheduled) "SET SCHEDULED DATE" "SET DEADLINE"))
         (todo-title (if todo
                        (let ((t-title (sanitize-title-for-display (todo-title todo))))
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
         (modal-bordered (tui:render-border modal-content tui:*border-double*
                                            :fg-color tui:*fg-blue*)))
    ;; Use composite-with-shadow for semi-transparent shadow
    (tui:composite-with-shadow modal-bordered background
                               :x-position tui:+center+
                               :y-position tui:+middle+)))

;;── Main View Method ───────────────────────────────────────────────────────────

(defmethod tui:view ((model app-model))
  "Main view dispatcher based on current view state."
  (llog:debug "Rendering view" :view-state (model-view-state model))
  ;; Check minimum terminal width
  (when (< (model-term-width model) *min-terminal-width*)
    (return-from tui:view (render-terminal-too-narrow model)))
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
    (:delete-tag-confirm (render-delete-tag-confirm-view model))
    (:edit-date (render-date-edit-view model))
    (:list-set-date (render-list-date-modal model))
    ((:add-scheduled-date :add-due-date) (render-form-date-edit-view model))
    (:help (render-help-view model))
    (:inline-tags (render-inline-tag-editor model))
    (:context-info (render-context-info-view model))
    (otherwise (render-list-view model))))
