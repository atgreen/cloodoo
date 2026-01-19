;;; main.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cloodoo)

;;── Error Handling ─────────────────────────────────────────────────────────────

(defun log-error-with-backtrace (condition)
  "Log an error condition with full backtrace to the debug log."
  (llog:error "Unhandled error"
              :condition-type (type-of condition)
              :message (format nil "~A" condition))
  ;; Capture backtrace
  (let ((backtrace-string
          (with-output-to-string (s)
            (format s "~%Backtrace:~%")
            #+sbcl
            (sb-debug:print-backtrace :stream s :count 50)
            #-sbcl
            (format s "(Backtrace not available on this implementation)"))))
    (llog:error "Backtrace" :trace backtrace-string)))

;;── TUI Application ────────────────────────────────────────────────────────────

(defun start-tui ()
  "Launch the TUI application."
  ;; Initialize LLM enrichment
  (init-enrichment)
  ;; Run with error handling that logs backtraces
  (handler-bind ((error (lambda (e)
                          (log-error-with-backtrace e)
                          ;; Re-signal to let it propagate
                          (signal e))))
    ;; Suppress poll warnings from SBCL when terminal resizes
    #+sbcl
    (handler-bind ((warning #'muffle-warning))
      (let* ((model (make-initial-model))
             (program (tui:make-program model :alt-screen t)))
        (tui:run program)))
    #-sbcl
    (let* ((model (make-initial-model))
           (program (tui:make-program model :alt-screen t)))
      (tui:run program))))

;;── Entry Point ────────────────────────────────────────────────────────────────

(defun main ()
  "The main entrypoint."
  (handler-bind ((error (lambda (e)
                          (format *error-output* "~%Error: ~A~%~%" e)
                          #+sbcl (sb-debug:print-backtrace :stream *error-output* :count 20)
                          (uiop:quit 1))))
    (clingon:run (make-app))))
