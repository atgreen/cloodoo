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
  "Launch the TUI application.
   Auto-connects to a paired sync server if a sync config exists."
  ;; Initialize LLM enrichment
  (init-enrichment)
  ;; Run with error handling that logs backtraces
  (handler-bind ((error (lambda (e)
                          (log-error-with-backtrace e)
                          ;; Re-signal to let it propagate
                          (signal e))))
    ;; Check for paired sync config
    (multiple-value-bind (sync-host sync-port sync-server-id)
        (find-paired-sync-config)
      ;; Suppress poll warnings from SBCL when terminal resizes
      #+sbcl
      (handler-bind ((warning #'muffle-warning))
        (let* ((model (make-initial-model))
               (program (tui:make-program model :alt-screen t :mouse :cell-motion)))
          (if sync-host
              ;; Auto-connect to paired sync server
              (let ((cert-path (namestring (paired-client-cert-file sync-server-id)))
                    (key-path (namestring (paired-client-key-file sync-server-id))))
                (start-sync-client sync-host sync-port
                                   :model model
                                   :program program
                                   :client-certificate cert-path
                                   :client-key key-path)
                (unwind-protect
                     (tui:run program)
                  (stop-sync-client)))
              ;; No sync config — run TUI without sync
              (tui:run program))))
      #-sbcl
      (let* ((model (make-initial-model))
             (program (tui:make-program model :alt-screen t :mouse :cell-motion)))
        (if sync-host
            (let ((cert-path (namestring (paired-client-cert-file sync-server-id)))
                  (key-path (namestring (paired-client-key-file sync-server-id))))
              (start-sync-client sync-host sync-port
                                 :model model
                                 :program program
                                 :client-certificate cert-path
                                 :client-key key-path)
              (unwind-protect
                   (tui:run program)
                (stop-sync-client)))
            (tui:run program))))))

;;── Entry Point ────────────────────────────────────────────────────────────────

(defun main ()
  "The main entrypoint."
  ;; Set up SIGQUIT handler to dump thread stacks (like JVM Ctrl+\)
  (setup-thread-dump-signal)
  (handler-bind ((error (lambda (e)
                          (format *error-output* "~%Error: ~A~%~%" e)
                          #+sbcl (sb-debug:print-backtrace :stream *error-output* :count 20)
                          (uiop:quit 1))))
    (clingon:run (make-app))))
