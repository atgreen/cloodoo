;;; debug.lisp - Thread debugging utilities
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cloodoo)

(defun dump-all-thread-stacks (&optional (stream *error-output*))
  "Dump stack traces for all running threads to STREAM."
  (format stream "~%~%=== Thread Stack Dump (PID ~A) ===~%" (sb-posix:getpid))
  (format stream "Time: ~A~%" (lt:now))
  (format stream "~%")

  (dolist (thread (bt:all-threads))
    (format stream "~%Thread: ~A~%" (bt:thread-name thread))
    (format stream "  Status: ~:[dead~;alive~]~%" (bt:thread-alive-p thread))
    (when (bt:thread-alive-p thread)
      (format stream "  Stack:~%")
      (handler-case
          #+sbcl
          (let ((backtrace (sb-debug:list-backtrace thread)))
            (loop for frame in backtrace
                  for i from 0
                  when (< i 30)  ; Limit to 30 frames
                  do (format stream "    ~2D: ~A~%" i frame)))
          #-sbcl
          (format stream "    (backtrace not available on this implementation)~%")
        (error (e)
          (format stream "    Error getting backtrace: ~A~%" e)))))

  (format stream "~%=== End Thread Stack Dump ===~%~%")
  (force-output stream))

(defun setup-thread-dump-signal ()
  "Set up SIGQUIT (signal 3) handler to dump thread stacks, like JVM."
  #+sbcl
  (sb-sys:enable-interrupt
   sb-unix:sigquit
   (lambda (signal info context)
     (declare (ignore signal info context))
     (dump-all-thread-stacks)))
  #-sbcl
  (warn "Thread dump signal handler not available on this Lisp implementation"))
