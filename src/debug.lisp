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
          (if (eq thread sb-thread:*current-thread*)
              ;; Current thread - print directly
              (sb-debug:print-backtrace :stream stream :count 30)
              ;; Other thread - have it print its own backtrace
              (progn
                (sb-thread:interrupt-thread
                 thread
                 (lambda ()
                   (handler-case
                       (sb-debug:print-backtrace :stream stream :count 30)
                     (error (e)
                       (format stream "    Error in thread backtrace: ~A~%" e)))))
                ;; Give it a moment to print
                (sleep 0.05)))
          #-sbcl
          (format stream "    (backtrace not available on this implementation)~%")
        (error (e)
          (format stream "    Error getting backtrace: ~A~%" e)))))

  (format stream "~%=== End Thread Stack Dump ===~%~%")
  (force-output stream))

(defun setup-thread-dump-signal ()
  "Set up SIGQUIT (signal 3) handler to dump thread stacks, like JVM."
  #+sbcl
  (handler-case
      (progn
        (sb-sys:enable-interrupt
         sb-unix:sigquit
         (lambda (signal info context)
           (declare (ignore signal info context))
           (ensure-cache-directory)
           (let* ((timestamp (lt:format-timestring nil (lt:now) :format '(:year "-" :month "-" :day "T" :hour ":" :min ":" :sec)))
                  (filename (format nil "~A/cloodoo-threaddump-~A.txt"
                                  (cache-directory) timestamp)))
             (handler-case
                 (with-open-file (stream filename
                                        :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
                   (dump-all-thread-stacks stream)
                   (format *error-output* "~%Thread dump written to: ~A~%" filename)
                   (force-output *error-output*))
               (error (e)
                 (format *error-output* "~%Failed to write thread dump: ~A~%" e)
                 (force-output *error-output*))))))
        (llog:info "SIGQUIT handler installed for thread dumps (kill -QUIT <pid> or Ctrl+\\)"))
    (error (e)
      (llog:error "Failed to install SIGQUIT handler" :error (princ-to-string e))))
  #-sbcl
  (warn "Thread dump signal handler not available on this Lisp implementation"))
