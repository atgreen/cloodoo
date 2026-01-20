;;; package.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(defpackage #:cloodoo
  (:use #:cl)
  (:local-nicknames (#:tui #:tuition)
                    (#:jzon #:com.inuoe.jzon)
                    (#:lt #:local-time))
  (:documentation "Personal TODO system with a modern TUI.")
  (:export
   ;; Model
   #:todo
   #:todo-id
   #:todo-title
   #:todo-description
   #:todo-priority
   #:todo-status
   #:todo-due-date
   #:todo-tags
   #:todo-created-at
   #:todo-completed-at
   #:make-todo

   ;; Priorities and statuses
   #:+priority-high+
   #:+priority-medium+
   #:+priority-low+
   #:+status-pending+
   #:+status-in-progress+
   #:+status-completed+
   #:+status-waiting+

   ;; Storage
   #:load-todos
   #:save-todos
   #:data-directory
   #:config-directory
   #:cache-directory
   #:ensure-data-directory
   #:ensure-config-directory
   #:ensure-cache-directory

   ;; Server
   #:start-server
   #:stop-server

   ;; Application
   #:+version+
   #:start-tui
   #:main))

(in-package #:cloodoo)

;;── Version ────────────────────────────────────────────────────────────────────

(version-string:define-version-parameter +version+ :cloodoo)
