;;; tests.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(defpackage #:cloodoo-tests
  (:use #:cl #:fiveam)
  (:export #:run-tests))

(in-package #:cloodoo-tests)

(def-suite cloodoo-tests
  :description "Tests for the Cluedo TODO system.")

(in-suite cloodoo-tests)

;;── Model Tests ────────────────────────────────────────────────────────────────

(test make-todo-test
  "Test creating a new TODO."
  (let ((todo (cloodoo:make-todo "Test task")))
    (is (string= "Test task" (cloodoo:todo-title todo)))
    (is (eq cloodoo:+priority-medium+ (cloodoo:todo-priority todo)))
    (is (eq cloodoo:+status-pending+ (cloodoo:todo-status todo)))
    (is (null (cloodoo:todo-description todo)))
    (is (null (cloodoo:todo-due-date todo)))
    (is (null (cloodoo:todo-tags todo)))))

(test make-todo-with-priority-test
  "Test creating a TODO with custom priority."
  (let ((todo (cloodoo:make-todo "High priority task"
                                 :priority cloodoo:+priority-high+)))
    (is (eq cloodoo:+priority-high+ (cloodoo:todo-priority todo)))))

(test make-todo-with-description-test
  "Test creating a TODO with description."
  (let ((todo (cloodoo:make-todo "Task with notes"
                                :description "These are my notes")))
    (is (string= "These are my notes" (cloodoo:todo-description todo)))))

(test make-todo-with-tags-test
  "Test creating a TODO with tags."
  (let ((todo (cloodoo:make-todo "Tagged task"
                                :tags '("work" "urgent"))))
    (is (equal '("work" "urgent") (cloodoo:todo-tags todo)))))

;;── Run Tests ──────────────────────────────────────────────────────────────────

(defun run-tests ()
  "Run all tests."
  (run! 'cloodoo-tests))
