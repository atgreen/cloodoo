;;; tests.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(defpackage #:cluedo-tests
  (:use #:cl #:fiveam)
  (:export #:run-tests))

(in-package #:cluedo-tests)

(def-suite cluedo-tests
  :description "Tests for the Cluedo TODO system.")

(in-suite cluedo-tests)

;;── Model Tests ────────────────────────────────────────────────────────────────

(test make-todo-test
  "Test creating a new TODO."
  (let ((todo (cluedo:make-todo "Test task")))
    (is (string= "Test task" (cluedo:todo-title todo)))
    (is (eq cluedo:+priority-medium+ (cluedo:todo-priority todo)))
    (is (eq cluedo:+status-pending+ (cluedo:todo-status todo)))
    (is (null (cluedo:todo-description todo)))
    (is (null (cluedo:todo-due-date todo)))
    (is (null (cluedo:todo-tags todo)))))

(test make-todo-with-priority-test
  "Test creating a TODO with custom priority."
  (let ((todo (cluedo:make-todo "High priority task"
                                 :priority cluedo:+priority-high+)))
    (is (eq cluedo:+priority-high+ (cluedo:todo-priority todo)))))

(test make-todo-with-description-test
  "Test creating a TODO with description."
  (let ((todo (cluedo:make-todo "Task with notes"
                                :description "These are my notes")))
    (is (string= "These are my notes" (cluedo:todo-description todo)))))

(test make-todo-with-tags-test
  "Test creating a TODO with tags."
  (let ((todo (cluedo:make-todo "Tagged task"
                                :tags '("work" "urgent"))))
    (is (equal '("work" "urgent") (cluedo:todo-tags todo)))))

;;── Run Tests ──────────────────────────────────────────────────────────────────

(defun run-tests ()
  "Run all tests."
  (run! 'cluedo-tests))
