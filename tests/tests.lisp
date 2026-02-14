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
  :description "Tests for the Cloodoo TODO system.")

(in-suite cloodoo-tests)

;;── Test Database Helper ──────────────────────────────────────────────────────

(defmacro with-test-db (&body body)
  "Execute BODY with a fresh in-memory test database.
   Suppresses change notifications and prevents re-entrant initialization."
  (let ((db-var (gensym "DB"))
        (db-path (gensym "PATH")))
    `(let* ((,db-path (merge-pathnames
                       (format nil "cloodoo-test-~A.db" (random 1000000))
                       (uiop:temporary-directory)))
            (,db-var (sqlite:connect (namestring ,db-path))))
       ;; Enable WAL mode and foreign keys like production
       (sqlite:execute-non-query ,db-var "PRAGMA journal_mode=WAL")
       (sqlite:execute-non-query ,db-var "PRAGMA busy_timeout=5000")
       (sqlite:execute-non-query ,db-var "PRAGMA foreign_keys=ON")
       (let ((cloodoo::*db* ,db-var)
             (cloodoo::*db-initializing* t)
             (cloodoo::*suppress-change-notifications* t))
         (unwind-protect
              (progn
                (cloodoo::init-db)
                ,@body)
           (sqlite:disconnect ,db-var)
           (when (probe-file ,db-path)
             (delete-file ,db-path))
           ;; Clean up WAL/SHM files
           (let ((wal (merge-pathnames (format nil "~A-wal" (pathname-name ,db-path))
                                       (uiop:temporary-directory)))
                 (shm (merge-pathnames (format nil "~A-shm" (pathname-name ,db-path))
                                       (uiop:temporary-directory))))
             (when (probe-file wal) (delete-file wal))
             (when (probe-file shm) (delete-file shm))))))))

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

;;── List Model Tests ──────────────────────────────────────────────────────────

(test make-list-definition-test
  "Test creating a new list definition."
  (let ((list-def (cloodoo:make-list-definition "Movies"
                    :description "Movies to watch"
                    :sections '("Action" "Comedy" "Drama"))))
    (is (stringp (cloodoo:list-def-id list-def)))
    (is (string= "Movies" (cloodoo:list-def-name list-def)))
    (is (string= "Movies to watch" (cloodoo:list-def-description list-def)))
    (is (equal '("Action" "Comedy" "Drama") (cloodoo:list-def-sections list-def)))
    (is (typep (cloodoo:list-def-created-at list-def) 'local-time:timestamp))))

(test make-list-item-test
  "Test creating a new list item."
  (let ((item (cloodoo:make-list-item "list-123" "Milk"
                :section "Dairy"
                :notes "2% preferred")))
    (is (stringp (cloodoo:list-item-id item)))
    (is (string= "list-123" (cloodoo:list-item-list-id item)))
    (is (string= "Milk" (cloodoo:list-item-title item)))
    (is (string= "Dairy" (cloodoo:list-item-section item)))
    (is (null (cloodoo:list-item-checked item)))
    (is (string= "2% preferred" (cloodoo:list-item-notes item)))))

;;── List Database CRUD Tests ──────────────────────────────────────────────────

(test list-definition-crud-test
  "Test CRUD operations for list definitions."
  (with-test-db
    ;; Create
    (let ((list-def (cloodoo:make-list-definition "Books"
                      :description "Books to read"
                      :sections '("Fiction" "Non-fiction"))))
      (cloodoo::db-save-list-definition list-def)

      ;; Read all
      (let ((loaded (cloodoo::db-load-list-definitions)))
        (is (= 1 (length loaded)))
        (is (string= "Books" (cloodoo:list-def-name (first loaded))))
        (is (string= "Books to read" (cloodoo:list-def-description (first loaded))))
        (is (equal '("Fiction" "Non-fiction") (cloodoo:list-def-sections (first loaded)))))

      ;; Find by name (case-insensitive)
      (let ((found (cloodoo::db-find-list-by-name "books")))
        (is (not (null found)))
        (is (string= "Books" (cloodoo:list-def-name found))))

      (let ((found (cloodoo::db-find-list-by-name "BOOKS")))
        (is (not (null found))))

      ;; Find by name - not found
      (let ((missing (cloodoo::db-find-list-by-name "Nonexistent")))
        (is (null missing)))

      ;; Find by ID
      (let ((found (cloodoo::db-find-list-by-id (cloodoo:list-def-id list-def))))
        (is (not (null found)))
        (is (string= "Books" (cloodoo:list-def-name found))))

      ;; Update
      (let ((updated (make-instance 'cloodoo:list-definition
                       :id (cloodoo:list-def-id list-def)
                       :name "Reading List"
                       :description "Updated description"
                       :sections '("Fiction" "Non-fiction" "Textbooks")
                       :created-at (cloodoo:list-def-created-at list-def))))
        (cloodoo::db-save-list-definition updated)
        (let ((reloaded (cloodoo::db-load-list-definitions)))
          (is (= 1 (length reloaded)))
          (is (string= "Reading List" (cloodoo:list-def-name (first reloaded))))
          (is (string= "Updated description" (cloodoo:list-def-description (first reloaded))))
          (is (equal '("Fiction" "Non-fiction" "Textbooks")
                     (cloodoo:list-def-sections (first reloaded))))))

      ;; Delete
      (cloodoo::db-delete-list-definition (cloodoo:list-def-id list-def))
      (let ((after-delete (cloodoo::db-load-list-definitions)))
        (is (= 0 (length after-delete)))))))

(test list-item-crud-test
  "Test CRUD operations for list items."
  (with-test-db
    ;; Create parent list
    (let ((list-def (cloodoo:make-list-definition "Grocery"
                      :sections '("Produce" "Dairy"))))
      (cloodoo::db-save-list-definition list-def)
      (let ((list-id (cloodoo:list-def-id list-def)))

        ;; Create items
        (let ((item1 (cloodoo:make-list-item list-id "Milk" :section "Dairy"))
              (item2 (cloodoo:make-list-item list-id "Apples" :section "Produce"))
              (item3 (cloodoo:make-list-item list-id "Bread")))
          (cloodoo::db-save-list-item item1)
          (cloodoo::db-save-list-item item2)
          (cloodoo::db-save-list-item item3)

          ;; Load items for list
          (let ((items (cloodoo::db-load-list-items list-id)))
            (is (= 3 (length items)))
            ;; All should be unchecked
            (is (every (lambda (i) (null (cloodoo:list-item-checked i))) items)))

          ;; Check an item
          (cloodoo::db-check-list-item (cloodoo:list-item-id item1) t)
          (let ((items (cloodoo::db-load-list-items list-id)))
            (let ((milk (find "Milk" items :key #'cloodoo:list-item-title :test #'string=)))
              (is (not (null milk)))
              (is (eq t (cloodoo:list-item-checked milk)))))

          ;; Uncheck
          (cloodoo::db-check-list-item (cloodoo:list-item-id item1) nil)
          (let ((items (cloodoo::db-load-list-items list-id)))
            (let ((milk (find "Milk" items :key #'cloodoo:list-item-title :test #'string=)))
              (is (null (cloodoo:list-item-checked milk)))))

          ;; Delete an item
          (cloodoo::db-delete-list-item (cloodoo:list-item-id item3))
          (let ((items (cloodoo::db-load-list-items list-id)))
            (is (= 2 (length items)))
            (is (null (find "Bread" items :key #'cloodoo:list-item-title :test #'string=)))))))))

(test list-item-section-grouping-test
  "Test that items are correctly grouped by section."
  (with-test-db
    (let ((list-def (cloodoo:make-list-definition "Test List"
                      :sections '("A" "B" "C"))))
      (cloodoo::db-save-list-definition list-def)
      (let ((list-id (cloodoo:list-def-id list-def)))
        ;; Add items in different sections
        (cloodoo::db-save-list-item (cloodoo:make-list-item list-id "Item-A1" :section "A"))
        (cloodoo::db-save-list-item (cloodoo:make-list-item list-id "Item-B1" :section "B"))
        (cloodoo::db-save-list-item (cloodoo:make-list-item list-id "Item-A2" :section "A"))
        (cloodoo::db-save-list-item (cloodoo:make-list-item list-id "No-Section"))

        ;; Use build-list-detail-flat-items
        (let* ((items (cloodoo::db-load-list-items list-id))
               (flat (cloodoo::build-list-detail-flat-items list-def items))
               (section-names (mapcar #'second
                                      (remove-if-not (lambda (e) (eq (first e) :section)) flat)))
               (item-titles (mapcar (lambda (e) (cloodoo:list-item-title (second e)))
                                    (remove-if-not (lambda (e) (eq (first e) :item)) flat))))
          ;; Sections should appear in defined order, only those with items
          (is (equal '("A" "B" "Other") section-names))
          ;; Items should be grouped under their sections
          (is (= 4 (length item-titles)))
          ;; A items come first, then B, then Other
          (is (string= "No-Section" (fourth item-titles))))))))

(test list-name-uniqueness-test
  "Test that list names are unique (case-insensitive)."
  (with-test-db
    (let ((list1 (cloodoo:make-list-definition "Grocery")))
      (cloodoo::db-save-list-definition list1)
      ;; Saving another list with same name (different case) should fail or replace
      ;; The DB has a unique index on LOWER(name) WHERE valid_to IS NULL
      (let ((list2 (cloodoo:make-list-definition "grocery")))
        (signals error
          (cloodoo::db-save-list-definition list2))))))

(test todo-to-list-item-atomic-test
  "Test atomic conversion of a TODO to a list item."
  (with-test-db
    ;; Create a list
    (let ((list-def (cloodoo:make-list-definition "Grocery"
                      :sections '("Dairy"))))
      (cloodoo::db-save-list-definition list-def)
      ;; Create a TODO
      (let ((todo (cloodoo:make-todo "buy milk")))
        (cloodoo::db-save-todo todo)
        ;; Verify TODO exists
        (let ((todos (cloodoo::db-load-todos)))
          (is (= 1 (length todos))))

        ;; Convert to list item
        (let ((result (cloodoo::db-convert-todo-to-list-item
                       (cloodoo:todo-id todo)
                       (cloodoo:list-def-id list-def)
                       "Milk"
                       :section "Dairy")))
          (is (not (null result))))

        ;; TODO should be closed (no longer in active set)
        (let ((todos (cloodoo::db-load-todos)))
          (is (= 0 (length todos))))

        ;; List item should exist
        (let ((items (cloodoo::db-load-list-items (cloodoo:list-def-id list-def))))
          (is (= 1 (length items)))
          (is (string= "Milk" (cloodoo:list-item-title (first items))))
          (is (string= "Dairy" (cloodoo:list-item-section (first items)))))))))

;;── Export Tests ──────────────────────────────────────────────────────────────

(test export-list-test
  "Test exporting a list as formatted text."
  (with-test-db
    (let ((list-def (cloodoo:make-list-definition "Grocery"
                      :sections '("Produce" "Dairy"))))
      (cloodoo::db-save-list-definition list-def)
      (let ((list-id (cloodoo:list-def-id list-def)))
        ;; Add items
        (let ((item1 (cloodoo:make-list-item list-id "Milk" :section "Dairy"))
              (item2 (cloodoo:make-list-item list-id "Apples" :section "Produce")))
          (cloodoo::db-save-list-item item1)
          (cloodoo::db-save-list-item item2)
          ;; Check one item
          (cloodoo::db-check-list-item (cloodoo:list-item-id item1) t)
          ;; Export
          (let* ((items (cloodoo::db-load-list-items list-id))
                 (output (with-output-to-string (s)
                           (cloodoo::export-list list-def items :stream s))))
            ;; Should contain list name
            (is (search "Grocery" output))
            ;; Should contain section headers
            (is (search "PRODUCE" output))
            (is (search "DAIRY" output))
            ;; Should contain items with checkboxes
            (is (search "[x] Milk" output))
            (is (search "[ ] Apples" output))))))))

;;── Multi-User Isolation Tests ────────────────────────────────────────────────

(test multi-user-todo-isolation-test
  "Test that todos saved with different user_ids are isolated."
  (with-test-db
    ;; Save a todo as alice
    (let ((alice-todo (cloodoo:make-todo "Alice's task")))
      (cloodoo::db-save-todo alice-todo :user-id "alice")
      ;; Save a todo as bob
      (let ((bob-todo (cloodoo:make-todo "Bob's task")))
        (cloodoo::db-save-todo bob-todo :user-id "bob")
        ;; Query as alice - should only see alice's todo
        (let ((alice-rows (cloodoo::db-load-current-rows-since "1970-01-01T00:00:00Z"
                                                                :user-id "alice")))
          (is (= 1 (length alice-rows)))
          (is (string= "Alice's task" (gethash "title" (first alice-rows)))))
        ;; Query as bob - should only see bob's todo
        (let ((bob-rows (cloodoo::db-load-current-rows-since "1970-01-01T00:00:00Z"
                                                              :user-id "bob")))
          (is (= 1 (length bob-rows)))
          (is (string= "Bob's task" (gethash "title" (first bob-rows)))))
        ;; Query without user-id - should see both
        (let ((all-rows (cloodoo::db-load-current-rows-since "1970-01-01T00:00:00Z")))
          (is (= 2 (length all-rows))))))))

(test multi-user-todo-delete-isolation-test
  "Test that deleting a todo with user_id only affects that user's data."
  (with-test-db
    (let ((alice-todo (cloodoo:make-todo "Shared title")))
      (cloodoo::db-save-todo alice-todo :user-id "alice")
      (let ((bob-todo (cloodoo:make-todo "Shared title")))
        (cloodoo::db-save-todo bob-todo :user-id "bob")
        ;; Delete alice's todo
        (cloodoo::db-delete-todo (cloodoo:todo-id alice-todo) :user-id "alice")
        ;; Bob's todo should still exist
        (let ((bob-rows (cloodoo::db-load-current-rows-since "1970-01-01T00:00:00Z"
                                                              :user-id "bob")))
          (is (= 1 (length bob-rows))))
        ;; Alice should have nothing
        (let ((alice-rows (cloodoo::db-load-current-rows-since "1970-01-01T00:00:00Z"
                                                                :user-id "alice")))
          (is (= 0 (length alice-rows))))))))

(test multi-user-list-isolation-test
  "Test that list definitions are isolated by user_id."
  (with-test-db
    ;; Create a list as alice
    (let ((alice-list (cloodoo:make-list-definition "Grocery"
                        :sections '("Produce" "Dairy"))))
      (cloodoo::db-save-list-definition alice-list :user-id "alice")
      ;; Create a list as bob
      (let ((bob-list (cloodoo:make-list-definition "Movies")))
        (cloodoo::db-save-list-definition bob-list :user-id "bob")
        ;; Query list defs as alice
        (let ((alice-lists (cloodoo::db-load-current-list-rows-since
                             "1970-01-01T00:00:00Z" :user-id "alice")))
          (is (= 1 (length alice-lists)))
          (is (string= "Grocery" (gethash "name" (first alice-lists)))))
        ;; Query list defs as bob
        (let ((bob-lists (cloodoo::db-load-current-list-rows-since
                           "1970-01-01T00:00:00Z" :user-id "bob")))
          (is (= 1 (length bob-lists)))
          (is (string= "Movies" (gethash "name" (first bob-lists)))))))))

(test multi-user-list-item-isolation-test
  "Test that list items are isolated by user_id."
  (with-test-db
    (let ((list-def (cloodoo:make-list-definition "Shared List")))
      (cloodoo::db-save-list-definition list-def)
      (let ((list-id (cloodoo:list-def-id list-def)))
        ;; Add item as alice
        (let ((alice-item (cloodoo:make-list-item list-id "Alice's item")))
          (cloodoo::db-save-list-item alice-item :user-id "alice")
          ;; Add item as bob
          (let ((bob-item (cloodoo:make-list-item list-id "Bob's item")))
            (cloodoo::db-save-list-item bob-item :user-id "bob")
            ;; Query items as alice
            (let ((alice-items (cloodoo::db-load-current-list-item-rows-since
                                 "1970-01-01T00:00:00Z" :user-id "alice")))
              (is (= 1 (length alice-items)))
              (is (string= "Alice's item" (gethash "title" (first alice-items)))))
            ;; Query items as bob
            (let ((bob-items (cloodoo::db-load-current-list-item-rows-since
                               "1970-01-01T00:00:00Z" :user-id "bob")))
              (is (= 1 (length bob-items)))
              (is (string= "Bob's item" (gethash "title" (first bob-items)))))))))))

(test multi-user-settings-isolation-test
  "Test that settings are isolated by user_id."
  (with-test-db
    ;; Save setting for alice
    (cloodoo::db-save-setting "theme" "dark" :user-id "alice")
    ;; Save setting for bob
    (cloodoo::db-save-setting "theme" "light" :user-id "bob")
    ;; Load alice's settings
    (let ((alice-settings (cloodoo::db-load-all-settings :user-id "alice")))
      (is (= 1 (hash-table-count alice-settings)))
      (is (string= "dark" (getf (gethash "theme" alice-settings) :value))))
    ;; Load bob's settings
    (let ((bob-settings (cloodoo::db-load-all-settings :user-id "bob")))
      (is (= 1 (hash-table-count bob-settings)))
      (is (string= "light" (getf (gethash "theme" bob-settings) :value))))
    ;; Load setting with timestamp for alice
    (multiple-value-bind (value updated-at)
        (cloodoo::db-load-setting-with-timestamp "theme" :user-id "alice")
      (is (string= "dark" value))
      (is (not (null updated-at))))))

(test migration-default-user-id-test
  "Test that existing data gets user_id = 'default' after migration."
  (with-test-db
    ;; Save a todo without user-id (standalone mode)
    (let ((todo (cloodoo:make-todo "Legacy task")))
      (cloodoo::db-save-todo todo)
      ;; Query with user-id "default" should find it
      (let ((rows (cloodoo::db-load-current-rows-since "1970-01-01T00:00:00Z"
                                                        :user-id "default")))
        (is (= 1 (length rows)))
        (is (string= "Legacy task" (gethash "title" (first rows))))))))

;;── Run Tests ──────────────────────────────────────────────────────────────────

(defun run-tests ()
  "Run all tests."
  (run! 'cloodoo-tests))
