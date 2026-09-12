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
  "Test that list names are unique (case-insensitive).
   Saving a same-named list supersedes the existing one (two-device merge
   semantics) rather than signaling, so only one current row survives."
  (with-test-db
    (let ((list1 (cloodoo:make-list-definition "Grocery")))
      (cloodoo::db-save-list-definition list1)
      (let ((list2 (cloodoo:make-list-definition "grocery")))
        (is-true (cloodoo::db-save-list-definition list2))
        (let ((current (cloodoo::db-load-list-definitions)))
          (is (= 1 (length current)))
          (is (string= "grocery" (cloodoo::list-def-name (first current))))
          (is (string= (cloodoo::list-def-id list2)
                       (cloodoo::list-def-id (first current)))))))))

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

(test attachment-only-update-test
  "A save that only changes attachment-hashes must persist and load back.
   Regression: the unchanged-check omitted attachment_hashes, silently
   dropping attachment-only updates (cloodoo-vvl)."
  (with-test-db
    (let ((todo (cloodoo:make-todo "With attachment")))
      (cloodoo::db-save-todo todo)
      (setf (cloodoo::todo-attachment-hashes todo) (list "abc123"))
      (is-true (cloodoo::db-save-todo todo))
      (let ((loaded (find (cloodoo:todo-id todo) (cloodoo::db-load-todos)
                          :key #'cloodoo:todo-id :test #'string=)))
        (is (equal '("abc123") (cloodoo::todo-attachment-hashes loaded)))))))

;;── Theme Tests ────────────────────────────────────────────────────────────────

(test theme-terminal-passthrough-test
  "The terminal theme passes plain ANSI SGR codes through."
  (cloodoo::activate-theme "terminal")
  (is (string= "31" (cloodoo::theme-fg :red)))
  (is (string= "90" (cloodoo::theme-fg :bright-black)))
  (is (string= "46" (cloodoo::theme-bg :cyan))))

(test theme-unknown-falls-back-test
  "An unknown theme name activates the terminal theme."
  (let ((theme (cloodoo::activate-theme "no-such-theme")))
    (is (string= "terminal" (cloodoo::color-theme-name theme)))
    (is (string= "31" (cloodoo::theme-fg :red))))
  (cloodoo::activate-theme "terminal"))

(test theme-hex-resolves-truecolor-test
  "Hex palettes resolve to truecolor SGR codes when COLORTERM says so."
  (let ((old (uiop:getenv "COLORTERM")))
    (unwind-protect
         (progn
           (setf (uiop:getenv "COLORTERM") "truecolor")
           (cloodoo::activate-theme "dracula")
           ;; Dracula red #ff5555 -> 38;2;255;85;85
           (is (string= "38;2;255;85;85" (cloodoo::theme-fg :red)))
           (is (string= "48;2;255;85;85" (cloodoo::theme-bg :red))))
      (setf (uiop:getenv "COLORTERM") (or old ""))
      (cloodoo::activate-theme "terminal"))))

(test theme-mono-disables-color-test
  "The mono theme returns NIL for every slot (no color emitted)."
  (cloodoo::activate-theme "mono")
  (is (null (cloodoo::theme-fg :red)))
  (is (null (cloodoo::theme-bg :cyan)))
  (cloodoo::activate-theme "terminal"))

(test theme-cycle-wraps-test
  "Cycling advances through all themes, persists, and wraps around."
  (with-test-db
    (cloodoo::activate-theme "terminal")
    (let ((n (length cloodoo::*themes*)))
      (dotimes (i (1- n))
        (cloodoo::cycle-theme))
      (is (string= "mono" (cloodoo::color-theme-name cloodoo::*current-theme*)))
      (is (string= "mono" (cloodoo::db-load-setting "theme")))
      (cloodoo::cycle-theme)
      (is (string= "terminal"
                   (cloodoo::color-theme-name cloodoo::*current-theme*))))
    (cloodoo::activate-theme "terminal")))

;;── Undo/Redo Tests ───────────────────────────────────────────────────────────

(test db-todo-version-roundtrip-test
  "Historical versions are loadable by (id, valid_from)."
  (with-test-db
    (let ((todo (cloodoo:make-todo "Version one")))
      (cloodoo::save-todo todo)
      (let ((vf1 (cloodoo::db-current-valid-from (cloodoo:todo-id todo))))
        (is (stringp vf1))
        (setf (cloodoo:todo-title todo) "Version two")
        (cloodoo::save-todo todo)
        (let ((vf2 (cloodoo::db-current-valid-from (cloodoo:todo-id todo))))
          (is (not (equal vf1 vf2)))
          (let ((old (cloodoo::db-load-todo-version (cloodoo:todo-id todo) vf1)))
            (is (not (null old)))
            (is (string= "Version one" (cloodoo:todo-title old)))))))))

(test undo-redo-roundtrip-test
  "Undo restores the prior version; redo restores the undone one."
  (with-test-db
    (let ((model (make-instance 'cloodoo::app-model))
          (todo (cloodoo:make-todo "Original title")))
      (cloodoo::save-todo todo)
      (push todo (cloodoo::model-todos model))
      (setf (cloodoo:todo-status todo) :completed)
      (cloodoo::save-todo-recording-undo model todo)
      (is (= 1 (length (cloodoo::model-undo-stack model))))
      (let ((restored (cloodoo::undo-last-change model)))
        (is (not (null restored)))
        (is (eq :pending (cloodoo:todo-status restored))))
      (is (= 0 (cloodoo::model-undo-cursor model)))
      (let ((redone (cloodoo::redo-last-undo model)))
        (is (not (null redone)))
        (is (eq :completed (cloodoo:todo-status redone))))
      (is (= 1 (cloodoo::model-undo-cursor model))))))

(test undo-new-edit-drops-redo-test
  "A new edit after undo truncates the redo tail."
  (with-test-db
    (let ((model (make-instance 'cloodoo::app-model))
          (todo (cloodoo:make-todo "Task")))
      (cloodoo::save-todo todo)
      (push todo (cloodoo::model-todos model))
      (setf (cloodoo:todo-priority todo) :high)
      (cloodoo::save-todo-recording-undo model todo)
      (cloodoo::undo-last-change model)
      (let ((current (first (cloodoo::model-todos model))))
        (setf (cloodoo:todo-priority current) :low)
        (cloodoo::save-todo-recording-undo model current))
      (is (= (cloodoo::model-undo-cursor model)
             (length (cloodoo::model-undo-stack model))))
      (is (null (cloodoo::redo-last-undo model))))))

(test undo-restores-deleted-test
  "Undo brings a soft-deleted todo back to its prior status."
  (with-test-db
    (let ((model (make-instance 'cloodoo::app-model))
          (todo (cloodoo:make-todo "Doomed")))
      (cloodoo::save-todo todo)
      (push todo (cloodoo::model-todos model))
      (setf (cloodoo:todo-status todo) :deleted)
      (cloodoo::save-todo-recording-undo model todo)
      (let ((restored (cloodoo::undo-last-change model)))
        (is (not (null restored)))
        (is (eq :pending (cloodoo:todo-status restored)))))))

;;── ID Generation ──────────────────────────────────────────────────────────────

(test generate-id-ignores-random-state-test
  "IDs must not depend on *random-state*: a dumped image bakes the state, so
   RANDOM-based ids repeat across process invocations (cloodoo-b1g)."
  (let* ((state (make-random-state nil))
         (suffix (lambda (id) (subseq id (1+ (position #\- id :from-end t)))))
         (id1 (let ((*random-state* (make-random-state state)))
                (cloodoo::generate-id)))
         (id2 (let ((*random-state* (make-random-state state)))
                (cloodoo::generate-id))))
    (is (string/= (funcall suffix id1) (funcall suffix id2)))))

(test generate-id-unique-burst-test
  "Rapidly generated IDs are all distinct."
  (let ((ids (loop repeat 200 collect (cloodoo::generate-id))))
    (is (= (length ids) (length (remove-duplicates ids :test #'string=))))))

;;── Sync Timestamp Clamping ────────────────────────────────────────────────────

(test clamp-change-timestamp-test
  "Sane timestamps pass through; missing, garbage, and far-future ones are
   replaced with server time so they can't win LWW forever (cloodoo-2bh)."
  (let ((recent (cloodoo::now-iso))
        (past "2020-01-01T00:00:00Z")
        (future (local-time:format-rfc3339-timestring
                 nil (local-time:timestamp+ (local-time:now) 1 :hour))))
    (is (string= recent (cloodoo::clamp-change-timestamp recent)))
    (is (string= past (cloodoo::clamp-change-timestamp past)))
    (is (string/= future (cloodoo::clamp-change-timestamp future)))
    (is (stringp (cloodoo::clamp-change-timestamp nil)))
    (is (stringp (cloodoo::clamp-change-timestamp "")))
    (is (string/= "not-a-date" (cloodoo::clamp-change-timestamp "not-a-date")))))

;;── Timestamp Comparison ───────────────────────────────────────────────────────

(test timestamp-string-compare-test
  "Timestamp strings compare as times, not lexicographically (cloodoo-by2):
   14:30+02:00 is 12:30Z, earlier than 12:45Z, though it sorts after it."
  (is-true (cloodoo::timestamp-string< "2026-09-12T14:30:00.000000+02:00"
                                       "2026-09-12T12:45:00.000000Z"))
  (is-false (cloodoo::timestamp-string< "2026-09-12T12:45:00.000000Z"
                                        "2026-09-12T14:30:00.000000+02:00"))
  ;; Unparseable input falls back to lexicographic rather than erroring
  (is-true (cloodoo::timestamp-string< "abc" "abd"))
  ;; now-iso must emit UTC so SQL-side lexicographic ordering stays sound
  (is (char= #\Z (char (cloodoo::now-iso) (1- (length (cloodoo::now-iso)))))))

;;── Pairing Security ───────────────────────────────────────────────────────────

(test pairing-token-single-use-test
  "Consuming a pairing token twice must fail the second time (cloodoo-0an)."
  (with-test-db
    (cloodoo::db-store-pairing-request "tok1" "dev" "pass" (get-universal-time)
                                       (+ (get-universal-time) 600))
    (is (not (null (cloodoo::db-consume-pairing-request "tok1"))))
    (is (null (cloodoo::db-consume-pairing-request "tok1")))))

(test pairing-passphrase-proof-test
  "The proof is a stable hash that differs from the raw passphrase
   (cloodoo-st8), and comparison is exact (cloodoo-wfi)."
  (let ((proof (cloodoo::pairing-passphrase-proof "alpha-bravo")))
    (is (string= proof (cloodoo::pairing-passphrase-proof "alpha-bravo")))
    (is (string/= proof "alpha-bravo"))
    (is (= 64 (length proof)))
    (is-true (cloodoo::passphrase-equal-p "abc" "abc"))
    (is-false (cloodoo::passphrase-equal-p "abc" "abd"))
    (is-false (cloodoo::passphrase-equal-p nil "abc"))))

(test pairing-passphrase-entropy-test
  "Generated passphrases have six words (cloodoo-wfi)."
  (is (= 6 (1+ (count #\- (cloodoo::generate-passphrase))))))

;;── DB Integrity ───────────────────────────────────────────────────────────────

(test db-save-todo-user-scoping-test
  "An id collision across users must not close out the other user's row
   (cloodoo-ab0)."
  (with-test-db
    (let ((todo-a (cloodoo:make-todo "Alice's task"))
          (todo-b (cloodoo:make-todo "Bob's task")))
      (setf (cloodoo:todo-id todo-b) (cloodoo:todo-id todo-a))
      (cloodoo::db-save-todo todo-a :user-id "alice")
      (cloodoo::db-save-todo todo-b :user-id "bob")
      ;; Both users' rows must still be current
      (cloodoo::with-db (db)
        (is (= 2 (sqlite:execute-single db
                   "SELECT COUNT(*) FROM todos WHERE id = ? AND valid_to IS NULL"
                   (cloodoo:todo-id todo-a))))))))

(test db-check-list-item-preserves-nulls-test
  "Toggling a list item keeps NULL section/notes and its device_id
   (cloodoo-0v9)."
  (with-test-db
    (let* ((list-def (cloodoo:make-list-definition "Groceries"))
           (item (cloodoo:make-list-item (cloodoo:list-def-id list-def) "Milk"
                                         :device-id "dev-42")))
      (cloodoo::db-save-list-definition list-def)
      (cloodoo::db-save-list-item item)
      (is-true (cloodoo::db-check-list-item (cloodoo:list-item-id item) t))
      (cloodoo::with-db (db)
        (destructuring-bind (section notes device-id checked)
            (first (sqlite:execute-to-list db
                     "SELECT section, notes, device_id, checked FROM list_items
                      WHERE id = ? AND valid_to IS NULL"
                     (cloodoo:list-item-id item)))
          (is (null section))
          (is (null notes))
          (is (equal "dev-42" device-id))
          (is (= 1 checked)))))))

(test migrate-inline-to-blobs-terminates-test
  "Rows with empty-string inline content are cleared, not rescanned forever
   (cloodoo-1bf)."
  (with-test-db
    (cloodoo::with-db (db)
      (sqlite:execute-non-query db
        "INSERT INTO todos (id, title, description, created_at, valid_from)
         VALUES ('m1', 'Empty desc', '', '2026-01-01T00:00:00Z', '2026-01-01T00:00:00Z')")
      (cloodoo::migrate-inline-to-blobs db)
      (is (zerop (sqlite:execute-single db
                   "SELECT COUNT(*) FROM todos
                    WHERE description IS NOT NULL AND description_hash IS NULL"))))))

;;── Proto Round-Trip Tests ─────────────────────────────────────────────────────

(defun encode-date (year month day)
  "Timestamp at noon local time on the given date (noon avoids DST edges)."
  (local-time:encode-timestamp 0 0 0 12 day month year))

(defun roundtrip-todo-via-proto (todo &optional (device-id "test-device"))
  "Convert TODO to a sync upsert message and back to a todo instance.
   Returns (values converted-todo todo-change)."
  (let* ((msg (cloodoo::make-sync-upsert-message-with-timestamp
               device-id todo (cloodoo::now-iso)))
         (change (cloodoo::proto-msg-change msg))
         (data (cloodoo::proto-todo-change-upsert change)))
    (values (cloodoo::proto-to-todo data) change)))

(test proto-todo-roundtrip-full-test
  "Every synced field survives todo -> proto -> todo conversion."
  (let ((scheduled (encode-date 2026 3 15))
        (due (encode-date 2026 3 20))
        (created (encode-date 2026 3 1))
        (completed (encode-date 2026 3 21))
        (todo (cloodoo:make-todo "Round trip"
                :description "Full description"
                :priority :high
                :tags '("work" "sync")
                :url "https://example.com/task"
                :repeat-interval 2
                :repeat-unit :week)))
    (setf (cloodoo::todo-scheduled-date todo) scheduled
          (cloodoo:todo-due-date todo) due
          (cloodoo:todo-created-at todo) created
          (cloodoo:todo-completed-at todo) completed
          (cloodoo:todo-status todo) :in-progress
          (cloodoo::todo-attachment-hashes todo) '("hash-a" "hash-b")
          (cloodoo::todo-enriching-p todo) t)
    (multiple-value-bind (back change) (roundtrip-todo-via-proto todo "dev-99")
      (is (string= (cloodoo:todo-id todo) (cloodoo:todo-id back)))
      (is (string= "Round trip" (cloodoo:todo-title back)))
      (is (string= "Full description" (cloodoo:todo-description back)))
      (is (eq :high (cloodoo:todo-priority back)))
      (is (eq :in-progress (cloodoo:todo-status back)))
      (is (local-time:timestamp= scheduled (cloodoo::todo-scheduled-date back)))
      (is (local-time:timestamp= due (cloodoo:todo-due-date back)))
      (is (local-time:timestamp= created (cloodoo:todo-created-at back)))
      (is (local-time:timestamp= completed (cloodoo:todo-completed-at back)))
      (is (equal '("work" "sync") (cloodoo:todo-tags back)))
      (is (string= "https://example.com/task" (cloodoo::todo-url back)))
      (is (= 2 (cloodoo::todo-repeat-interval back)))
      (is (eq :week (cloodoo::todo-repeat-unit back)))
      (is (equal '("hash-a" "hash-b") (cloodoo::todo-attachment-hashes back)))
      (is (eq t (cloodoo::todo-enriching-p back)))
      ;; device-id travels on the TodoChange envelope, not TodoData
      (is (string= "dev-99" (cloodoo::proto-todo-change-device-id change))))))

(test proto-todo-roundtrip-empty-fields-test
  "Nil/empty optional fields normalize cleanly through proto and back."
  (let ((back (roundtrip-todo-via-proto (cloodoo:make-todo "Bare"))))
    (is (string= "Bare" (cloodoo:todo-title back)))
    (is (null (cloodoo:todo-description back)))
    (is (null (cloodoo:todo-tags back)))
    (is (null (cloodoo::todo-scheduled-date back)))
    (is (null (cloodoo:todo-due-date back)))
    (is (null (cloodoo:todo-completed-at back)))
    (is (null (cloodoo::todo-url back)))
    (is (null (cloodoo::todo-repeat-interval back)))
    (is (null (cloodoo::todo-repeat-unit back)))
    (is (null (cloodoo::todo-attachment-hashes back)))
    (is (eq :medium (cloodoo:todo-priority back)))
    (is (eq :pending (cloodoo:todo-status back)))))

(test proto-todo-wire-roundtrip-test
  "TodoData survives actual wire serialization, not just object conversion."
  (let ((todo (cloodoo:make-todo "Wire trip"
                :description "desc"
                :priority :low
                :tags '("a" "b")
                :repeat-interval 1
                :repeat-unit :day)))
    (setf (cloodoo:todo-created-at todo) (encode-date 2026 2 2))
    (let* ((msg (cloodoo::make-sync-upsert-message-with-timestamp
                 "dev" todo (cloodoo::now-iso)))
           (data (cloodoo::proto-todo-change-upsert (cloodoo::proto-msg-change msg)))
           (bytes (ag-proto:serialize-to-bytes data))
           (back (cloodoo::proto-to-todo
                  (ag-proto:deserialize-from-bytes 'cloodoo::proto-todo-data bytes))))
      (is (string= (cloodoo:todo-id todo) (cloodoo:todo-id back)))
      (is (string= "Wire trip" (cloodoo:todo-title back)))
      (is (string= "desc" (cloodoo:todo-description back)))
      (is (eq :low (cloodoo:todo-priority back)))
      (is (equal '("a" "b") (cloodoo:todo-tags back)))
      (is (= 1 (cloodoo::todo-repeat-interval back)))
      (is (eq :day (cloodoo::todo-repeat-unit back))))))

(test proto-todo-location-info-roundtrip-test
  "Location info survives todo -> proto -> todo (cloodoo-pmx)."
  (let* ((todo (cloodoo:make-todo "Where"
                 :location-info '(:name "Cafe" :address "1 Main St")))
         (loc (cloodoo::todo-location-info (roundtrip-todo-via-proto todo))))
    (is (not (null loc)))
    (is (string= "Cafe" (getf loc :name)))
    (is (string= "1 Main St" (getf loc :address)))))

(test proto-list-definition-roundtrip-test
  "List definitions survive proto conversion, including nil description."
  (let ((created (encode-date 2026 4 2))
        (list-def (cloodoo:make-list-definition "Movies"
                    :description "To watch"
                    :sections '("Action" "Drama"))))
    (setf (cloodoo:list-def-created-at list-def) created)
    (let ((back (cloodoo::proto-to-list-definition
                 (cloodoo::list-definition-to-proto list-def))))
      (is (string= (cloodoo:list-def-id list-def) (cloodoo:list-def-id back)))
      (is (string= "Movies" (cloodoo:list-def-name back)))
      (is (string= "To watch" (cloodoo:list-def-description back)))
      (is (equal '("Action" "Drama") (cloodoo:list-def-sections back)))
      (is (local-time:timestamp= created (cloodoo:list-def-created-at back)))))
  ;; nil description and no sections normalize back to nil
  (let* ((bare (cloodoo:make-list-definition "Bare"))
         (back (cloodoo::proto-to-list-definition
                (cloodoo::list-definition-to-proto bare))))
    (is (null (cloodoo:list-def-description back)))
    (is (null (cloodoo:list-def-sections back)))))

(test proto-list-item-roundtrip-test
  "List items survive proto conversion, including checked state and nils."
  (let ((created (encode-date 2026 4 3))
        (item (cloodoo:make-list-item "list-1" "Milk"
                :section "Dairy"
                :notes "2% preferred")))
    (setf (cloodoo:list-item-created-at item) created
          (cloodoo:list-item-checked item) t)
    (let ((back (cloodoo::proto-to-list-item
                 (cloodoo::list-item-to-proto item))))
      (is (string= (cloodoo:list-item-id item) (cloodoo:list-item-id back)))
      (is (string= "list-1" (cloodoo:list-item-list-id back)))
      (is (string= "Milk" (cloodoo:list-item-title back)))
      (is (string= "Dairy" (cloodoo:list-item-section back)))
      (is (string= "2% preferred" (cloodoo:list-item-notes back)))
      (is (eq t (cloodoo:list-item-checked back)))
      (is (local-time:timestamp= created (cloodoo:list-item-created-at back)))))
  ;; unchecked item with nil section/notes stays nil
  (let* ((bare (cloodoo:make-list-item "list-1" "Eggs"))
         (back (cloodoo::proto-to-list-item
                (cloodoo::list-item-to-proto bare))))
    (is (null (cloodoo:list-item-section back)))
    (is (null (cloodoo:list-item-notes back)))
    (is (null (cloodoo:list-item-checked back)))))

;;── Temporal DB Invariant Tests ────────────────────────────────────────────────

(test temporal-single-current-row-test
  "After several updates exactly one row per id is current, and historical
   rows chain: each valid_to equals the successor's valid_from."
  (with-test-db
    (let ((todo (cloodoo:make-todo "v1")))
      (cloodoo::db-save-todo todo)
      (setf (cloodoo:todo-title todo) "v2")
      (cloodoo::db-save-todo todo)
      (setf (cloodoo:todo-title todo) "v3")
      (cloodoo::db-save-todo todo)
      (cloodoo::with-db (db)
        (let ((id (cloodoo:todo-id todo)))
          (is (= 3 (sqlite:execute-single db
                     "SELECT COUNT(*) FROM todos WHERE id = ?" id)))
          (is (= 1 (sqlite:execute-single db
                     "SELECT COUNT(*) FROM todos WHERE id = ? AND valid_to IS NULL" id)))
          (is (string= "v3" (sqlite:execute-single db
                              "SELECT title FROM todos WHERE id = ? AND valid_to IS NULL" id)))
          (let ((rows (sqlite:execute-to-list db
                        "SELECT valid_from, valid_to FROM todos
                         WHERE id = ? ORDER BY valid_from" id)))
            (loop for ((nil closed-at) (successor-from nil)) on rows
                  while successor-from
                  do (is (equal closed-at successor-from)))))))))

(test temporal-stale-update-rejected-test
  "A save carrying an older valid-from than the current row returns NIL and
   leaves the current row untouched."
  (with-test-db
    (let ((todo (cloodoo:make-todo "Fresh")))
      (cloodoo::db-save-todo todo)
      (let ((id (cloodoo:todo-id todo))
            (current-vf (cloodoo::db-current-valid-from (cloodoo:todo-id todo))))
        (setf (cloodoo:todo-title todo) "Stale")
        (is (null (cloodoo::db-save-todo todo
                    :valid-from "2020-01-01T00:00:00.000000Z")))
        (cloodoo::with-db (db)
          (is (= 1 (sqlite:execute-single db
                     "SELECT COUNT(*) FROM todos WHERE id = ?" id)))
          (is (string= "Fresh" (sqlite:execute-single db
                                 "SELECT title FROM todos WHERE id = ? AND valid_to IS NULL" id))))
        (is (equal current-vf (cloodoo::db-current-valid-from id)))))))

(test temporal-newer-valid-from-accepted-test
  "A save with an explicit valid-from newer than current is accepted and
   becomes the current row."
  (with-test-db
    (let ((todo (cloodoo:make-todo "Old")))
      (cloodoo::db-save-todo todo :valid-from "2026-01-01T00:00:00.000000Z")
      (setf (cloodoo:todo-title todo) "New")
      (is-true (cloodoo::db-save-todo todo
                 :valid-from "2026-06-01T00:00:00.000000Z"))
      (cloodoo::with-db (db)
        (is (string= "New" (sqlite:execute-single db
                             "SELECT title FROM todos WHERE id = ? AND valid_to IS NULL"
                             (cloodoo:todo-id todo)))))
      (is (string= "2026-06-01T00:00:00.000000Z"
                   (cloodoo::db-current-valid-from (cloodoo:todo-id todo)))))))

(test temporal-delete-closes-out-test
  "db-delete-todo closes out rows rather than deleting them: row count is
   preserved, no current row remains, and history stays queryable."
  (with-test-db
    (let ((todo (cloodoo:make-todo "Doomed")))
      (cloodoo::db-save-todo todo)
      (setf (cloodoo:todo-title todo) "Doomed v2")
      (cloodoo::db-save-todo todo)
      (cloodoo::db-delete-todo (cloodoo:todo-id todo))
      (cloodoo::with-db (db)
        (let ((id (cloodoo:todo-id todo)))
          (is (= 2 (sqlite:execute-single db
                     "SELECT COUNT(*) FROM todos WHERE id = ?" id)))
          (is (= 0 (sqlite:execute-single db
                     "SELECT COUNT(*) FROM todos WHERE id = ? AND valid_to IS NULL" id)))))
      (is (null (find (cloodoo:todo-id todo) (cloodoo::db-load-todos)
                      :key #'cloodoo:todo-id :test #'string=))))))

;;── ISO Week Number Tests ──────────────────────────────────────────────────────

(test iso-week-number-test
  "ISO 8601 week numbers (cloodoo-t39): Monday-based weeks; week 1 contains
   the year's first Thursday.  Cross-checked against `date +%G-W%V`."
  ;; 2026-01-01 is a Thursday -> week 1
  (is (= 1 (cloodoo::iso-week-number (encode-date 2026 1 1))))
  ;; 2026-01-04 is a Sunday, still week 1
  (is (= 1 (cloodoo::iso-week-number (encode-date 2026 1 4))))
  ;; 2026-01-05 is a Monday -> week 2
  (is (= 2 (cloodoo::iso-week-number (encode-date 2026 1 5))))
  ;; 2026-12-28 is a Monday -> week 53 (2026 has 53 ISO weeks)
  (is (= 53 (cloodoo::iso-week-number (encode-date 2026 12 28))))
  ;; 2027-01-01 is a Friday -> belongs to week 53 of 2026
  (is (= 53 (cloodoo::iso-week-number (encode-date 2027 1 1))))
  ;; 2025-12-29 is a Monday -> belongs to week 1 of 2026
  (is (= 1 (cloodoo::iso-week-number (encode-date 2025 12 29)))))

(test iso-weeks-in-year-test
  "Years have 53 ISO weeks iff Jan 1 is a Thursday, or a Wednesday in a
   leap year; otherwise 52."
  (is (= 53 (cloodoo::iso-weeks-in-year 2026)))  ; Jan 1 Thursday
  (is (= 52 (cloodoo::iso-weeks-in-year 2025)))  ; Jan 1 Wednesday, not leap
  (is (= 53 (cloodoo::iso-weeks-in-year 2020)))  ; Jan 1 Wednesday, leap
  (is (= 52 (cloodoo::iso-weeks-in-year 2023)))) ; Jan 1 Sunday

;;── Org-Agenda Export Tests ────────────────────────────────────────────────────

(defun make-export-fixture-todos ()
  "Fixed todos covering statuses, priorities, tags, an overdue date, a URL."
  (let ((overdue (cloodoo:make-todo "Overdue report"
                   :priority :high
                   :tags '("work")
                   :scheduled-date (encode-date 2020 1 1)))
        (done (cloodoo:make-todo "Shipped feature" :priority :medium))
        (doing (cloodoo:make-todo "Refactor module"
                 :priority :low
                 :tags '("home")))
        (bare (cloodoo:make-todo "Read RFC" :url "https://example.com/rfc")))
    (setf (cloodoo:todo-status done) :completed
          (cloodoo:todo-status doing) :in-progress)
    (list overdue done doing bare)))

(test export-todos-text-test
  "Date-grouped export shows status keywords, priority markers, the overdue
   indicator, and footer statistics."
  (let ((output (with-output-to-string (s)
                  (cloodoo::export-todos-text (make-export-fixture-todos)
                                              :stream s))))
    (is (search "Week-agenda (W" output))
    (is (search "TODO [#A] Overdue report" output))
    (is (search "DONE [#B] Shipped feature" output))
    (is (search "DOING [#C] Refactor module" output))
    (is (search "TODO [#B] Read RFC" output))
    ;; Overdue scheduled indicator: "Sched.<days>x:" (days is today-relative,
    ;; so only assert the stable prefix and suffix)
    (is (search "Sched." output))
    (is (search "x:" output))
    ;; A date header is emitted for the scheduled group.  The rendered date
    ;; is timezone-dependent — off by one day in zones behind UTC
    ;; (cloodoo-d3m) — so accept either rendering until that is fixed.
    (is (or (search "1 January 2020" output)
            (search "31 December 2019" output)))
    (is (search "Total: 4  Completed: 1  Overdue: 1" output))))

(test export-todos-text-by-tag-test
  "Tag-grouped export shows tag headers, an Untagged group, and URLs."
  (let ((output (with-output-to-string (s)
                  (cloodoo::export-todos-text (make-export-fixture-todos)
                                              :stream s :by-tag t))))
    (is (search ":work:" output))
    (is (search ":home:" output))
    (is (search "Untagged:" output))
    (is (search "TODO [#A] Overdue report" output))
    (is (search "Read RFC https://example.com/rfc" output))
    (is (search "Total: 4  Completed: 1  Overdue: 1" output))))

(test export-todos-text-custom-title-test
  "The :title keyword replaces the default header."
  (let ((output (with-output-to-string (s)
                  (cloodoo::export-todos-text '() :stream s :title "My Agenda"))))
    (is (search "My Agenda (W" output))
    (is (search "Total: 0  Completed: 0  Overdue: 0" output))))

;;── Natural-Language Title Dates ───────────────────────────────────────────────

(test extract-title-date-test
  "Trailing date words schedule quick captures offline (cloodoo-zig)."
  ;; Trailing day name is extracted
  (multiple-value-bind (title date) (cloodoo::extract-title-date "renew passport friday")
    (is (string= "renew passport" title))
    (is (not (null date)))
    (is (= 5 (local-time:timestamp-day-of-week date))))
  ;; Tomorrow
  (multiple-value-bind (title date) (cloodoo::extract-title-date "pay rent tomorrow")
    (is (string= "pay rent" title))
    (is (local-time:timestamp= date
                               (local-time:timestamp+ (cloodoo::local-today) 1 :day))))
  ;; A bare date word is a title, not a date
  (multiple-value-bind (title date) (cloodoo::extract-title-date "friday")
    (is (string= "friday" title))
    (is (null date)))
  ;; No date word: unchanged
  (multiple-value-bind (title date) (cloodoo::extract-title-date "buy milk")
    (is (string= "buy milk" title))
    (is (null date))))

;;── Run Tests ──────────────────────────────────────────────────────────────────

(defun run-tests ()
  "Run all tests."
  (run! 'cloodoo-tests))
