;;; lists.lisp - Generic list management (grocery lists, watchlists, etc.)
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cloodoo)

;;── List Definition Class ────────────────────────────────────────────────────

(defclass list-definition ()
  ((id
    :initarg :id
    :accessor list-def-id
    :type string
    :documentation "Unique identifier for the list definition.")
   (name
    :initarg :name
    :accessor list-def-name
    :type string
    :documentation "Display name (e.g., \"Grocery\"). Case-insensitive unique.")
   (description
    :initarg :description
    :initform nil
    :accessor list-def-description
    :type (or null string)
    :documentation "LLM hint describing what belongs on this list.")
   (sections
    :initarg :sections
    :initform nil
    :accessor list-def-sections
    :type list
    :documentation "Ordered list of section name strings.")
   (created-at
    :initarg :created-at
    :accessor list-def-created-at
    :type lt:timestamp
    :documentation "Timestamp of creation.")
   (device-id
    :initarg :device-id
    :initform nil
    :accessor list-def-device-id
    :type (or null string)
    :documentation "UUID of the device that created/modified this definition."))
  (:documentation "A user-defined list (e.g., Grocery, Movies to Watch)."))

(defun make-list-definition (name &key description sections device-id)
  "Create a new list definition."
  (make-instance 'list-definition
                 :id (generate-id)
                 :name name
                 :description description
                 :sections sections
                 :device-id device-id
                 :created-at (lt:now)))

(defmethod print-object ((def list-definition) stream)
  (print-unreadable-object (def stream :type t :identity t)
    (format stream "~A (~A)" (list-def-name def) (list-def-id def))))

;;── List Item Class ──────────────────────────────────────────────────────────

(defclass list-item ()
  ((id
    :initarg :id
    :accessor list-item-id
    :type string
    :documentation "Unique identifier for the list item.")
   (list-id
    :initarg :list-id
    :accessor list-item-list-id
    :type string
    :documentation "ID of the parent list definition.")
   (title
    :initarg :title
    :accessor list-item-title
    :type string
    :documentation "Item text (e.g., \"Milk (2%)\").")
   (section
    :initarg :section
    :initform nil
    :accessor list-item-section
    :type (or null string)
    :documentation "Section within the list (e.g., \"Dairy\").")
   (checked
    :initarg :checked
    :initform nil
    :accessor list-item-checked
    :type boolean
    :documentation "Whether the item is checked off.")
   (notes
    :initarg :notes
    :initform nil
    :accessor list-item-notes
    :type (or null string)
    :documentation "Optional notes.")
   (created-at
    :initarg :created-at
    :accessor list-item-created-at
    :type lt:timestamp
    :documentation "Timestamp of creation.")
   (device-id
    :initarg :device-id
    :initform nil
    :accessor list-item-device-id
    :type (or null string)
    :documentation "UUID of the device that created/modified this item."))
  (:documentation "An item on a user-defined list."))

(defun make-list-item (list-id title &key section notes device-id)
  "Create a new list item."
  (make-instance 'list-item
                 :id (generate-id)
                 :list-id list-id
                 :title title
                 :section section
                 :notes notes
                 :device-id device-id
                 :created-at (lt:now)))

(defmethod print-object ((item list-item) stream)
  (print-unreadable-object (item stream :type t :identity t)
    (format stream "~A [~A] ~A"
            (list-item-id item)
            (if (list-item-checked item) "x" " ")
            (list-item-title item))))

;;── Default Lists ────────────────────────────────────────────────────────────

(defvar *default-lists*
  (list
   (list :name "Grocery"
         :description "Items to buy at a grocery store or supermarket."
         :sections '("Produce" "Bakery" "Deli" "Meat & Seafood" "Dairy"
                     "Frozen" "Pantry" "Snacks" "Beverages"
                     "Cleaning & Household" "Paper Products"
                     "Health & Beauty" "Other")))
  "Default list definitions shipped with Cloodoo.")

;;── Change Notification Hooks ────────────────────────────────────────────────

(defvar *list-change-hook* nil
  "Function to call when a list definition is saved. Called with (list-def).
   Used by sync server to broadcast changes to connected clients.")

(defvar *list-item-change-hook* nil
  "Function to call when a list item is saved. Called with (list-item).
   Used by sync server to broadcast changes to connected clients.")

(defvar *list-delete-hook* nil
  "Function to call when a list definition is deleted. Called with (list-def-id).
   Used by sync server to broadcast deletions to connected clients.")

(defvar *list-item-delete-hook* nil
  "Function to call when a list item is deleted. Called with (list-item-id).
   Used by sync server to broadcast deletions to connected clients.")

;;── Default List Seeding ─────────────────────────────────────────────────────

(defun seed-default-lists ()
  "Seed default list definitions if none exist."
  (let ((existing (db-load-list-definitions)))
    (when (null existing)
      (let ((*suppress-change-notifications* t))
        (dolist (def-plist *default-lists*)
          (let ((list-def (make-list-definition
                           (getf def-plist :name)
                           :description (getf def-plist :description)
                           :sections (getf def-plist :sections)
                           :device-id (get-device-id))))
            (db-save-list-definition list-def)))))))
