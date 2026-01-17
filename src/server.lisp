;;; server.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cloodoo)

;;── HTTP API Server for Browser Extension ──────────────────────────────────────

(defvar *server* nil
  "The Hunchentoot acceptor instance.")

(defvar *default-port* 9876
  "Default port for the API server.")

;;── Routes ─────────────────────────────────────────────────────────────────────

(easy-routes:defroute api-health ("/api/health" :method :get) ()
  (setf (hunchentoot:content-type*) "application/json")
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "status" ht) "ok")
    (jzon:stringify ht)))

(easy-routes:defroute api-list-todos ("/api/todos" :method :get) ()
  (setf (hunchentoot:content-type*) "application/json")
  (let ((todos (load-todos)))
    (jzon:stringify (coerce (mapcar #'todo-to-hash-table todos) 'vector))))

(easy-routes:defroute api-create-todo ("/api/todos" :method :post) ()
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((body (hunchentoot:raw-post-data :force-text t))
         (data (jzon:parse body))
         (title (gethash "title" data))
         (description (gethash "description" data))
         (priority (gethash "priority" data))
         (tags (gethash "tags" data))
         (due-date (gethash "due_date" data)))
    (when title
      (let ((todo (make-todo title
                             :description description
                             :priority (when priority
                                         (intern (string-upcase priority) :keyword))
                             :tags tags
                             :due-date (when due-date
                                         (lt:parse-rfc3339-timestring due-date))))
            (todos (load-todos)))
        (push todo todos)
        (save-todos todos)
        (setf (hunchentoot:return-code*) 201)
        (jzon:stringify (todo-to-hash-table todo))))))

;;── Server Control ─────────────────────────────────────────────────────────────

(defun start-server (&key (port *default-port*))
  "Start the API server on the specified port."
  (when *server*
    (stop-server))
  (setf *server* (make-instance 'easy-routes:easy-routes-acceptor
                                :port port
                                :address "127.0.0.1"))
  (hunchentoot:start *server*)
  (format t "~&Cloodoo API server running on http://127.0.0.1:~A~%" port)
  *server*)

(defun stop-server ()
  "Stop the API server."
  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil)
    (format t "~&Cloodoo API server stopped.~%")))
