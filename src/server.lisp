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

(defvar *default-address* "127.0.0.1"
  "Default bind address for the API server.")

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
         (tags-raw (gethash "tags" data))
         (tags (when tags-raw
                 (parse-tags (if (stringp tags-raw)
                                 tags-raw
                                 (coerce tags-raw 'list)))))
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

;;── Sync API Routes ──────────────────────────────────────────────────────────────

(easy-routes:defroute api-device ("/api/device" :method :get) ()
  "Return this device's ID and current server time."
  (setf (hunchentoot:content-type*) "application/json")
  (let ((response (make-hash-table :test #'equal)))
    (setf (gethash "device_id" response) (get-device-id))
    (setf (gethash "server_time" response) (now-iso))
    (jzon:stringify response)))

(easy-routes:defroute api-sync-get ("/api/sync" :method :get) (since)
  "Return all rows where valid_from > since timestamp.
   Query parameter: since (ISO 8601 timestamp)"
  (setf (hunchentoot:content-type*) "application/json")
  (let ((response (make-hash-table :test #'equal)))
    (setf (gethash "device_id" response) (get-device-id))
    (setf (gethash "server_time" response) (now-iso))
    (if since
        (let ((rows (db-load-rows-since since)))
          (setf (gethash "rows" response) (coerce rows 'vector)))
        ;; If no since provided, return all rows since epoch
        (let ((rows (db-load-rows-since "1970-01-01T00:00:00Z")))
          (setf (gethash "rows" response) (coerce rows 'vector))))
    (jzon:stringify response)))

(easy-routes:defroute api-sync-post ("/api/sync" :method :post) ()
  "Receive rows from another device to merge.
   Request body: {device_id: string, rows: array}"
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((body (hunchentoot:raw-post-data :force-text t))
         (data (jzon:parse body))
         (source-device-id (gethash "device_id" data))
         (rows-data (gethash "rows" data))
         (response (make-hash-table :test #'equal)))
    (if (and source-device-id rows-data)
        (let ((rows (coerce rows-data 'list)))
          (multiple-value-bind (accepted rejected)
              (db-merge-rows rows source-device-id)
            (setf (gethash "accepted" response) accepted)
            (setf (gethash "rejected" response) rejected)
            (setf (gethash "server_time" response) (now-iso))
            (jzon:stringify response)))
        (progn
          (setf (hunchentoot:return-code*) 400)
          (setf (gethash "error" response) "Missing device_id or rows")
          (jzon:stringify response)))))

;;── Server Control ─────────────────────────────────────────────────────────────

(defun start-server (&key (port *default-port*) (address *default-address*))
  "Start the API server on the specified port and address.
   Use address \"0.0.0.0\" to listen on all interfaces (needed for Tailscale sync)."
  (when *server*
    (stop-server))
  (setf *server* (make-instance 'easy-routes:easy-routes-acceptor
                                :port port
                                :address address))
  (hunchentoot:start *server*)
  (format t "~&Cloodoo API server running on http://~A:~A~%" address port)
  (format t "~&Device ID: ~A~%" (get-device-id))
  *server*)

(defun stop-server ()
  "Stop the API server."
  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil)
    (format t "~&Cloodoo API server stopped.~%")))
