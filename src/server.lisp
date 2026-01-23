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

;;── Pairing API Routes ─────────────────────────────────────────────────────────

(easy-routes:defroute api-pair-info ("/pair/:token" :method :get) ()
  "Return pairing info (device name, expiry) for a token.
   Does NOT consume the token - use POST to download the certificate."
  (setf (hunchentoot:content-type*) "application/json")
  (let ((request (get-pairing-request token))
        (response (make-hash-table :test #'equal)))
    (if request
        (progn
          (setf (gethash "device_name" response) (pairing-request-device-name request))
          (setf (gethash "expires_in" response)
                (- (pairing-request-expires-at request) (get-universal-time)))
          (jzon:stringify response))
        (progn
          (setf (hunchentoot:return-code*) 404)
          (setf (gethash "error" response) "Invalid or expired pairing token")
          (jzon:stringify response)))))

(easy-routes:defroute api-pair-download ("/pair/:token" :method :post) ()
  "Download the .p12 certificate bundle for a pairing token.
   This consumes the token (single-use).
   Returns the .p12 file as application/x-pkcs12."
  (let ((request (consume-pairing-request token)))
    (if request
        (let ((p12-path (pairing-request-p12-path request)))
          (if (probe-file p12-path)
              (progn
                (setf (hunchentoot:content-type*) "application/x-pkcs12")
                (setf (hunchentoot:header-out "Content-Disposition")
                      (format nil "attachment; filename=\"~A.p12\""
                              (pairing-request-device-name request)))
                ;; Read and return the file contents
                (with-open-file (stream p12-path
                                        :direction :input
                                        :element-type '(unsigned-byte 8))
                  (let ((data (make-array (file-length stream)
                                          :element-type '(unsigned-byte 8))))
                    (read-sequence data stream)
                    data)))
              (progn
                (setf (hunchentoot:content-type*) "application/json")
                (setf (hunchentoot:return-code*) 500)
                (jzon:stringify (alexandria:plist-hash-table
                                 '("error" "Certificate file not found")
                                 :test #'equal)))))
        (progn
          (setf (hunchentoot:content-type*) "application/json")
          (setf (hunchentoot:return-code*) 404)
          (jzon:stringify (alexandria:plist-hash-table
                           '("error" "Invalid or expired pairing token")
                           :test #'equal))))))

(easy-routes:defroute api-pair-status ("/api/pair/status" :method :get) ()
  "Check if CA is initialized and return server info for pairing."
  (setf (hunchentoot:content-type*) "application/json")
  (let ((response (make-hash-table :test #'equal)))
    (setf (gethash "ca_initialized" response) (ca-initialized-p))
    (setf (gethash "server_cert_initialized" response) (server-cert-initialized-p))
    (setf (gethash "device_id" response) (get-device-id))
    (setf (gethash "local_ips" response) (coerce (detect-local-ips) 'vector))
    (jzon:stringify response)))

(easy-routes:defroute api-pair-debug ("/api/pair/debug" :method :get) ()
  "Debug: list all pending pairing tokens."
  (setf (hunchentoot:content-type*) "application/json")
  (let ((tokens nil))
    (bt:with-lock-held (*pairing-lock*)
      (maphash (lambda (token request)
                 (push (list :token token
                             :device (pairing-request-device-name request)
                             :expires-in (- (pairing-request-expires-at request)
                                           (get-universal-time)))
                       tokens))
               *pending-pairings*))
    (jzon:stringify (or tokens #()))))

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
