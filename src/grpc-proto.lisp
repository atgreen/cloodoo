;;;; grpc-proto.lisp - Generated from proto/cloodoo_sync.proto
;;;;
;;;; Modified to use cloodoo package with proto- prefix to avoid conflicts
;;;; with existing model classes.

(in-package #:cloodoo)

;;; ============================================================================
;;; Proto Message Classes (prefixed with proto- to avoid model conflicts)
;;; ============================================================================

(defclass proto-location-info (ag-proto:proto-message)
  ((name :initarg :name :accessor proto-location-name :initform "" :type string)
   (address :initarg :address :accessor proto-location-address :initform "" :type string)
   (phone :initarg :phone :accessor proto-location-phone :initform "" :type string)
   (map-url :initarg :map-url :accessor proto-location-map-url :initform "" :type string)
   (website :initarg :website :accessor proto-location-website :initform "" :type string))
  (:documentation "Proto message: LocationInfo"))

(defmethod ag-proto:serialize-to-bytes ((obj proto-location-info))
  (let ((buffer (make-array 64 :element-type '(unsigned-byte 8)
                               :fill-pointer 0 :adjustable t)))
    (let ((value (slot-value obj 'name)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 1 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    (let ((value (slot-value obj 'address)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 2 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    (let ((value (slot-value obj 'phone)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 3 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    (let ((value (slot-value obj 'map-url)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 4 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    (let ((value (slot-value obj 'website)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 5 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    buffer))

(defmethod ag-proto:deserialize-from-bytes ((type (eql 'proto-location-info)) data)
  (let ((obj (make-instance 'proto-location-info))
        (buffer (cons data 0)))
    (loop while (< (cdr buffer) (length data))
          do (let* ((tag (ag-proto::read-varint buffer))
                    (field-number (ash tag -3)))
               (case field-number
                 (1 (setf (slot-value obj 'name)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (2 (setf (slot-value obj 'address)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (3 (setf (slot-value obj 'phone)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (4 (setf (slot-value obj 'map-url)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (5 (setf (slot-value obj 'website)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (otherwise (ag-proto::skip-field buffer (logand tag 7))))))
    obj))

;;; ----------------------------------------------------------------------------

(defclass proto-todo-data (ag-proto:proto-message)
  ((id :initarg :id :accessor proto-todo-id :initform "" :type string)
   (title :initarg :title :accessor proto-todo-title :initform "" :type string)
   (description :initarg :description :accessor proto-todo-description :initform "" :type string)
   (priority :initarg :priority :accessor proto-todo-priority :initform "" :type string)
   (status :initarg :status :accessor proto-todo-status :initform "" :type string)
   (scheduled-date :initarg :scheduled-date :accessor proto-todo-scheduled-date :initform "" :type string)
   (due-date :initarg :due-date :accessor proto-todo-due-date :initform "" :type string)
   (tags :initarg :tags :accessor proto-todo-tags :initform nil :type list)
   (estimated-minutes :initarg :estimated-minutes :accessor proto-todo-estimated-minutes
                      :initform 0 :type (signed-byte 32))
   (location-info :initarg :location-info :accessor proto-todo-location-info :initform nil :type t)
   (url :initarg :url :accessor proto-todo-url :initform "" :type string)
   (created-at :initarg :created-at :accessor proto-todo-created-at :initform "" :type string)
   (completed-at :initarg :completed-at :accessor proto-todo-completed-at :initform "" :type string)
   (parent-id :initarg :parent-id :accessor proto-todo-parent-id :initform "" :type string)
   (repeat-interval :initarg :repeat-interval :accessor proto-todo-repeat-interval
                    :initform 0 :type (signed-byte 32))
   (repeat-unit :initarg :repeat-unit :accessor proto-todo-repeat-unit :initform "" :type string))
  (:documentation "Proto message: TodoData"))

(defmethod ag-proto:serialize-to-bytes ((obj proto-todo-data))
  (let ((buffer (make-array 64 :element-type '(unsigned-byte 8)
                               :fill-pointer 0 :adjustable t)))
    (let ((value (slot-value obj 'id)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 1 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    (let ((value (slot-value obj 'title)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 2 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    (let ((value (slot-value obj 'description)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 3 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    (let ((value (slot-value obj 'priority)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 4 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    (let ((value (slot-value obj 'status)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 5 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    (let ((value (slot-value obj 'scheduled-date)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 6 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    (let ((value (slot-value obj 'due-date)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 7 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    (dolist (elem (slot-value obj 'tags))
      (ag-proto::write-field-tag 8 2 buffer)
      (ag-proto::write-length-delimited (ag-proto::string-to-utf8 elem) buffer))
    (let ((value (slot-value obj 'estimated-minutes)))
      (when (and value (not (eql value 0)))
        (ag-proto::write-field-tag 9 0 buffer)
        (ag-proto::write-varint value buffer)))
    (let ((value (slot-value obj 'location-info)))
      (when value
        (ag-proto::write-field-tag 10 2 buffer)
        (ag-proto::write-length-delimited (ag-proto:serialize-to-bytes value) buffer)))
    (let ((value (slot-value obj 'url)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 11 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    (let ((value (slot-value obj 'created-at)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 12 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    (let ((value (slot-value obj 'completed-at)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 13 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    (let ((value (slot-value obj 'parent-id)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 14 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    (let ((value (slot-value obj 'repeat-interval)))
      (when (and value (not (eql value 0)))
        (ag-proto::write-field-tag 15 0 buffer)
        (ag-proto::write-varint value buffer)))
    (let ((value (slot-value obj 'repeat-unit)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 16 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    buffer))

(defmethod ag-proto:deserialize-from-bytes ((type (eql 'proto-todo-data)) data)
  (let ((obj (make-instance 'proto-todo-data))
        (buffer (cons data 0)))
    (loop while (< (cdr buffer) (length data))
          do (let* ((tag (ag-proto::read-varint buffer))
                    (field-number (ash tag -3))
                    (wire-type (logand tag 7)))
               (case field-number
                 (1 (setf (slot-value obj 'id)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (2 (setf (slot-value obj 'title)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (3 (setf (slot-value obj 'description)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (4 (setf (slot-value obj 'priority)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (5 (setf (slot-value obj 'status)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (6 (setf (slot-value obj 'scheduled-date)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (7 (setf (slot-value obj 'due-date)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (8 (push (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))
                          (slot-value obj 'tags)))
                 (9 (setf (slot-value obj 'estimated-minutes)
                          (let ((n (logand (ag-proto::read-varint buffer) 4294967295)))
                            (if (>= n 2147483648) (- n 4294967296) n))))
                 (10 (setf (slot-value obj 'location-info)
                           (let ((data (ag-proto::read-length-delimited buffer)))
                             (ag-proto:deserialize-from-bytes 'proto-location-info data))))
                 (11 (setf (slot-value obj 'url)
                           (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (12 (setf (slot-value obj 'created-at)
                           (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (13 (setf (slot-value obj 'completed-at)
                           (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (14 (setf (slot-value obj 'parent-id)
                           (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (15 (setf (slot-value obj 'repeat-interval)
                           (let ((n (logand (ag-proto::read-varint buffer) 4294967295)))
                             (if (>= n 2147483648) (- n 4294967296) n))))
                 (16 (setf (slot-value obj 'repeat-unit)
                           (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (otherwise (ag-proto::skip-field buffer wire-type)))))
    (setf (slot-value obj 'tags) (nreverse (slot-value obj 'tags)))
    obj))

;;; ----------------------------------------------------------------------------

(defclass proto-todo-change (ag-proto:proto-message)
  ((device-id :initarg :device-id :accessor proto-change-device-id :initform "" :type string)
   (timestamp :initarg :timestamp :accessor proto-change-timestamp :initform "" :type string)
   (upsert :initarg :upsert :accessor proto-change-upsert :initform nil :type t)
   (delete-id :initarg :delete-id :accessor proto-change-delete-id :initform "" :type string)
   (change-case :initform nil :accessor proto-change-case
                :documentation "Which change variant is set (:upsert or :delete-id)"))
  (:documentation "Proto message: TodoChange"))

(defmethod ag-proto:serialize-to-bytes ((obj proto-todo-change))
  (let ((buffer (make-array 64 :element-type '(unsigned-byte 8)
                               :fill-pointer 0 :adjustable t)))
    (let ((value (slot-value obj 'device-id)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 1 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    (let ((value (slot-value obj 'timestamp)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 2 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    (when (eq (slot-value obj 'change-case) :upsert)
      (let ((value (slot-value obj 'upsert)))
        (when value
          (ag-proto::write-field-tag 3 2 buffer)
          (ag-proto::write-length-delimited (ag-proto:serialize-to-bytes value) buffer))))
    (when (eq (slot-value obj 'change-case) :delete-id)
      (let ((value (slot-value obj 'delete-id)))
        (when (and value (plusp (length value)))
          (ag-proto::write-field-tag 4 2 buffer)
          (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer))))
    buffer))

(defmethod ag-proto:deserialize-from-bytes ((type (eql 'proto-todo-change)) data)
  (let ((obj (make-instance 'proto-todo-change))
        (buffer (cons data 0)))
    (loop while (< (cdr buffer) (length data))
          do (let* ((tag (ag-proto::read-varint buffer))
                    (field-number (ash tag -3))
                    (wire-type (logand tag 7)))
               (case field-number
                 (1 (setf (slot-value obj 'device-id)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (2 (setf (slot-value obj 'timestamp)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (3 (progn
                      (setf (slot-value obj 'delete-id) "")
                      (setf (slot-value obj 'upsert)
                            (let ((data (ag-proto::read-length-delimited buffer)))
                              (ag-proto:deserialize-from-bytes 'proto-todo-data data)))
                      (setf (slot-value obj 'change-case) :upsert)))
                 (4 (progn
                      (setf (slot-value obj 'upsert) nil)
                      (setf (slot-value obj 'delete-id)
                            (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer)))
                      (setf (slot-value obj 'change-case) :delete-id)))
                 (otherwise (ag-proto::skip-field buffer wire-type)))))
    obj))

;;; ----------------------------------------------------------------------------

(defclass proto-sync-init (ag-proto:proto-message)
  ((device-id :initarg :device-id :accessor proto-init-device-id :initform "" :type string)
   (since :initarg :since :accessor proto-init-since :initform "" :type string)
   (client-time :initarg :client-time :accessor proto-init-client-time :initform "" :type string))
  (:documentation "Proto message: SyncInit"))

(defmethod ag-proto:serialize-to-bytes ((obj proto-sync-init))
  (let ((buffer (make-array 64 :element-type '(unsigned-byte 8)
                               :fill-pointer 0 :adjustable t)))
    (let ((value (slot-value obj 'device-id)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 1 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    (let ((value (slot-value obj 'since)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 2 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    (let ((value (slot-value obj 'client-time)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 3 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    buffer))

(defmethod ag-proto:deserialize-from-bytes ((type (eql 'proto-sync-init)) data)
  (let ((obj (make-instance 'proto-sync-init))
        (buffer (cons data 0)))
    (loop while (< (cdr buffer) (length data))
          do (let* ((tag (ag-proto::read-varint buffer))
                    (field-number (ash tag -3))
                    (wire-type (logand tag 7)))
               (case field-number
                 (1 (setf (slot-value obj 'device-id)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (2 (setf (slot-value obj 'since)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (3 (setf (slot-value obj 'client-time)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (otherwise (ag-proto::skip-field buffer wire-type)))))
    obj))

;;; ----------------------------------------------------------------------------

(defclass proto-sync-ack (ag-proto:proto-message)
  ((server-time :initarg :server-time :accessor proto-ack-server-time :initform "" :type string)
   (pending-changes :initarg :pending-changes :accessor proto-ack-pending-changes
                    :initform 0 :type (signed-byte 32))
   (error :initarg :error :accessor proto-ack-error :initform "" :type string))
  (:documentation "Proto message: SyncAck"))

(defmethod ag-proto:serialize-to-bytes ((obj proto-sync-ack))
  (let ((buffer (make-array 64 :element-type '(unsigned-byte 8)
                               :fill-pointer 0 :adjustable t)))
    (let ((value (slot-value obj 'server-time)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 1 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    (let ((value (slot-value obj 'pending-changes)))
      (when (and value (not (eql value 0)))
        (ag-proto::write-field-tag 2 0 buffer)
        (ag-proto::write-varint value buffer)))
    (let ((value (slot-value obj 'error)))
      (when (and value (plusp (length value)))
        (ag-proto::write-field-tag 3 2 buffer)
        (ag-proto::write-length-delimited (ag-proto::string-to-utf8 value) buffer)))
    buffer))

(defmethod ag-proto:deserialize-from-bytes ((type (eql 'proto-sync-ack)) data)
  (let ((obj (make-instance 'proto-sync-ack))
        (buffer (cons data 0)))
    (loop while (< (cdr buffer) (length data))
          do (let* ((tag (ag-proto::read-varint buffer))
                    (field-number (ash tag -3))
                    (wire-type (logand tag 7)))
               (case field-number
                 (1 (setf (slot-value obj 'server-time)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (2 (setf (slot-value obj 'pending-changes)
                          (let ((n (logand (ag-proto::read-varint buffer) 4294967295)))
                            (if (>= n 2147483648) (- n 4294967296) n))))
                 (3 (setf (slot-value obj 'error)
                          (ag-proto::utf8-to-string (ag-proto::read-length-delimited buffer))))
                 (otherwise (ag-proto::skip-field buffer wire-type)))))
    obj))

;;; ----------------------------------------------------------------------------

(defclass proto-sync-message (ag-proto:proto-message)
  ((init :initarg :init :accessor proto-msg-init :initform nil :type t)
   (ack :initarg :ack :accessor proto-msg-ack :initform nil :type t)
   (change :initarg :change :accessor proto-msg-change :initform nil :type t)
   (msg-case :initform nil :accessor proto-msg-case
             :documentation "Which msg variant is set (:init, :ack, or :change)"))
  (:documentation "Proto message: SyncMessage"))

(defmethod ag-proto:serialize-to-bytes ((obj proto-sync-message))
  (let ((buffer (make-array 64 :element-type '(unsigned-byte 8)
                               :fill-pointer 0 :adjustable t)))
    (when (eq (slot-value obj 'msg-case) :init)
      (let ((value (slot-value obj 'init)))
        (when value
          (ag-proto::write-field-tag 1 2 buffer)
          (ag-proto::write-length-delimited (ag-proto:serialize-to-bytes value) buffer))))
    (when (eq (slot-value obj 'msg-case) :ack)
      (let ((value (slot-value obj 'ack)))
        (when value
          (ag-proto::write-field-tag 2 2 buffer)
          (ag-proto::write-length-delimited (ag-proto:serialize-to-bytes value) buffer))))
    (when (eq (slot-value obj 'msg-case) :change)
      (let ((value (slot-value obj 'change)))
        (when value
          (ag-proto::write-field-tag 3 2 buffer)
          (ag-proto::write-length-delimited (ag-proto:serialize-to-bytes value) buffer))))
    buffer))

(defmethod ag-proto:deserialize-from-bytes ((type (eql 'proto-sync-message)) data)
  (let ((obj (make-instance 'proto-sync-message))
        (buffer (cons data 0)))
    (loop while (< (cdr buffer) (length data))
          do (let* ((tag (ag-proto::read-varint buffer))
                    (field-number (ash tag -3))
                    (wire-type (logand tag 7)))
               (case field-number
                 (1 (progn
                      (setf (slot-value obj 'ack) nil)
                      (setf (slot-value obj 'change) nil)
                      (setf (slot-value obj 'init)
                            (let ((data (ag-proto::read-length-delimited buffer)))
                              (ag-proto:deserialize-from-bytes 'proto-sync-init data)))
                      (setf (slot-value obj 'msg-case) :init)))
                 (2 (progn
                      (setf (slot-value obj 'init) nil)
                      (setf (slot-value obj 'change) nil)
                      (setf (slot-value obj 'ack)
                            (let ((data (ag-proto::read-length-delimited buffer)))
                              (ag-proto:deserialize-from-bytes 'proto-sync-ack data)))
                      (setf (slot-value obj 'msg-case) :ack)))
                 (3 (progn
                      (setf (slot-value obj 'init) nil)
                      (setf (slot-value obj 'ack) nil)
                      (setf (slot-value obj 'change)
                            (let ((data (ag-proto::read-length-delimited buffer)))
                              (ag-proto:deserialize-from-bytes 'proto-todo-change data)))
                      (setf (slot-value obj 'msg-case) :change)))
                 (otherwise (ag-proto::skip-field buffer wire-type)))))
    obj))

;;; ============================================================================
;;; Client Stub (for reference, server-side mainly uses service impl)
;;; ============================================================================

(defclass proto-todo-sync-stub ()
  ((channel :initarg :channel :accessor proto-stub-channel
            :documentation "gRPC channel for this stub"))
  (:documentation "Client stub for TodoSync service"))

(defun make-proto-todo-sync-stub (channel)
  "Create a new TodoSync client stub"
  (make-instance 'proto-todo-sync-stub :channel channel))

(defmethod proto-todo-sync-stream ((stub proto-todo-sync-stub)
                                   &key metadata timeout)
  "Initiate TodoSync.SyncStream bidirectional streaming RPC.
Returns a grpc-bidi-stream. Use stream-send to send messages,
stream-read-message to receive messages, and stream-close-send when done."
  (let* ((grpc-pkg (find-package :ag-grpc))
         (call-fn (and grpc-pkg
                       (symbol-function
                        (find-symbol "CALL-BIDIRECTIONAL-STREAMING" grpc-pkg)))))
    (unless call-fn
      (error "ag-grpc package not loaded. Load ag-grpc before calling RPC methods."))
    (funcall call-fn (proto-stub-channel stub)
             "/cloodoo.TodoSync/SyncStream"
             :response-type 'proto-sync-message
             :metadata metadata
             :timeout timeout)))

;;; ============================================================================
;;; Conversion Functions: todo <-> proto-todo-data
;;; ============================================================================

(defun timestamp-to-iso8601 (ts)
  "Convert a local-time timestamp to ISO 8601 string, or empty string if nil."
  (if ts
      (lt:format-timestring nil ts :format lt:+iso-8601-format+)
      ""))

(defun iso8601-to-timestamp (str)
  "Parse an ISO 8601 string to local-time timestamp, or nil if empty/invalid."
  (when (and str (plusp (length str)))
    (handler-case
        (lt:parse-timestring str)
      (error () nil))))

(defun keyword-to-string (kw)
  "Convert keyword to lowercase string, or empty string if nil."
  (if kw
      (string-downcase (symbol-name kw))
      ""))

(defun string-to-keyword (str)
  "Convert string to keyword, or nil if empty."
  (when (and str (plusp (length str)))
    (intern (string-upcase str) :keyword)))

(defun todo-to-proto (todo)
  "Convert a todo instance to proto-todo-data."
  (let ((loc-info (todo-location-info todo)))
    (make-instance 'proto-todo-data
                   :id (todo-id todo)
                   :title (todo-title todo)
                   :description (or (todo-description todo) "")
                   :priority (keyword-to-string (todo-priority todo))
                   :status (keyword-to-string (todo-status todo))
                   :scheduled-date (timestamp-to-iso8601 (todo-scheduled-date todo))
                   :due-date (timestamp-to-iso8601 (todo-due-date todo))
                   :tags (or (todo-tags todo) nil)
                   :estimated-minutes (or (todo-estimated-minutes todo) 0)
                   :location-info (when loc-info
                                    (make-instance 'proto-location-info
                                                   :name (or (getf loc-info :name) "")
                                                   :address (or (getf loc-info :address) "")
                                                   :phone (or (getf loc-info :phone) "")
                                                   :map-url (or (getf loc-info :map-url) "")
                                                   :website (or (getf loc-info :website) "")))
                   :url (or (todo-url todo) "")
                   :created-at (timestamp-to-iso8601 (todo-created-at todo))
                   :completed-at (timestamp-to-iso8601 (todo-completed-at todo))
                   :parent-id (or (todo-parent-id todo) "")
                   :repeat-interval (or (todo-repeat-interval todo) 0)
                   :repeat-unit (keyword-to-string (todo-repeat-unit todo)))))

(defun proto-to-todo (proto)
  "Convert a proto-todo-data to a todo instance."
  (let ((loc-proto (proto-todo-location-info proto)))
    (make-instance 'todo
                   :id (proto-todo-id proto)
                   :title (proto-todo-title proto)
                   :description (let ((d (proto-todo-description proto)))
                                  (if (plusp (length d)) d nil))
                   :priority (or (string-to-keyword (proto-todo-priority proto)) +priority-medium+)
                   :status (or (string-to-keyword (proto-todo-status proto)) +status-pending+)
                   :scheduled-date (iso8601-to-timestamp (proto-todo-scheduled-date proto))
                   :due-date (iso8601-to-timestamp (proto-todo-due-date proto))
                   :tags (proto-todo-tags proto)
                   :estimated-minutes (let ((m (proto-todo-estimated-minutes proto)))
                                        (if (zerop m) nil m))
                   :location-info (when loc-proto
                                    (list :name (proto-location-name loc-proto)
                                          :address (proto-location-address loc-proto)
                                          :phone (proto-location-phone loc-proto)
                                          :map-url (proto-location-map-url loc-proto)
                                          :website (proto-location-website loc-proto)))
                   :url (let ((u (proto-todo-url proto)))
                          (if (plusp (length u)) u nil))
                   :created-at (or (iso8601-to-timestamp (proto-todo-created-at proto))
                                   (lt:now))
                   :completed-at (iso8601-to-timestamp (proto-todo-completed-at proto))
                   :parent-id (let ((p (proto-todo-parent-id proto)))
                                (if (plusp (length p)) p nil))
                   :repeat-interval (let ((i (proto-todo-repeat-interval proto)))
                                      (if (zerop i) nil i))
                   :repeat-unit (string-to-keyword (proto-todo-repeat-unit proto)))))

;;; ============================================================================
;;; Helper constructors for sync messages
;;; ============================================================================

(defun make-sync-init-message (device-id &optional since client-time)
  "Create a SyncMessage containing a SyncInit."
  (let ((msg (make-instance 'proto-sync-message)))
    (setf (proto-msg-init msg)
          (make-instance 'proto-sync-init
                         :device-id device-id
                         :since (or since "")
                         :client-time (or client-time "")))
    (setf (proto-msg-case msg) :init)
    msg))

(defun make-sync-ack-message (server-time pending-changes &optional error-message)
  "Create a SyncMessage containing a SyncAck.
   If ERROR-MESSAGE is provided, the connection is being rejected."
  (let ((msg (make-instance 'proto-sync-message)))
    (setf (proto-msg-ack msg)
          (make-instance 'proto-sync-ack
                         :server-time server-time
                         :pending-changes pending-changes
                         :error (or error-message "")))
    (setf (proto-msg-case msg) :ack)
    msg))

(defun make-sync-upsert-message (device-id todo)
  "Create a SyncMessage containing a TodoChange with upsert.
   Uses current time as timestamp - suitable for real-time changes."
  (let ((msg (make-instance 'proto-sync-message))
        (change (make-instance 'proto-todo-change
                               :device-id device-id
                               :timestamp (timestamp-to-iso8601 (lt:now)))))
    (setf (proto-change-upsert change) (todo-to-proto todo))
    (setf (proto-change-case change) :upsert)
    (setf (proto-msg-change msg) change)
    (setf (proto-msg-case msg) :change)
    msg))

(defun make-sync-upsert-message-with-timestamp (device-id todo timestamp)
  "Create a SyncMessage containing a TodoChange with upsert using a specific timestamp.
   TIMESTAMP should be an ISO8601 string. Used for syncing historical data with correct timestamps."
  (let ((msg (make-instance 'proto-sync-message))
        (change (make-instance 'proto-todo-change
                               :device-id device-id
                               :timestamp timestamp)))
    (setf (proto-change-upsert change) (todo-to-proto todo))
    (setf (proto-change-case change) :upsert)
    (setf (proto-msg-change msg) change)
    (setf (proto-msg-case msg) :change)
    msg))

(defun make-sync-delete-message (device-id todo-id)
  "Create a SyncMessage containing a TodoChange with delete."
  (let ((msg (make-instance 'proto-sync-message))
        (change (make-instance 'proto-todo-change
                               :device-id device-id
                               :timestamp (timestamp-to-iso8601 (lt:now)))))
    (setf (proto-change-delete-id change) todo-id)
    (setf (proto-change-case change) :delete-id)
    (setf (proto-msg-change msg) change)
    (setf (proto-msg-case msg) :change)
    msg))
