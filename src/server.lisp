;;; server.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cloodoo)

;;── HTTP Server for Certificate Pairing ───────────────────────────────────────

(defvar *server* nil
  "The Hunchentoot acceptor instance.")

(defvar *default-port* 9876
  "Default port for the pairing server.")

(defvar *default-address* "127.0.0.1"
  "Default bind address for the pairing server.")

;;── Passphrase-based Encryption ──────────────────────────────────────────────

(defun derive-key-from-passphrase (passphrase salt)
  "Derive a 256-bit AES key from PASSPHRASE and SALT using PBKDF2-SHA256.
   SALT should be a byte vector (16 bytes recommended).
   Returns a 32-byte key as a byte vector."
  (let ((passphrase-bytes (flexi-streams:string-to-octets passphrase :external-format :utf-8)))
    (ironclad:derive-key
     (ironclad:make-kdf 'ironclad:pbkdf2 :digest 'ironclad:sha256)
     passphrase-bytes
     salt
     100000  ; iterations
     32)))   ; key length (256 bits)

(defun encrypt-with-passphrase (plaintext passphrase)
  "Encrypt PLAINTEXT string using PASSPHRASE.
   Returns a hash table with :encrypted, :iv, and :salt (all base64 encoded)."
  (let* ((salt (ironclad:random-data 16))
         (iv (ironclad:random-data 12))  ; 96-bit IV for GCM
         (key (derive-key-from-passphrase passphrase salt))
         (plaintext-bytes (flexi-streams:string-to-octets plaintext :external-format :utf-8))
         (aad (ironclad:ascii-string-to-byte-array "cloodoo-pairing"))
         (cipher (ironclad:make-authenticated-encryption-mode
                  :gcm :cipher-name 'ironclad:aes :key key :initialization-vector iv))
         (ciphertext (ironclad:encrypt-message cipher plaintext-bytes
                                               :associated-data aad))
         (tag (make-array 16 :element-type '(unsigned-byte 8))))
    ;; Get authentication tag
    (ironclad:produce-tag cipher :tag tag)
    ;; Return base64-encoded components (append tag to ciphertext)
    (let ((result (make-hash-table :test #'equal))
          (ciphertext-with-tag (concatenate '(vector (unsigned-byte 8)) ciphertext tag)))
      (setf (gethash "encrypted" result) (base64-encode ciphertext-with-tag))
      (setf (gethash "iv" result) (base64-encode iv))
      (setf (gethash "salt" result) (base64-encode salt))
      result)))

(defun bytes-to-base64 (bytes)
  "Convert byte vector to base64 string."
  (cl-base64:usb8-array-to-base64-string bytes))

(defun base64-encode (bytes)
  "Encode byte vector as base64 string."
  (cl-base64:usb8-array-to-base64-string bytes))

(defun base64-decode (string)
  "Decode a base64 string to a byte vector."
  (cl-base64:base64-string-to-usb8-array string))

(defun decrypt-with-passphrase (encrypted-data passphrase)
  "Decrypt data encrypted by ENCRYPT-WITH-PASSPHRASE.
   ENCRYPTED-DATA is a hash table with keys \"encrypted\", \"iv\", \"salt\"
   (all base64 encoded).  Returns the plaintext string."
  (let* ((ciphertext-with-tag (base64-decode (gethash "encrypted" encrypted-data)))
         (iv (base64-decode (gethash "iv" encrypted-data)))
         (salt (base64-decode (gethash "salt" encrypted-data)))
         (key (derive-key-from-passphrase passphrase salt))
         (aad (ironclad:ascii-string-to-byte-array "cloodoo-pairing"))
         ;; Last 16 bytes are the GCM authentication tag
         (tag-offset (- (length ciphertext-with-tag) 16))
         (ciphertext (subseq ciphertext-with-tag 0 tag-offset))
         (tag (subseq ciphertext-with-tag tag-offset))
         (cipher (ironclad:make-authenticated-encryption-mode
                  :gcm :cipher-name 'ironclad:aes :key key :initialization-vector iv)))
    (let ((plaintext-bytes (ironclad:decrypt-message cipher ciphertext
                                                     :associated-data aad))
          (computed-tag (make-array 16 :element-type '(unsigned-byte 8))))
      (ironclad:produce-tag cipher :tag computed-tag)
      (unless (ironclad:constant-time-equal computed-tag tag)
        (error "Decryption failed: authentication tag mismatch"))
      (flexi-streams:octets-to-string plaintext-bytes :external-format :utf-8))))

;;── Pairing API Routes ─────────────────────────────────────────────────────────

(easy-routes:defroute api-pair-info ("/pair/:token" :method :get) ()
  "Return pairing info (device name, expiry) for a token.
   Does NOT consume the token - use POST to download the certificate."
  (setf (hunchentoot:content-type*) "application/json")
  (let ((request (get-pairing-request token))
        (response (make-hash-table :test #'equal)))
    (cond (request
          (setf (gethash "device_name" response) (pairing-request-device-name request))
          (setf (gethash "expires_in" response)
                (- (pairing-request-expires-at request) (get-universal-time)))
          (jzon:stringify response))
      (t
          (setf (hunchentoot:return-code*) 404)
          (setf (gethash "error" response) "Invalid or expired pairing token")
          (jzon:stringify response)))))

(easy-routes:defroute api-pair-download-pem ("/pair/:token/pem" :method :post) ()
  "Download the certificate and key as encrypted payload for a pairing token.
   Requires passphrase in request body: {passphrase: string}.
   This consumes the token (single-use).
   Returns JSON: {encrypted: base64, iv: base64, salt: base64}.
   The encrypted payload contains: {cert: PEM, key: PEM, ca_cert: PEM, device_name: string}."
  (setf (hunchentoot:content-type*) "application/json")
  (let ((request (get-pairing-request token)))
    (cond (request (let* ((body (hunchentoot:raw-post-data :force-text t))
               (data (when (plusp (length body)) (jzon:parse body)))
               (provided-passphrase (when data (gethash "passphrase" data)))
               (expected-passphrase (pairing-request-passphrase request)))
          (cond ((and provided-passphrase
                   (string= provided-passphrase expected-passphrase))
                (consume-pairing-request token)
                (let ((device-name (pairing-request-device-name request))
                      (payload (make-hash-table :test #'equal)))
                  (let ((cert-path (client-cert-file device-name))
                        (key-path (client-key-file device-name))
                        (ca-path (ca-cert-file)))
                    (cond ((and (probe-file cert-path) (probe-file key-path))
                           ;; Build the payload to encrypt
                           (setf (gethash "cert" payload)
                                 (uiop:read-file-string cert-path))
                           (setf (gethash "key" payload)
                                 (uiop:read-file-string key-path))
                           (when (probe-file ca-path)
                             (setf (gethash "ca_cert" payload)
                                   (uiop:read-file-string ca-path)))
                           (setf (gethash "device_name" payload) device-name)
                           ;; Encrypt the payload with the passphrase
                           (let ((encrypted-response
                                   (encrypt-with-passphrase
                                    (jzon:stringify payload)
                                    provided-passphrase)))
                             (jzon:stringify encrypted-response)))
                          (t
                           (setf (hunchentoot:return-code*) 500)
                           (jzon:stringify (alexandria:plist-hash-table
                                            '("error" "Certificate files not found")
                                            :test #'equal)))))))
                (t
                 (setf (hunchentoot:return-code*) 403)
                 (jzon:stringify (alexandria:plist-hash-table
                                  '("error" "Invalid passphrase")
                                  :test #'equal))))))
          (t
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


;;── Server Control ─────────────────────────────────────────────────────────────

(defun start-server (&key (port *default-port*) (address *default-address*) hostname email)
  "Start the HTTP server for certificate pairing.
   Use address \"0.0.0.0\" to listen on all interfaces.
   When HOSTNAME is provided, start an HTTPS server on PORT (default 443)
   using Let's Encrypt via pure-tls ACME.  EMAIL is the contact address
   for Let's Encrypt (defaults to \"admin@HOSTNAME\")."
  (when *server*
    (stop-server))
  (if hostname
      (let ((acme-port (if (= port *default-port*) 443 port))
            (contact-email (or email (format nil "admin@~A" hostname))))
        (setf *server* (pure-tls/acme:make-acme-acceptor
                         hostname contact-email
                         :port acme-port
                         :production t
                         :acceptor-class 'easy-routes:easy-routes-acceptor))
        (hunchentoot:start *server*)
        (format t "~&Cloodoo API server running on https://~A:~A~%" hostname acme-port))
      (progn
        (setf *server* (make-instance 'easy-routes:easy-routes-acceptor
                                      :port port
                                      :address address))
        (hunchentoot:start *server*)
        (format t "~&Cloodoo API server running on http://~A:~A~%" address port)))
  (format t "~&Device ID: ~A~%" (get-device-id))
  *server*)

(defun stop-server ()
  "Stop the API server."
  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil)
    (format t "~&Cloodoo API server stopped.~%")))
