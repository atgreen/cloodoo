;;; server.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cloodoo)

;;── HTTP Server for Certificate Pairing ───────────────────────────────────────

(defvar *server* nil
  "The Hunchentoot acceptor instance.")

(defclass acme-routes-acceptor (pure-tls/acme:acme-acceptor
                                easy-routes:easy-routes-acceptor)
  ()
  (:documentation "ACME acceptor with easy-routes dispatch."))

(defvar *default-port* 9876
  "Default port for the pairing server.")

(defvar *default-address* "127.0.0.1"
  "Default bind address for the pairing server.")

(defvar *server-hostname* nil
  "The public hostname of this server, set when running with --hostname.
   Used for constructing pairing URLs.")

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


;;── HTML Page Rendering ───────────────────────────────────────────────────────

(defun pairing-url-for-token (token)
  "Construct the full pairing URL for TOKEN using *server-hostname*."
  (if *server-hostname*
      (format nil "https://~A/pair/~A" *server-hostname* token)
      (format nil "http://localhost:~A/pair/~A" *default-port* token)))

(defun generate-qr-svg (data &key (module-size 4))
  "Generate an SVG QR code for DATA.  Returns an SVG string."
  (handler-case
      (let* ((qr (cl-qrencode:encode-symbol data :level :level-m))
             (size (cl-qrencode:modules qr))
             (mat (cl-qrencode:matrix qr))
             (margin 4)
             (total (+ size (* 2 margin)))
             (px (* total module-size))
             (out (make-string-output-stream)))
        (format out "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 ~D ~D' width='~D' height='~D'>"
                total total px px)
        (format out "<rect width='~D' height='~D' fill='white'/>" total total)
        (loop for y from 0 below size
              do (loop for x from 0 below size
                       when (cl-qrencode:dark-module-p mat y x)
                       do (format out "<rect x='~D' y='~D' width='1' height='1' fill='black'/>"
                                  (+ margin x) (+ margin y))))
        (format out "</svg>")
        (get-output-stream-string out))
    (error () nil)))

(defun html-page (title body-html)
  "Wrap BODY-HTML in a minimal HTML page with TITLE."
  (format nil "<!DOCTYPE html>
<html><head>
<meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'>
<title>~A</title>
<style>
body{font-family:system-ui,sans-serif;max-width:600px;margin:2em auto;padding:0 1em;color:#1a1a1a;background:#fafafa}
h1{color:#333}
.qr{text-align:center;margin:1.5em 0}
.qr svg{max-width:256px;border:8px solid white;box-shadow:0 2px 8px rgba(0,0,0,0.1)}
.passphrase{font-family:monospace;font-size:1.3em;background:#e8f5e9;padding:0.8em;border-radius:6px;text-align:center;letter-spacing:0.05em}
.recovery-codes{background:#fff3e0;padding:1em;border-radius:6px;margin:1em 0}
.recovery-codes code{display:block;font-size:1.1em;margin:0.3em 0}
.warning{color:#e65100;font-weight:bold}
pre{background:#f5f5f5;padding:0.8em;border-radius:4px;overflow-x:auto}
.error{color:#c62828;background:#ffebee;padding:1em;border-radius:6px}
input[type=text]{font-family:monospace;font-size:1.1em;padding:0.5em;width:100%%;box-sizing:border-box}
button{background:#1565c0;color:white;border:none;padding:0.7em 1.5em;font-size:1em;border-radius:4px;cursor:pointer}
button:hover{background:#0d47a1}
</style>
</head><body><h1>~A</h1>~A</body></html>" title title body-html))

(defun html-pairing-section (pairing-request)
  "Render the QR code + passphrase section for a PAIRING-REQUEST."
  (let* ((token (pairing-request-token pairing-request))
         (passphrase (pairing-request-passphrase pairing-request))
         (url (pairing-url-for-token token))
         (qr-svg (generate-qr-svg url)))
    (format nil "
<div class='qr'>~A</div>
<p>Scan this QR code with the Cloodoo app, or use the CLI:</p>
<pre>cloodoo cert pair ~A</pre>
<p>Passphrase:</p>
<div class='passphrase'>~A</div>
<p>This link expires in 10 minutes.</p>"
            (or qr-svg "<p>[QR code generation failed]</p>")
            url passphrase)))

;;── Self-Service Registration Routes ─────────────────────────────────────────

(easy-routes:defroute register-page ("/register" :method :get) (invite)
  "Self-service registration page gated by invite code."
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (cond
    ;; No invite code provided
    ((or (null invite) (zerop (length invite)))
     (html-page "Cloodoo" "<p class='error'>An invite code is required to register.</p>"))
    ;; Invalid or expired invite code
    ((null (db-validate-invite-code invite))
     (html-page "Cloodoo" "<p class='error'>Invalid or expired invite code.</p>"))
    ;; Valid invite — perform registration
    (t
     (handler-case
         (let* ((user-id (generate-device-id))
                (pairing-req (create-pairing-request user-id))
                (recovery-codes (generate-recovery-codes 8))
                (code-hashes (mapcar #'hash-recovery-code recovery-codes)))
           ;; Persist
           (db-consume-invite-code invite)
           (db-create-user user-id :created-via (format nil "invite:~A" invite))
           (db-store-recovery-codes user-id code-hashes)
           ;; Render
           (html-page "Welcome to Cloodoo"
                      (format nil "
<p>Your account has been created. Pair your first device now:</p>
~A
<div class='recovery-codes'>
<p class='warning'>Save these recovery codes somewhere safe. You will need them if you lose all your devices.</p>
~{<code>~A</code>~}
</div>"
                              (html-pairing-section pairing-req)
                              recovery-codes)))
       (error (e)
         (html-page "Registration Error"
                    (format nil "<p class='error'>Registration failed: ~A</p>"
                            (hunchentoot:escape-for-html (princ-to-string e)))))))))

;;── Account Recovery Routes ──────────────────────────────────────────────────

(easy-routes:defroute recover-page ("/recover" :method :get) ()
  "Account recovery page — form to enter a recovery code."
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (html-page "Account Recovery"
             "<p>Enter one of your recovery codes to regain access to your account.</p>
<form method='post' action='/recover'>
<p><input type='text' name='code' placeholder='A3B7-K9M2-P4Q8-R1S5' required></p>
<p><button type='submit'>Recover Account</button></p>
</form>"))

(easy-routes:defroute recover-submit ("/recover" :method :post) ()
  "Process a recovery code submission."
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (let* ((body (hunchentoot:post-parameter "code"))
         (code (when body (string-trim '(#\Space #\Tab #\Newline #\Return) body))))
    (cond
      ((or (null code) (zerop (length code)))
       (html-page "Recovery Error"
                  "<p class='error'>No recovery code provided.</p>
                   <p><a href='/recover'>Try again</a></p>"))
      (t
       (let* ((code-hash (hash-recovery-code code))
              (result (db-validate-recovery-code code-hash)))
         (cond
           ((null result)
            (html-page "Recovery Error"
                       "<p class='error'>Invalid or already-used recovery code.</p>
                        <p><a href='/recover'>Try again</a></p>"))
           (t
            (handler-case
                (let* ((user-id (getf result :user-id))
                       (recovery-id (getf result :id)))
                  ;; Consume the code
                  (db-consume-recovery-code recovery-id)
                  ;; Revoke old cert, issue new one
                  (when (client-cert-exists-p user-id)
                    (revoke-client-cert user-id))
                  ;; Remove old cert files so issue-client-cert can create new ones
                  (when (probe-file (client-cert-file user-id))
                    (delete-file (client-cert-file user-id)))
                  (when (probe-file (client-key-file user-id))
                    (delete-file (client-key-file user-id)))
                  ;; Issue new cert and create pairing request
                  (let ((pairing-req (create-pairing-request user-id)))
                    (html-page "Account Recovered"
                               (format nil "
<p>Your account has been recovered. A new certificate has been issued. Pair your device now:</p>
~A
<p class='warning'>The recovery code you used is now consumed.
If you have few remaining codes, ask an admin for new ones.</p>"
                                       (html-pairing-section pairing-req)))))
              (error (e)
                (html-page "Recovery Error"
                           (format nil "<p class='error'>Recovery failed: ~A</p>
                                        <p><a href='/recover'>Try again</a></p>"
                                   (hunchentoot:escape-for-html (princ-to-string e)))))))))))))

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
        (setf *server* (make-instance 'acme-routes-acceptor
                                      :domains (list hostname)
                                      :email contact-email
                                      :port acme-port
                                      :production t))
        (handler-case
            (progn
              (hunchentoot:start *server*)
              (format t "~&Cloodoo API server running on https://~A:~A~%" hostname acme-port))
          (error (e)
            (setf *server* nil)
            (error e))))
      (progn
        (setf *server* (make-instance 'easy-routes:easy-routes-acceptor
                                      :port port
                                      :address address))
        (handler-case
            (progn
              (hunchentoot:start *server*)
              (format t "~&Cloodoo API server running on http://~A:~A~%" address port))
          (error (e)
            (setf *server* nil)
            (error e)))))
  (format t "~&Device ID: ~A~%" (get-device-id))
  *server*)

(defun stop-server ()
  "Stop the API server."
  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil)
    (format t "~&Cloodoo API server stopped.~%")))
