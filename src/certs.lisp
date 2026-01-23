;;; certs.lisp - Certificate management for mTLS sync
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cloodoo)

;;── Certificate Storage Paths ─────────────────────────────────────────────────

(defun certs-directory ()
  "Return the directory for storing certificates."
  (merge-pathnames "certs/" (data-directory)))

(defun ensure-certs-directory ()
  "Ensure the certificates directory exists."
  (ensure-data-directory)
  (let ((dir (certs-directory)))
    (ensure-directories-exist dir)
    dir))

(defun ca-key-file ()
  "Path to the CA private key."
  (merge-pathnames "ca.key" (certs-directory)))

(defun ca-cert-file ()
  "Path to the CA certificate."
  (merge-pathnames "ca.crt" (certs-directory)))

(defun server-key-file ()
  "Path to the server private key."
  (merge-pathnames "server.key" (certs-directory)))

(defun server-cert-file ()
  "Path to the server certificate."
  (merge-pathnames "server.crt" (certs-directory)))

(defun client-key-file (name)
  "Path to a client's private key."
  (merge-pathnames (format nil "clients/~A.key" name) (certs-directory)))

(defun client-cert-file (name)
  "Path to a client's certificate."
  (merge-pathnames (format nil "clients/~A.crt" name) (certs-directory)))

(defun client-p12-file (name)
  "Path to a client's PKCS#12 bundle."
  (merge-pathnames (format nil "clients/~A.p12" name) (certs-directory)))

(defun clients-directory ()
  "Path to the clients subdirectory."
  (merge-pathnames "clients/" (certs-directory)))

(defun revoked-file ()
  "Path to the revoked certificates list."
  (merge-pathnames "revoked.txt" (certs-directory)))

;;── Passphrase Generation ─────────────────────────────────────────────────────

(defvar *passphrase-words*
  '("alpha" "bravo" "charlie" "delta" "echo" "foxtrot" "golf" "hotel"
    "india" "juliet" "kilo" "lima" "mike" "november" "oscar" "papa"
    "quebec" "romeo" "sierra" "tango" "uniform" "victor" "whiskey"
    "xray" "yankee" "zulu" "red" "blue" "green" "yellow" "orange"
    "purple" "black" "white" "silver" "golden" "copper" "iron" "steel"
    "quick" "slow" "bright" "dark" "warm" "cold" "soft" "hard"
    "north" "south" "east" "west" "upper" "lower" "inner" "outer"
    "apple" "banana" "cherry" "date" "elder" "fig" "grape" "hazel"
    "horse" "tiger" "eagle" "shark" "wolf" "bear" "hawk" "lion"
    "river" "mountain" "forest" "desert" "ocean" "island" "valley" "plain")
  "Word list for generating memorable passphrases.")

(defun generate-passphrase (&optional (word-count 3))
  "Generate a memorable passphrase with WORD-COUNT words."
  (let ((words (loop repeat word-count
                     collect (nth (random (length *passphrase-words*))
                                  *passphrase-words*))))
    (format nil "~{~A~^-~}" words)))

;;── OpenSSL Command Helpers ───────────────────────────────────────────────────

(defun run-openssl (&rest args)
  "Run openssl with the given arguments. Returns (values output error-output exit-code)."
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program (cons "openssl" args)
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (values output error-output exit-code)))

(defun openssl-available-p ()
  "Check if openssl command is available."
  (multiple-value-bind (out err code)
      (run-openssl "version")
    (declare (ignore out err))
    (zerop code)))

;;── CA Initialization ─────────────────────────────────────────────────────────

(defun ca-initialized-p ()
  "Check if the CA has been initialized."
  (and (probe-file (ca-key-file))
       (probe-file (ca-cert-file))))

(defun init-ca (&key (days 3650) force)
  "Initialize the Certificate Authority.
   Creates a self-signed CA certificate valid for DAYS days.
   If FORCE is true, overwrites existing CA (WARNING: invalidates all certs)."
  (when (and (ca-initialized-p) (not force))
    (error "CA already initialized. Use :force t to regenerate (invalidates all existing certificates)."))

  (unless (openssl-available-p)
    (error "OpenSSL not found. Please install OpenSSL."))

  (ensure-certs-directory)
  (ensure-directories-exist (clients-directory))

  (let ((ca-key (namestring (ca-key-file)))
        (ca-cert (namestring (ca-cert-file))))

    ;; Generate CA private key
    (multiple-value-bind (out err code)
        (run-openssl "genrsa" "-out" ca-key "4096")
      (declare (ignore out))
      (unless (zerop code)
        (error "Failed to generate CA key: ~A" err)))

    ;; Set restrictive permissions on private key
    (uiop:run-program (list "chmod" "600" ca-key) :ignore-error-status t)

    ;; Generate self-signed CA certificate
    (multiple-value-bind (out err code)
        (run-openssl "req" "-x509" "-new" "-nodes"
                     "-key" ca-key
                     "-sha256"
                     "-days" (format nil "~D" days)
                     "-out" ca-cert
                     "-subj" (format nil "/CN=Cloodoo CA/O=Cloodoo/OU=~A"
                                     (get-device-id)))
      (declare (ignore out))
      (unless (zerop code)
        (error "Failed to generate CA certificate: ~A" err)))

    (format t "CA initialized successfully.~%")
    (format t "CA certificate: ~A~%" ca-cert)
    t))

;;── Server Certificate ────────────────────────────────────────────────────────

(defun server-cert-initialized-p ()
  "Check if the server certificate has been initialized."
  (and (probe-file (server-key-file))
       (probe-file (server-cert-file))))

(defun init-server-cert (&key (days 365) hosts)
  "Initialize the server certificate.
   HOSTS is a list of hostnames/IPs for the SAN extension.
   If not provided, uses localhost, 127.0.0.1, and tries to detect Tailscale IP."
  (unless (ca-initialized-p)
    (error "CA not initialized. Run 'cloodoo cert init' first."))

  (let* ((server-key (namestring (server-key-file)))
         (server-cert (namestring (server-cert-file)))
         (server-csr (namestring (merge-pathnames "server.csr" (certs-directory))))
         (san-file (namestring (merge-pathnames "server-san.cnf" (certs-directory))))
         ;; Build list of SANs
         (all-hosts (or hosts
                        (append '("localhost" "127.0.0.1")
                                (detect-local-ips))))
         ;; Build SAN extension
         (san-entries (loop for host in all-hosts
                            for i from 1
                            collect (if (ip-address-p host)
                                        (format nil "IP.~D = ~A" i host)
                                        (format nil "DNS.~D = ~A" i host)))))

    ;; Generate server private key
    (multiple-value-bind (out err code)
        (run-openssl "genrsa" "-out" server-key "2048")
      (declare (ignore out))
      (unless (zerop code)
        (error "Failed to generate server key: ~A" err)))

    (uiop:run-program (list "chmod" "600" server-key) :ignore-error-status t)

    ;; Create SAN config file
    (with-open-file (stream san-file :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
      (format stream "[req]~%")
      (format stream "distinguished_name = req_distinguished_name~%")
      (format stream "req_extensions = v3_req~%")
      (format stream "prompt = no~%")
      (format stream "[req_distinguished_name]~%")
      (format stream "CN = cloodoo-server~%")
      (format stream "O = Cloodoo~%")
      (format stream "[v3_req]~%")
      (format stream "basicConstraints = CA:FALSE~%")
      (format stream "keyUsage = digitalSignature, keyEncipherment~%")
      (format stream "extendedKeyUsage = serverAuth~%")
      (format stream "subjectAltName = @alt_names~%")
      (format stream "[alt_names]~%")
      (dolist (entry san-entries)
        (format stream "~A~%" entry)))

    ;; Generate CSR
    (multiple-value-bind (out err code)
        (run-openssl "req" "-new"
                     "-key" server-key
                     "-out" server-csr
                     "-config" san-file)
      (declare (ignore out))
      (unless (zerop code)
        (error "Failed to generate server CSR: ~A" err)))

    ;; Sign with CA
    (multiple-value-bind (out err code)
        (run-openssl "x509" "-req"
                     "-in" server-csr
                     "-CA" (namestring (ca-cert-file))
                     "-CAkey" (namestring (ca-key-file))
                     "-CAcreateserial"
                     "-out" server-cert
                     "-days" (format nil "~D" days)
                     "-sha256"
                     "-extensions" "v3_req"
                     "-extfile" san-file)
      (declare (ignore out))
      (unless (zerop code)
        (error "Failed to sign server certificate: ~A" err)))

    ;; Cleanup temp files
    (when (probe-file server-csr) (delete-file server-csr))
    (when (probe-file san-file) (delete-file san-file))

    (format t "Server certificate created.~%")
    (format t "Valid hosts: ~{~A~^, ~}~%" all-hosts)
    t))

;;── Client Certificate ────────────────────────────────────────────────────────

(defun issue-client-cert (name &key (days 365))
  "Issue a client certificate for NAME.
   Returns the passphrase for the .p12 bundle."
  (unless (ca-initialized-p)
    (error "CA not initialized. Run 'cloodoo cert init' first."))

  (when (client-cert-exists-p name)
    (error "Certificate for '~A' already exists. Revoke it first or choose a different name." name))

  (ensure-directories-exist (clients-directory))

  (let* ((client-key (namestring (client-key-file name)))
         (client-cert (namestring (client-cert-file name)))
         (client-csr (namestring (merge-pathnames (format nil "clients/~A.csr" name)
                                                   (certs-directory))))
         (client-p12 (namestring (client-p12-file name)))
         (passphrase (generate-passphrase 3)))

    ;; Generate client private key
    (multiple-value-bind (out err code)
        (run-openssl "genrsa" "-out" client-key "2048")
      (declare (ignore out))
      (unless (zerop code)
        (error "Failed to generate client key: ~A" err)))

    (uiop:run-program (list "chmod" "600" client-key) :ignore-error-status t)

    ;; Generate CSR
    (multiple-value-bind (out err code)
        (run-openssl "req" "-new"
                     "-key" client-key
                     "-out" client-csr
                     "-subj" (format nil "/CN=~A/O=Cloodoo Client" name))
      (declare (ignore out))
      (unless (zerop code)
        (error "Failed to generate client CSR: ~A" err)))

    ;; Sign with CA
    (multiple-value-bind (out err code)
        (run-openssl "x509" "-req"
                     "-in" client-csr
                     "-CA" (namestring (ca-cert-file))
                     "-CAkey" (namestring (ca-key-file))
                     "-CAcreateserial"
                     "-out" client-cert
                     "-days" (format nil "~D" days)
                     "-sha256")
      (declare (ignore out))
      (unless (zerop code)
        (error "Failed to sign client certificate: ~A" err)))

    ;; Create PKCS#12 bundle with CA cert included
    (multiple-value-bind (out err code)
        (run-openssl "pkcs12" "-export"
                     "-out" client-p12
                     "-inkey" client-key
                     "-in" client-cert
                     "-certfile" (namestring (ca-cert-file))
                     "-name" name
                     "-passout" (format nil "pass:~A" passphrase))
      (declare (ignore out))
      (unless (zerop code)
        (error "Failed to create PKCS#12 bundle: ~A" err)))

    ;; Cleanup CSR
    (when (probe-file client-csr) (delete-file client-csr))

    ;; Record issuance
    (record-cert-issued name)

    passphrase))

(defun client-cert-exists-p (name)
  "Check if a certificate exists for NAME (and is not revoked)."
  (and (probe-file (client-cert-file name))
       (not (cert-revoked-p name))))

;;── Certificate Listing ───────────────────────────────────────────────────────

(defun list-client-certs ()
  "List all issued client certificates.
   Returns a list of plists with :name, :issued-at, :revoked-p."
  (let ((clients-dir (clients-directory))
        (revoked (load-revoked-list))
        (results nil))
    (when (probe-file clients-dir)
      (dolist (file (uiop:directory-files clients-dir))
        (when (string= (pathname-type file) "crt")
          (let* ((name (pathname-name file))
                 (issued-at (file-write-date file)))
            (push (list :name name
                        :issued-at issued-at
                        :revoked-p (member name revoked :test #'string=))
                  results)))))
    (nreverse results)))

;;── Certificate Revocation ────────────────────────────────────────────────────

(defun load-revoked-list ()
  "Load the list of revoked certificate names."
  (let ((file (revoked-file)))
    (if (probe-file file)
        (with-open-file (stream file :direction :input)
          (loop for line = (read-line stream nil nil)
                while line
                collect (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
        nil)))

(defun save-revoked-list (names)
  "Save the list of revoked certificate names."
  (ensure-certs-directory)
  (with-open-file (stream (revoked-file)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (name names)
      (write-line name stream))))

(defun cert-revoked-p (name)
  "Check if a certificate is revoked."
  (member name (load-revoked-list) :test #'string=))

(defun revoke-client-cert (name)
  "Revoke a client certificate by NAME.
   The certificate files are kept for audit purposes but marked as revoked."
  (unless (probe-file (client-cert-file name))
    (error "No certificate found for '~A'" name))

  (when (cert-revoked-p name)
    (error "Certificate for '~A' is already revoked" name))

  (let ((revoked (load-revoked-list)))
    (push name revoked)
    (save-revoked-list revoked))

  ;; Delete the .p12 file so it can't be used
  (let ((p12 (client-p12-file name)))
    (when (probe-file p12)
      (delete-file p12)))

  (format t "Certificate for '~A' has been revoked.~%" name)
  t)

(defun record-cert-issued (name)
  "Record that a certificate was issued (for future tracking)."
  (declare (ignore name))
  ;; For now, the file existence is the record
  ;; Could add to a log file with timestamps if needed
  t)

;;── IP Address Helpers ────────────────────────────────────────────────────────

(defun ip-address-p (string)
  "Check if STRING looks like an IP address."
  (and (stringp string)
       (or (every (lambda (c) (or (digit-char-p c) (char= c #\.))) string)
           (find #\: string))))  ; IPv6

(defun detect-local-ips ()
  "Detect local IP addresses including Tailscale."
  (let ((ips nil))
    (handler-case
        (multiple-value-bind (out err code)
            (uiop:run-program '("hostname" "-I")
                              :output :string
                              :error-output :string
                              :ignore-error-status t)
          (declare (ignore err))
          (when (zerop code)
            (setf ips (str:split #\Space (str:trim out)))))
      (error () nil))
    ;; Filter out empty strings and link-local
    (remove-if (lambda (ip)
                 (or (zerop (length ip))
                     (str:starts-with-p "fe80:" ip)
                     (str:starts-with-p "169.254." ip)))
               ips)))

;;── Pairing System ────────────────────────────────────────────────────────────

(defvar *pending-pairings* (make-hash-table :test #'equal)
  "Map of pairing tokens to pairing info.")

(defvar *pairing-lock* (bt:make-lock "pairing-lock")
  "Lock for pairing operations.")

(defstruct pairing-request
  token
  device-name
  passphrase
  p12-path
  created-at
  expires-at)

(defun generate-pairing-token ()
  "Generate a short, URL-safe pairing token."
  (let ((chars "abcdefghijkmnpqrstuvwxyz23456789"))  ; Avoid confusing chars
    (coerce (loop repeat 8
                  collect (char chars (random (length chars))))
            'string)))

(defun create-pairing-request (device-name &key (expiry-minutes 10))
  "Create a pairing request for DEVICE-NAME.
   Returns the pairing-request struct."
  (let* ((passphrase (issue-client-cert device-name))
         (token (generate-pairing-token))
         (now (get-universal-time))
         (request (make-pairing-request
                   :token token
                   :device-name device-name
                   :passphrase passphrase
                   :p12-path (namestring (client-p12-file device-name))
                   :created-at now
                   :expires-at (+ now (* expiry-minutes 60)))))
    (bt:with-lock-held (*pairing-lock*)
      (setf (gethash token *pending-pairings*) request))
    request))

(defun get-pairing-request (token)
  "Get a pairing request by token, or NIL if not found or expired."
  (bt:with-lock-held (*pairing-lock*)
    (let ((request (gethash token *pending-pairings*)))
      (when request
        (if (> (get-universal-time) (pairing-request-expires-at request))
            (progn
              (remhash token *pending-pairings*)
              nil)
            request)))))

(defun consume-pairing-request (token)
  "Get and remove a pairing request (single-use).
   Returns the request or NIL."
  (bt:with-lock-held (*pairing-lock*)
    (let ((request (gethash token *pending-pairings*)))
      (when request
        (remhash token *pending-pairings*)
        (if (> (get-universal-time) (pairing-request-expires-at request))
            nil
            request)))))

(defun cleanup-expired-pairings ()
  "Remove expired pairing requests."
  (let ((now (get-universal-time)))
    (bt:with-lock-held (*pairing-lock*)
      (maphash (lambda (token request)
                 (when (> now (pairing-request-expires-at request))
                   (remhash token *pending-pairings*)))
               *pending-pairings*))))

;;── QR Code Generation (ASCII) ────────────────────────────────────────────────

(defun generate-qr-ascii (data)
  "Generate ASCII art QR code for DATA using qrencode if available.
   Returns the QR code string or NIL if qrencode is not available."
  (handler-case
      (multiple-value-bind (out err code)
          (uiop:run-program (list "qrencode" "-t" "UTF8" "-m" "2" data)
                            :output :string
                            :error-output :string
                            :ignore-error-status t)
        (declare (ignore err))
        (if (zerop code)
            out
            nil))
    (error () nil)))
