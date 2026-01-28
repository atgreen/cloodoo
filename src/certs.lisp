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

(defun generate-passphrase (&optional (word-count 4))
  "Generate a memorable passphrase with WORD-COUNT words using CSPRNG.
   Default 4 words (~26 bits entropy from 72-word list)."
  (let ((words (loop repeat word-count
                     collect (nth (secure-random (length *passphrase-words*))
                                  *passphrase-words*))))
    (format nil "~{~A~^-~}" words)))

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

  (ensure-certs-directory)
  (ensure-directories-exist (clients-directory))

  (let ((ca-key-path (namestring (ca-key-file)))
        (ca-cert-path (namestring (ca-cert-file))))

    ;; Generate 4096-bit RSA key pair
    (multiple-value-bind (n e d p q)
        (cl-x509:generate-rsa-key-pair :bits 4096)

      ;; Save private key
      (cl-x509:save-private-key-pem ca-key-path n e d p q)

      ;; Generate self-signed CA certificate
      (let ((cert-der (cl-x509:generate-self-signed-certificate
                       :n n :e e :d d
                       :cn "Cloodoo CA"
                       :o "Cloodoo"
                       :ou (get-device-id)
                       :days days)))
        (cl-x509:save-certificate-pem cert-der ca-cert-path)))

    (format t "CA initialized successfully.~%")
    (format t "CA certificate: ~A~%" ca-cert-path)
    t))

;;── Server Certificate ────────────────────────────────────────────────────────

(defun server-cert-initialized-p ()
  "Check if the server certificate has been initialized."
  (and (probe-file (server-key-file))
       (probe-file (server-cert-file))))

(defun init-server-cert (&key (days 365) hosts)
  "Initialize the server certificate.
   HOSTS is a list of hostnames/IPs for the SAN extension.
   If not provided, uses localhost, 127.0.0.1, and tries to detect local network IPs."
  (unless (ca-initialized-p)
    (error "CA not initialized. Run 'cloodoo cert init' first."))

  (let* ((server-key-path (namestring (server-key-file)))
         (server-cert-path (namestring (server-cert-file)))
         (all-hosts (or hosts
                        (append '("localhost" "127.0.0.1")
                                (detect-local-ips)))))

    ;; Load CA key for signing
    (multiple-value-bind (ca-n ca-e ca-d)
        (cl-x509:load-rsa-key-pair (namestring (ca-key-file)))
      (declare (ignore ca-e))

      ;; Generate server key pair
      (multiple-value-bind (srv-n srv-e srv-d srv-p srv-q)
          (cl-x509:generate-rsa-key-pair :bits 2048)

        (cl-x509:save-private-key-pem server-key-path srv-n srv-e srv-d srv-p srv-q)

        ;; Build SAN entries
        (let ((san-names (mapcar (lambda (host)
                                   (if (ip-address-p host)
                                       (list :ip host)
                                       (list :dns host)))
                                 all-hosts)))

          ;; Generate server certificate signed by CA
          (let ((cert-der (cl-x509:generate-signed-certificate
                           :signer-n ca-n
                           :signer-d ca-d
                           :issuer-cn "Cloodoo CA"
                           :issuer-o "Cloodoo"
                           :issuer-ou (get-device-id)
                           :subject-n srv-n
                           :subject-e srv-e
                           :cn "cloodoo-server"
                           :o "Cloodoo"
                           :days days
                           :extensions (list
                                        (cl-x509:encode-basic-constraints :ca nil)
                                        (cl-x509:encode-key-usage
                                         :digital-signature :key-encipherment)
                                        (cl-x509:encode-extended-key-usage
                                         cl-x509:*oid-server-auth*)
                                        (cl-x509:encode-san-extension san-names)))))
            (cl-x509:save-certificate-pem cert-der server-cert-path)))))

    (format t "Server certificate created.~%")
    (format t "Valid hosts: ~{~A~^, ~}~%" all-hosts)
    t))

;;── Client Certificate ────────────────────────────────────────────────────────

(defun issue-client-cert (name &key (days 365))
  "Issue a client certificate for NAME.
   Returns the passphrase for pairing verification."
  (unless (ca-initialized-p)
    (error "CA not initialized. Run 'cloodoo cert init' first."))

  (when (client-cert-exists-p name)
    (error "Certificate for '~A' already exists. Revoke it first or choose a different name." name))

  ;; Clear any prior revocation so re-issued certs aren't born revoked
  (when (cert-revoked-p name)
    (let ((revoked (remove name (load-revoked-list) :test #'string=)))
      (save-revoked-list revoked)))

  (ensure-directories-exist (clients-directory))

  (let ((client-key-path (namestring (client-key-file name)))
        (client-cert-path (namestring (client-cert-file name)))
        (passphrase (generate-passphrase 3)))

    ;; Load CA key for signing
    (multiple-value-bind (ca-n ca-e ca-d)
        (cl-x509:load-rsa-key-pair (namestring (ca-key-file)))
      (declare (ignore ca-e))

      ;; Generate client key pair
      (multiple-value-bind (cli-n cli-e cli-d cli-p cli-q)
          (cl-x509:generate-rsa-key-pair :bits 2048)

        (cl-x509:save-private-key-pem client-key-path cli-n cli-e cli-d cli-p cli-q)

        ;; Generate client certificate signed by CA
        (let ((cert-der (cl-x509:generate-signed-certificate
                         :signer-n ca-n
                         :signer-d ca-d
                         :issuer-cn "Cloodoo CA"
                         :issuer-o "Cloodoo"
                         :issuer-ou (get-device-id)
                         :subject-n cli-n
                         :subject-e cli-e
                         :cn name
                         :o "Cloodoo Client"
                         :days days
                         :extensions (list
                                      (cl-x509:encode-basic-constraints :ca nil)
                                      (cl-x509:encode-key-usage
                                       :digital-signature :key-encipherment)
                                      (cl-x509:encode-extended-key-usage
                                       cl-x509:*oid-client-auth*)))))
          (cl-x509:save-certificate-pem cert-der client-cert-path))))

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
  "Detect local IP addresses including VPN interfaces."
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
    ;; Filter out empty strings, link-local, and IPv6 (not supported in SAN encoding)
    (let ((filtered (remove-if (lambda (ip)
                                 (or (zerop (length ip))
                                     (find #\: ip)
                                     (str:starts-with-p "169.254." ip)))
                               ips)))
      ;; Prefer VPN IPs (100.x.x.x CGNAT range) first
      (stable-sort (copy-list filtered)
                    (lambda (a b)
                      (declare (ignore b))
                      (str:starts-with-p "100." a))))))

;;── Pairing System ────────────────────────────────────────────────────────────

(defvar *pending-pairings* (make-hash-table :test #'equal)
  "Map of pairing tokens to pairing info.")

(defvar *pairing-lock* (bt:make-lock "pairing-lock")
  "Lock for pairing operations.")

(defstruct pairing-request
  token
  device-name
  passphrase
  created-at
  expires-at)

(defun generate-pairing-token ()
  "Generate a short, URL-safe pairing token using CSPRNG.
   12 characters from a 31-char alphabet ≈ 59 bits of entropy."
  (let ((chars "abcdefghijkmnpqrstuvwxyz23456789"))  ; Avoid confusing chars
    (coerce (loop repeat 12
                  collect (char chars (secure-random (length chars))))
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

;;── Paired Certificate Storage (certs received from remote servers) ───────────

(defun paired-directory ()
  "Directory for storing certificates received from remote servers during pairing."
  (merge-pathnames "paired/" (certs-directory)))

(defun paired-server-directory (server-id)
  "Directory for a specific remote server's client cert."
  (merge-pathnames (format nil "~A/" server-id) (paired-directory)))

(defun paired-client-cert-file (server-id)
  "Path to the client certificate received from a remote server."
  (merge-pathnames "client.crt" (paired-server-directory server-id)))

(defun paired-client-key-file (server-id)
  "Path to the client key received from a remote server."
  (merge-pathnames "client.key" (paired-server-directory server-id)))

(defun paired-sync-config-file (server-id)
  "Path to the sync config file for a paired remote server."
  (merge-pathnames "sync.json" (paired-server-directory server-id)))

(defun find-paired-sync-config ()
  "Scan paired/*/sync.json for the first valid sync config.
   Returns (values host port server-id) or NIL if none found."
  (let ((paired-dir (paired-directory)))
    (when (probe-file paired-dir)
      (dolist (dir (uiop:subdirectories paired-dir))
        (let* ((server-id (car (last (pathname-directory dir))))
               (config-path (merge-pathnames "sync.json" dir)))
          (when (probe-file config-path)
            (handler-case
                (let* ((content (uiop:read-file-string config-path))
                       (config (jzon:parse content))
                       (host (gethash "host" config))
                       (port (gethash "port" config)))
                  (when (and host port)
                    (return-from find-paired-sync-config
                      (values host port server-id))))
              (error () nil))))))))


;;── QR Code Generation (ASCII) ────────────────────────────────────────────────

(defun generate-qr-ascii (data)
  "Generate Unicode QR code for DATA using cl-qrencode.
   Uses half-block characters to render two rows per line.
   Returns the QR code string or NIL on error."
  (handler-case
      (let* ((qr (cl-qrencode:encode-symbol data :level :level-m))
             (size (cl-qrencode:modules qr))
             (mat (cl-qrencode:matrix qr))
             (margin 2)
             (total (+ size (* 2 margin)))
             (lines nil))
        ;; Process two rows at a time using Unicode half-block characters:
        ;;   top=dark, bot=dark -> "█" (full block)
        ;;   top=dark, bot=light -> "▀" (upper half)
        ;;   top=light, bot=dark -> "▄" (lower half)
        ;;   top=light, bot=light -> " " (space)
        (loop for y from 0 below total by 2
              do (let ((line (make-string-output-stream)))
                   (write-string "  " line)  ; left indent
                   (loop for x from 0 below total
                         do (let ((top-dark (and (>= y margin) (< y (+ size margin))
                                                 (>= x margin) (< x (+ size margin))
                                                 (cl-qrencode:dark-module-p
                                                  mat (- y margin) (- x margin))))
                                  (bot-dark (and (>= (1+ y) margin) (< (1+ y) (+ size margin))
                                                 (>= x margin) (< x (+ size margin))
                                                 (cl-qrencode:dark-module-p
                                                  mat (- (1+ y) margin) (- x margin)))))
                              (write-string
                               (cond ((and top-dark bot-dark) "█")
                                     (top-dark "▀")
                                     (bot-dark "▄")
                                     (t " "))
                               line)))
                   (push (get-output-stream-string line) lines)))
        (format nil "~{~A~%~}" (nreverse lines)))
    (error () nil)))
