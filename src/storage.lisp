;;; storage.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cloodoo)

;;── Platform-Aware Directory Support ──────────────────────────────────────────
;;
;; On Unix/Linux: Uses XDG Base Directory Specification
;;   - Data:   $XDG_DATA_HOME/cloodoo/   (default ~/.local/share/cloodoo/)
;;   - Config: $XDG_CONFIG_HOME/cloodoo/ (default ~/.config/cloodoo/)
;;   - Cache:  $XDG_CACHE_HOME/cloodoo/  (default ~/.cache/cloodoo/)
;;
;; On Windows: Uses %APPDATA%
;;   - All:    %APPDATA%\cloodoo\
;;
;; Legacy ~/.cloodoo/ is supported for backward compatibility on Unix.

(defun windowsp ()
  "Return T if running on Windows."
  (member :windows *features*))

(defun legacy-data-directory ()
  "Return the legacy ~/.cloodoo/ directory path."
  (merge-pathnames ".cloodoo/" (user-homedir-pathname)))

(defun windows-appdata ()
  "Return Windows %APPDATA% directory."
  (let ((appdata (uiop:getenv "APPDATA")))
    (when (and appdata (> (length appdata) 0))
      (pathname (if (str:ends-with-p "\\" appdata)
                    appdata
                    (concatenate 'string appdata "\\"))))))

(defun xdg-data-home ()
  "Return XDG_DATA_HOME or default ~/.local/share/"
  (let ((env (uiop:getenv "XDG_DATA_HOME")))
    (if (and env (> (length env) 0))
        (uiop:ensure-directory-pathname env)
        (merge-pathnames ".local/share/" (user-homedir-pathname)))))

(defun xdg-config-home ()
  "Return XDG_CONFIG_HOME or default ~/.config/"
  (let ((env (uiop:getenv "XDG_CONFIG_HOME")))
    (if (and env (> (length env) 0))
        (uiop:ensure-directory-pathname env)
        (merge-pathnames ".config/" (user-homedir-pathname)))))

(defun xdg-cache-home ()
  "Return XDG_CACHE_HOME or default ~/.cache/"
  (let ((env (uiop:getenv "XDG_CACHE_HOME")))
    (if (and env (> (length env) 0))
        (uiop:ensure-directory-pathname env)
        (merge-pathnames ".cache/" (user-homedir-pathname)))))

(defun data-directory ()
  "Return the path to the cloodoo data directory.
   If CLOODOO_HOME is set: $CLOODOO_HOME/data/
   On Windows: %APPDATA%\\cloodoo\\
   On Unix: XDG_DATA_HOME/cloodoo/ (default ~/.local/share/cloodoo/)
   Falls back to legacy ~/.cloodoo/ if it exists and new location doesn't."
  ;; Check for CLOODOO_HOME first
  (let ((cloodoo-home (uiop:getenv "CLOODOO_HOME")))
    (if (and cloodoo-home (> (length cloodoo-home) 0))
        (merge-pathnames "data/" (uiop:ensure-directory-pathname cloodoo-home))
        ;; Otherwise use platform defaults
        (if (windowsp)
            ;; Windows: use %APPDATA%\cloodoo\
            (let ((appdata (windows-appdata)))
              (if appdata
                  (merge-pathnames "cloodoo/" appdata)
                  (legacy-data-directory)))
            ;; Unix: use XDG with legacy fallback
            (let ((xdg-dir (merge-pathnames "cloodoo/" (xdg-data-home)))
                  (legacy-dir (legacy-data-directory)))
              (if (or (probe-file xdg-dir)
                      (not (probe-file legacy-dir)))
                  xdg-dir
                  legacy-dir))))))

(defun config-directory ()
  "Return the path to the cloodoo config directory.
   If CLOODOO_HOME is set: $CLOODOO_HOME/config/
   On Windows: %APPDATA%\\cloodoo\\
   On Unix: XDG_CONFIG_HOME/cloodoo/ (default ~/.config/cloodoo/)"
  ;; Check for CLOODOO_HOME first
  (let ((cloodoo-home (uiop:getenv "CLOODOO_HOME")))
    (if (and cloodoo-home (> (length cloodoo-home) 0))
        (merge-pathnames "config/" (uiop:ensure-directory-pathname cloodoo-home))
        ;; Otherwise use platform defaults
        (if (windowsp)
            (data-directory)  ; Windows uses same dir for data and config
            ;; Unix: use XDG with legacy fallback
            (let ((xdg-dir (merge-pathnames "cloodoo/" (xdg-config-home)))
                  (legacy-dir (legacy-data-directory)))
              (if (or (probe-file xdg-dir)
                      (not (probe-file legacy-dir)))
                  xdg-dir
                  legacy-dir))))))

(defun cache-directory ()
  "Return the path to the cloodoo cache directory.
   If CLOODOO_HOME is set: $CLOODOO_HOME/cache/
   On Windows: %LOCALAPPDATA%\\cloodoo\\
   On Unix: XDG_CACHE_HOME/cloodoo/ (default ~/.cache/cloodoo/)"
  ;; Check for CLOODOO_HOME first
  (let ((cloodoo-home (uiop:getenv "CLOODOO_HOME")))
    (if (and cloodoo-home (> (length cloodoo-home) 0))
        (merge-pathnames "cache/" (uiop:ensure-directory-pathname cloodoo-home))
        ;; Otherwise use platform defaults
        (if (windowsp)
            (let ((localappdata (uiop:getenv "LOCALAPPDATA")))
              (if (and localappdata (> (length localappdata) 0))
                  (merge-pathnames "cloodoo/" (uiop:ensure-directory-pathname localappdata))
                  (data-directory)))
            (merge-pathnames "cloodoo/" (xdg-cache-home))))))

(defun todos-file ()
  "Return the path to the todos.json file."
  (merge-pathnames "todos.json" (data-directory)))

(defun user-context-file ()
  "Return the path to the user context file."
  (merge-pathnames "context.txt" (config-directory)))

(defun sync-config-file ()
  "Return the path to the sync-config.lisp file."
  (merge-pathnames "sync-config.lisp" (config-directory)))

(defun load-sync-config ()
  "Load sync configuration from sync-config.lisp.
   Returns a plist with :host, :port, :client-cert, :client-key.
   If file doesn't exist, returns default localhost config."
  (let ((config-path (sync-config-file)))
    (if (probe-file config-path)
        (handler-case
            (with-open-file (in config-path)
              ;; No #. evaluation while reading config (cloodoo-oun)
              (let ((*read-eval* nil))
                (read in)))
          (error (e)
            (warn "Failed to read sync config from ~A: ~A" config-path e)
            '(:host "localhost" :port 50051)))
        '(:host "localhost" :port 50051))))

(defun ensure-data-directory ()
  "Ensure the data directory exists."
  (ensure-directories-exist (data-directory)))

(defun ensure-config-directory ()
  "Ensure the config directory exists."
  (ensure-directories-exist (config-directory)))

(defun ensure-cache-directory ()
  "Ensure the cache directory exists."
  (ensure-directories-exist (cache-directory)))

;;── Serialization ──────────────────────────────────────────────────────────────

(defun location-info-to-hash-table (location-info)
  "Convert a location-info plist to a hash table for JSON serialization."
  (when location-info
    (let ((ht (make-hash-table :test #'equal)))
      (setf (gethash "name" ht) (getf location-info :name))
      (setf (gethash "address" ht) (getf location-info :address))
      (setf (gethash "phone" ht) (getf location-info :phone))
      (setf (gethash "map_url" ht) (getf location-info :map-url))
      (setf (gethash "website" ht) (getf location-info :website))
      ht)))

(defun hash-table-to-location-info (ht)
  "Convert a hash table from JSON to a location-info plist."
  (when (and ht (hash-table-p ht))
    (let ((name (gethash "name" ht)))
      (when name
        (list :name name
              :address (gethash "address" ht)
              :phone (gethash "phone" ht)
              :map-url (gethash "map_url" ht)
              :website (gethash "website" ht))))))

(defun todo-to-hash-table (todo)
  "Convert a TODO object to a hash table for JSON serialization."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "id" ht) (todo-id todo))
    (setf (gethash "title" ht) (todo-title todo))
    (setf (gethash "description" ht) (todo-description todo))
    (setf (gethash "priority" ht) (string-downcase (symbol-name (todo-priority todo))))
    (setf (gethash "status" ht) (string-downcase (symbol-name (todo-status todo))))
    (setf (gethash "scheduled_date" ht) (when (todo-scheduled-date todo)
                                           (lt:format-rfc3339-timestring nil (todo-scheduled-date todo))))
    (setf (gethash "due_date" ht) (when (todo-due-date todo)
                                     (lt:format-rfc3339-timestring nil (todo-due-date todo))))
    (setf (gethash "tags" ht) (todo-tags todo))
    (setf (gethash "location_info" ht) (location-info-to-hash-table (todo-location-info todo)))
    (setf (gethash "url" ht) (todo-url todo))
    (setf (gethash "attachment_hashes" ht) (when (todo-attachment-hashes todo)
                                              (coerce (todo-attachment-hashes todo) 'vector)))
    (setf (gethash "created_at" ht) (lt:format-rfc3339-timestring nil (todo-created-at todo)))
    (setf (gethash "completed_at" ht) (when (todo-completed-at todo)
                                         (lt:format-rfc3339-timestring nil (todo-completed-at todo))))
    (setf (gethash "device_id" ht) (todo-device-id todo))
    (setf (gethash "repeat_interval" ht) (todo-repeat-interval todo))
    (setf (gethash "repeat_unit" ht) (when (todo-repeat-unit todo)
                                        (string-downcase (symbol-name (todo-repeat-unit todo)))))
    ht))

(defun hash-table-to-todo (ht)
  "Convert a hash table from jzon to a TODO object."
  (make-instance 'todo
                 :id (gethash "id" ht)
                 :title (gethash "title" ht)
                 :description (let ((d (gethash "description" ht)))
                                (unless (eq d 'null) d))
                 :priority (intern (string-upcase (gethash "priority" ht)) :keyword)
                 :status (intern (string-upcase (gethash "status" ht)) :keyword)
                 :scheduled-date (let ((d (gethash "scheduled_date" ht)))
                                   (when (and d (not (eq d 'null)) (stringp d))
                                     (lt:parse-timestring d)))
                 :due-date (let ((d (gethash "due_date" ht)))
                             (when (and d (not (eq d 'null)) (stringp d))
                               (lt:parse-timestring d)))
                 :tags (let ((tags (gethash "tags" ht)))
                         (when (and tags (not (eq tags 'null)) (typep tags 'sequence))
                           (coerce tags 'list)))
                 :location-info (hash-table-to-location-info (gethash "location_info" ht))
                 :url (let ((u (gethash "url" ht)))
                        (unless (eq u 'null) u))
                 :attachment-hashes (let ((ah (gethash "attachment_hashes" ht)))
                                      (when (and ah (not (eq ah 'null)))
                                        (coerce ah 'list)))
                 :created-at (lt:parse-timestring (gethash "created_at" ht))
                 :completed-at (let ((c (gethash "completed_at" ht)))
                                 (when (and c (not (eq c 'null)) (stringp c))
                                   (lt:parse-timestring c)))
                 :device-id (let ((d (gethash "device_id" ht)))
                              (unless (eq d 'null) d))
                 :repeat-interval (let ((i (gethash "repeat_interval" ht)))
                                    (unless (or (null i) (eq i 'null)) i))
                 :repeat-unit (let ((u (gethash "repeat_unit" ht)))
                                (when (and u (not (eq u 'null)) (stringp u))
                                  (intern (string-upcase u) :keyword)))))

;;── Load/Save ──────────────────────────────────────────────────────────────────

;;; load-todos / save-todos / load-presets / save-presets are defined in
;;; db.lisp (SQLite).  The JSON versions that lived here were shadowed by
;;; those redefinitions on every load; migrate-json-to-db reads the legacy
;;; JSON file directly (cloodoo-y5r).

;;── User Context ──────────────────────────────────────────────────────────────

(defparameter *default-user-context*
  "# User Context for TODO Enrichment
#
# This file provides personal context to help the AI assistant better understand
# and enrich your TODO items. Add information about yourself, your work, family,
# preferences, common locations, etc.
#
# Examples:
# - I work at Acme Corp as a software engineer
# - My dentist is Dr. Smith at 123 Main St, phone 555-1234
# - My kids are Alice (10) and Bob (7)
# - I live in Springfield
# - My preferred grocery store is Whole Foods on Oak Street
#
# Add your context below:

")

(defun load-user-context ()
  "Load the user context from the context file. Returns nil if file doesn't exist or is empty."
  (let ((file (user-context-file)))
    (when (probe-file file)
      (let ((content (uiop:read-file-string file)))
        (when (> (length (string-trim '(#\Space #\Newline #\Tab #\Return) content)) 0)
          content)))))

(defun ensure-user-context-file ()
  "Ensure the user context file exists with default template."
  (ensure-config-directory)
  (let ((file (user-context-file)))
    (unless (probe-file file)
      (with-open-file (stream file
                              :direction :output
                              :if-does-not-exist :create)
        (write-string *default-user-context* stream)))
    file))

(defun get-editor ()
  "Get the user's preferred editor from EDITOR or VISUAL env vars, or fall back to common editors."
  (or (uiop:getenv "EDITOR")
      (uiop:getenv "VISUAL")
      (cond
        ((uiop:run-program "which nano" :ignore-error-status t :output nil) "nano")
        ((uiop:run-program "which vim" :ignore-error-status t :output nil) "vim")
        ((uiop:run-program "which vi" :ignore-error-status t :output nil) "vi")
        (t "nano"))))

