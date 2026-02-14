;;; enrich.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cloodoo)

;;── LLM Enrichment for TODO Items ─────────────────────────────────────────────

(defvar *enrichment-enabled* t
  "Whether to use LLM enrichment for new TODO items.")

;;── LLM Provider Configuration ────────────────────────────────────────────────

(defvar *llm-provider* nil
  "Current LLM provider: :gemini, :ollama, :openai, or :anthropic.")

(defvar *llm-model* nil
  "Model name for the current provider.")

(defvar *llm-endpoint* nil
  "Custom endpoint URL (optional, uses default if nil).")

(defvar *llm-api-key* nil
  "API key for providers that require authentication.")

(defun default-config ()
  "Return default configuration plist."
  '(:provider :gemini
    :model "gemini-2.0-flash"
    :endpoint nil
    :api-key-env "GEMINI_API_KEY"))

(defun config-file-path ()
  "Return path to the config file."
  (merge-pathnames "config.lisp" (config-directory)))

(defun load-config ()
  "Load configuration from config file or return defaults."
  (let ((config-file (config-file-path)))
    (cond ((probe-file config-file) (handler-case
            (with-open-file (in config-file :direction :input)
              (let* ((eof (list nil))
                     (config (read in nil eof)))
                (cond ((eq config eof)
                       (llog:warn "Config file is empty, using defaults")
                       (default-config))
                      (t
                       (llog:info "Loaded config from file" :path (namestring config-file))
                       config))))
          (error (e)
            (llog:warn "Failed to read config file, using defaults" :error (format nil "~A" e))
            (default-config))))
      (t
          (llog:info "No config file found, creating default" :path (namestring config-file))
          (save-config (default-config))
          (default-config)))))

(defun save-config (config)
  "Save configuration to config file."
  (ensure-config-directory)
  (let ((config-file (config-file-path)))
    (with-open-file (out config-file :direction :output :if-exists :supersede)
      (format out ";;; Cloodoo LLM Configuration~%")
      (format out ";;; Provider options: :gemini, :ollama, :openai, :anthropic~%")
      (format out ";;;~%")
      (format out ";;; For Ollama (local):~%")
      (format out ";;;   :provider :ollama~%")
      (format out ";;;   :model \"llama3.2:latest\"~%")
      (format out ";;;   :endpoint \"http://localhost:11434\"  ; optional, this is the default~%")
      (format out ";;;~%")
      (format out ";;; For Gemini:~%")
      (format out ";;;   :provider :gemini~%")
      (format out ";;;   :model \"gemini-2.0-flash\"~%")
      (format out ";;;   :api-key-env \"GEMINI_API_KEY\"  ; env var name containing API key~%")
      (format out ";;;~%")
      (format out ";;; For OpenAI:~%")
      (format out ";;;   :provider :openai~%")
      (format out ";;;   :model \"gpt-4o-mini\"~%")
      (format out ";;;   :api-key-env \"OPENAI_API_KEY\"~%")
      (format out ";;;~%")
      (format out ";;; For Anthropic:~%")
      (format out ";;;   :provider :anthropic~%")
      (format out ";;;   :model \"claude-sonnet-4-20250514\"~%")
      (format out ";;;   :api-key-env \"ANTHROPIC_API_KEY\"~%")
      (format out "~%")
      (write config :stream out :pretty t :case :downcase))
    (llog:info "Saved config" :path (namestring config-file))))

(defun apply-config (config)
  "Apply configuration to global variables."
  (setf *llm-provider* (getf config :provider)
        *llm-model* (getf config :model)
        *llm-endpoint* (getf config :endpoint))
  ;; Load API key from environment variable if specified
  (let ((api-key-env (getf config :api-key-env)))
    (when api-key-env
      ;; Try loading .env files first
      (let ((env-file (merge-pathnames ".env" (config-directory))))
        (when (probe-file env-file)
          (dotenv:load-env env-file)))
      (when (probe-file ".env")
        (dotenv:load-env ".env"))
      ;; Now get the key
      (setf *llm-api-key* (uiop:getenv api-key-env))))
  (llog:info "Applied LLM config"
             :provider *llm-provider*
             :model *llm-model*
             :endpoint *llm-endpoint*
             :has-api-key (if *llm-api-key* "yes" "no")))

;;── Debug Logging ─────────────────────────────────────────────────────────────

(defun init-debug-logging ()
  "Initialize debug logging to cloodoo-debug.log file."
  (ensure-cache-directory)
  (let* ((log-file (merge-pathnames "cloodoo-debug.log" (cache-directory)))
         (file-output (llog:make-file-output (namestring log-file)
                                             :encoder (llog:make-json-encoder)
                                             :buffer-mode :none)))  ; Immediate flush
    (setf llog:*logger* (llog:make-logger :outputs (list file-output)
                                          :level :debug))
    (llog:info "Debug logging initialized"
               :log-file (namestring log-file)
               :timestamp (lt:format-rfc3339-timestring nil (lt:now)))))

(defun init-enrichment ()
  "Initialize enrichment by loading config and setting up the LLM provider."
  ;; Initialize debug logging first
  (ensure-config-directory)
  (init-debug-logging)

  (llog:info "Initializing enrichment system")

  ;; Load and apply configuration
  (let ((config (load-config)))
    (apply-config config))

  ;; Check if we have what we need for the selected provider
  (cond
    ;; Ollama doesn't need an API key
    ((eql *llm-provider* :ollama)
     (llog:info "Using Ollama provider (no API key required)"
                :model *llm-model*
                :endpoint (or *llm-endpoint* "http://localhost:11434")))
    ;; Other providers need an API key
    (*llm-api-key*
     (llog:info "LLM provider configured"
                :provider *llm-provider*
                :model *llm-model*
                :key-length (length *llm-api-key*)
                :key-prefix (subseq *llm-api-key* 0 (min 8 (length *llm-api-key*)))))
    (t
     (llog:warn "No API key found for provider - enrichment disabled"
                :provider *llm-provider*)
     (setf *enrichment-enabled* nil)))

  (llog:info "Enrichment initialization complete"
             :enabled *enrichment-enabled*
             :provider *llm-provider*
             :model *llm-model*))

;;── URL Metadata Fetching ─────────────────────────────────────────────────────

(defparameter *url-fetch-timeout* 5
  "Timeout in seconds for fetching URL metadata.")

(defun extract-url-from-text (text)
  "Extract the first URL from text. Returns the URL string or NIL."
  (when (and text (stringp text) (> (length text) 0))
    (let ((patterns '("https?://[^\\s<>\"'`\\)\\]\\}]+")))
      (dolist (pattern patterns)
        (multiple-value-bind (match-start match-end)
            (ppcre:scan pattern text)
          (when match-start
            (return-from extract-url-from-text
              (subseq text match-start match-end)))))
      nil)))

(defun extract-html-title (html)
  "Extract the <title> content from HTML. Returns string or NIL."
  (when html
    (multiple-value-bind (match groups)
        (ppcre:scan-to-strings "(?is)<title[^>]*>([^<]*)</title>" html)
      (declare (ignore match))
      (when (and groups (> (length groups) 0))
        (let ((title (string-trim '(#\Space #\Tab #\Newline #\Return) (aref groups 0))))
          (when (> (length title) 0)
            title))))))

(defun extract-meta-description (html)
  "Extract the meta description from HTML. Returns string or NIL."
  (when html
    ;; Try both name= and property= variants
    (let ((patterns '("(?is)<meta[^>]+(?:name|property)=[\"'](?:description|og:description)[\"'][^>]+content=[\"']([^\"']*)[\"']"
                      "(?is)<meta[^>]+content=[\"']([^\"']*)[\"'][^>]+(?:name|property)=[\"'](?:description|og:description)[\"']")))
      (dolist (pattern patterns)
        (multiple-value-bind (match groups)
            (ppcre:scan-to-strings pattern html)
          (declare (ignore match))
          (when (and groups (> (length groups) 0))
            (let ((desc (string-trim '(#\Space #\Tab #\Newline #\Return) (aref groups 0))))
              (when (> (length desc) 0)
                (return-from extract-meta-description desc))))))
      nil)))

(defun fetch-url-metadata (url)
  "Fetch a URL and extract title and description metadata.
   Returns a plist (:url :title :description) or NIL on failure.
   Uses a timeout to avoid blocking on slow/unresponsive sites."
  (when (and url (stringp url) (> (length url) 0))
    (llog:debug "Fetching URL metadata" :url url)
    (handler-case
        (let ((response (dex:get url
                                  :read-timeout *url-fetch-timeout*
                                  :connect-timeout *url-fetch-timeout*
                                  :keep-alive nil
                                  :headers '(("User-Agent" . "Mozilla/5.0 (compatible; Cloodoo/1.0)")
                                            ("Accept" . "text/html,*/*")))))
          (when response
            (let* ((html (if (stringp response)
                             response
                             (babel:octets-to-string response :encoding :utf-8 :errorp nil)))
                   (title (extract-html-title html))
                   (description (extract-meta-description html)))
              (llog:info "URL metadata fetched"
                         :url url
                         :title title
                         :description-length (when description (length description)))
              (list :url url
                    :title title
                    :description (when description
                                   ;; Truncate long descriptions
                                   (if (> (length description) 500)
                                       (concatenate 'string (subseq description 0 497) "...")
                                       description))))))
      (error (e)
        (let ((error-str (format nil "~A" e)))
          (if (search "timeout" (string-downcase error-str))
              (llog:warn "URL fetch timed out" :url url :timeout *url-fetch-timeout*)
              (llog:warn "URL fetch failed" :url url :error error-str)))
        nil))))

(defparameter *enrichment-system-prompt-template*
  "You convert raw user input into structured TODO data. Fix all typos. Preserve all important details (destinations, names, locations, brands, quantities).

Today: ~A (~A)

Return ONLY valid JSON with these keys:
- task_title: Action-verb title for tasks; bare item name for list items (see rules below)
- description: Cleaned-up notes, or null
- category: Work | Personal | Health | Finance | Home | Family | Shopping | Travel | Learning | Other
- priority: \"high\" (urgent/ASAP/≤48h deadline/health/safety/financial/blocking) | \"medium\" (≤1 week/routine/appointments) | \"low\" (no deadline/someday/leisure)
- scheduled_date: ISO 8601 when to work on it, or null
- due_date: ISO 8601 deadline, or null
- repeat_interval: integer (1=every, 2=every other...), or null
- repeat_unit: \"day\" | \"week\" | \"month\" | \"year\", or null
- url: any URL from input (http/https/message/file) - NEVER drop URLs, or null
- list_name: exact list name if item belongs on a user-defined list, else null
- list_section: section for first item if list has sections, else null
- list_items: [{\"title\":\"...\",\"section\":\"...\"}] for multi-item or enumerable input; null for single items
- location: {\"name\",\"address\",\"phone\",\"map_url\",\"website\"} for any place/business, or null. map_url: https://www.google.com/maps/search/?api=1&query=URL+Encoded+Name

TITLE RULES:
- Tasks (list_name=null): Start with action verb. Keep all details.
  \"call CAA to tow car to Canadian Tire\" -> \"Call CAA to Tow Car to Canadian Tire\"
- List items (list_name set): BARE NAME ONLY. Strip verbs (buy/watch/read/get/add/pick up), articles (the/some), list-echoing words. KEEP brands, qualifiers, quantities.
  \"buy a2 milk\" -> \"A2 Milk\" | \"3 lbs ground beef\" -> \"3 lbs Ground Beef\" | \"watch Trading Places\" -> \"Trading Places\" | \"get organic eggs\" -> \"Organic Eggs\"

DATES: Parse relative to today. \"on tuesday\"/\"friday 3pm\" -> scheduled_date. \"due friday\" -> due_date. Include time (\"3pm\" -> \"T15:00:00\"). Ambiguous -> scheduled_date.
RECURRENCE: daily=1/day | weekly=1/week | biweekly=2/week | monthly=1/month | yearly=1/year | else null.
LOCATION: For any business/doctor/store/venue, fill location object. For calls, include known phone in description.

LIST ITEMS EXPANSION:
- Multiple items (comma/and-separated): generate list_items array
- \"all of\"/\"every\"/known collections: enumerate ALL items using your knowledge (e.g., complete filmographies, discographies, series entries)
- \"ingredients for\"/recipes: recall the ACTUAL recipe from authoritative culinary knowledge. List every ingredient with realistic quantities and correct grocery sections. Do not guess or abbreviate—use a real recipe.
- When expanding items, draw on your training data as if consulting a reference source: cookbooks for recipes, music databases for discographies, film databases for filmographies, etc. If you have access to the internet, consult authoritative sources.
- Single item: list_items=null, use task_title only

USER CONTEXT: If provided, use it to infer location, relationships, domain knowledge, and preferences.
~A
Examples:
title=\"dentist next tues\" notes=\"dr tam @ lawernce dentust\" -> {\"task_title\":\"Schedule Dentist Appointment\",\"description\":\"Dr. Tam @ Lawrence Dentist\",\"category\":\"Health\",\"priority\":\"medium\",\"scheduled_date\":\"2026-01-21\",\"due_date\":null,\"location\":{\"name\":\"Lawrence Family Dentist\",\"address\":null,\"phone\":null,\"map_url\":\"https://www.google.com/maps/search/?api=1&query=Lawrence+Family+Dentist\",\"website\":null}} ; lint:suppress max-line-length
title=\"report due friday\" notes=\"quarterly sales\" -> {\"task_title\":\"Complete Quarterly Sales Report\",\"description\":\"Quarterly sales\",\"category\":\"Work\",\"priority\":\"high\",\"scheduled_date\":null,\"due_date\":\"2026-01-24\",\"location\":null} ; lint:suppress max-line-length
title=\"buy milk and eggs\" (Grocery list) -> {\"task_title\":\"Milk\",\"list_name\":\"Grocery\",\"list_section\":\"Dairy\",\"list_items\":[{\"title\":\"Milk\",\"section\":\"Dairy\"},{\"title\":\"Eggs\",\"section\":\"Dairy\"}],\"category\":\"Shopping\",\"priority\":\"medium\"} ; lint:suppress max-line-length
title=\"watch all Christopher Nolan movies\" (Movie list) -> {\"task_title\":\"Memento\",\"list_name\":\"Movie\",\"list_items\":[{\"title\":\"Memento\",\"section\":null},{\"title\":\"Insomnia\",\"section\":null},{\"title\":\"Batman Begins\",\"section\":null},{\"title\":\"The Prestige\",\"section\":null},{\"title\":\"The Dark Knight\",\"section\":null},{\"title\":\"Inception\",\"section\":null},{\"title\":\"The Dark Knight Rises\",\"section\":null},{\"title\":\"Interstellar\",\"section\":null},{\"title\":\"Dunkirk\",\"section\":null},{\"title\":\"Tenet\",\"section\":null},{\"title\":\"Oppenheimer\",\"section\":null}]} ; lint:suppress max-line-length
title=\"ingredients for pound cake\" (Grocery list, sections: Produce/Dairy/Meat/Pantry/...) -> {\"task_title\":\"Butter\",\"list_name\":\"Grocery\",\"list_section\":\"Dairy\",\"list_items\":[{\"title\":\"Butter\",\"section\":\"Dairy\"},{\"title\":\"Sugar\",\"section\":\"Pantry\"},{\"title\":\"Eggs\",\"section\":\"Dairy\"},{\"title\":\"All-Purpose Flour\",\"section\":\"Pantry\"},{\"title\":\"Vanilla Extract\",\"section\":\"Pantry\"}]} ; lint:suppress max-line-length

Respond with ONLY the JSON object, no markdown.")

(defun format-list-definitions-for-prompt ()
  "Format list definitions from the database for inclusion in the LLM prompt.
   Returns a string describing available lists, or empty string if none."
  (handler-case
      (let ((lists (db-load-list-definitions)))
        (if lists
            (with-output-to-string (s)
              (format s "~%USER LISTS (use exact names; only assign if input clearly matches):~%")
              (dolist (list-def lists)
                (format s "- ~A" (list-def-name list-def))
                (when (list-def-description list-def)
                  (format s ": ~A" (list-def-description list-def)))
                (when (list-def-sections list-def)
                  (format s " [sections: ~{~A~^, ~}]" (list-def-sections list-def)))
                (format s "~%"))
              (format s "If no list matches, set list_name=null (regular task).~%"))
            ""))
    (error () "")))

(defun get-enrichment-system-prompt (&optional list-definitions-text)
  "Get the enrichment system prompt with today's date and optional list definitions filled in."
  (let ((now (lt:now)))
    (format nil *enrichment-system-prompt-template*
            (lt:format-timestring nil now
                                 :format '(:long-weekday ", " :long-month " " :day ", " :year))
            (lt:format-timestring nil now
                                 :format '(:year "-" (:month 2) "-" (:day 2)))
            (or list-definitions-text ""))))

(defun make-completer ()
  "Create an LLM completer based on the configured provider."
  (llog:debug "Creating completer"
              :provider *llm-provider*
              :model *llm-model*
              :endpoint *llm-endpoint*)
  (case *llm-provider*
    (:ollama
     ;; Ollama endpoint should include /api/chat
     (let* ((base-endpoint (or *llm-endpoint* "http://localhost:11434"))
            (endpoint (if (str:ends-with-p "/api/chat" base-endpoint)
                          base-endpoint
                          (str:concat base-endpoint "/api/chat")))
            (completer (make-instance 'completions:ollama-completer
                                      :model *llm-model*
                                      :endpoint endpoint)))
       (llog:debug "Ollama completer created successfully" :endpoint endpoint)
       completer))
    (:gemini
     (when *llm-api-key*
       (let ((completer (make-instance 'completions:openai-completer
                                       :endpoint "https://generativelanguage.googleapis.com/v1beta/openai/chat/completions"
                                       :api-key *llm-api-key*
                                       :model *llm-model*)))
         (llog:debug "Gemini completer created successfully")
         completer)))
    (:openai
     (when *llm-api-key*
       (let ((completer (make-instance 'completions:openai-completer
                                       :api-key *llm-api-key*
                                       :model *llm-model*)))
         (llog:debug "OpenAI completer created successfully")
         completer)))
    (:anthropic
     (when *llm-api-key*
       (let ((completer (make-instance 'completions:anthropic-completer
                                       :api-key *llm-api-key*
                                       :model *llm-model*)))
         (llog:debug "Anthropic completer created successfully")
         completer)))
    (otherwise
     (llog:error "Unknown LLM provider" :provider *llm-provider*)
     nil)))

(defun get-json-response-format ()
  "Return the appropriate JSON response format parameter for the current provider.
   Ollama uses 'json', OpenAI-compatible APIs use 'json_object'."
  (case *llm-provider*
    (:ollama "json")
    (otherwise "json_object")))

(defun extract-json-from-response (response)
  "Extract JSON object from a response that may contain markdown code blocks."
  (let* ((trimmed (string-trim '(#\Space #\Newline #\Tab #\Return) response))
         ;; Find the first { and last }
         (start (position #\{ trimmed))
         (end (position #\} trimmed :from-end t)))
    (if (and start end (< start end))
        (subseq trimmed start (1+ end))
        trimmed)))

(defun parse-iso-date (date-string)
  "Parse an ISO 8601 date string into a local-time timestamp.
   Handles formats like '2026-01-20' and '2026-01-20T14:00:00'.
   Date-only strings are interpreted as midnight in LOCAL time (not UTC).
   Returns NIL if parsing fails or date-string is nil/empty."
  (when (and date-string (stringp date-string) (> (length date-string) 0))
    (handler-case
        (let ((trimmed (string-trim '(#\Space #\Newline #\Tab) date-string)))
          (cond
            ;; Full ISO with time - parse as-is
            ((find #\T trimmed)
             (lt:parse-timestring trimmed))
            ;; Date only - parse as midnight LOCAL time
            ((= (length trimmed) 10)
             (let ((year (parse-integer trimmed :start 0 :end 4))
                   (month (parse-integer trimmed :start 5 :end 7))
                   (day (parse-integer trimmed :start 8 :end 10)))
               (lt:encode-timestamp 0 0 0 0 day month year
                                    :timezone lt:*default-timezone*)))
            (t nil)))
      (error (e)
        (llog:debug "Failed to parse date"
                    :date-string date-string
                    :error (format nil "~A" e))
        nil))))

(defun json-null-to-nil (value)
  "Convert jzon's 'null symbol to CL nil."
  (if (eq value 'null) nil value))

(defun parse-location-info (location-data)
  "Parse location data from the LLM response into a plist.
   Returns a plist with :name :address :phone :map-url :website, or NIL if no location."
  (when (and location-data (hash-table-p location-data))
    (let ((name (json-null-to-nil (gethash "name" location-data)))
          (address (json-null-to-nil (gethash "address" location-data)))
          (phone (json-null-to-nil (gethash "phone" location-data)))
          (map-url (json-null-to-nil (gethash "map_url" location-data)))
          (website (json-null-to-nil (gethash "website" location-data))))
      ;; Only return location info if at least a name exists
      (when name
        (list :name name
              :address address
              :phone phone
              :map-url map-url
              :website website)))))

(defun parse-repeat-unit (unit-string)
  "Parse a repeat unit string into a keyword. Returns nil if not valid."
  (when (and unit-string (stringp unit-string) (> (length unit-string) 0))
    (let ((lower (string-downcase unit-string)))
      (cond
        ((string= lower "day") :day)
        ((string= lower "week") :week)
        ((string= lower "month") :month)
        ((string= lower "year") :year)
        (t nil)))))

(defun parse-enrichment-response (response)
  "Parse the JSON response from the LLM enrichment.
   Returns a plist with :title, :description, :category, :priority, :location-info,
   :repeat-interval, :repeat-unit or NIL if parsing fails."
  (llog:debug "Parsing enrichment response"
              :response-length (length response)
              :response-preview (subseq response 0 (min 200 (length response))))
  (handler-case
      (let ((json-str (extract-json-from-response response)))
        (llog:debug "Extracted JSON string"
                    :json-length (length json-str)
                    :json-content json-str)
        (let ((data (jzon:parse json-str)))
          (cond ((hash-table-p data) (let* ((location-info (parse-location-info (gethash "location" data)))
                     (scheduled-date (parse-iso-date (json-null-to-nil (gethash "scheduled_date" data))))
                     (due-date (parse-iso-date (json-null-to-nil (gethash "due_date" data))))
                     (raw-repeat-interval (json-null-to-nil (gethash "repeat_interval" data)))
                     (raw-repeat-unit (json-null-to-nil (gethash "repeat_unit" data)))
                     (repeat-interval (when (and raw-repeat-interval (numberp raw-repeat-interval) (> raw-repeat-interval 0))
                                        (truncate raw-repeat-interval)))
                     (repeat-unit (when repeat-interval (parse-repeat-unit raw-repeat-unit)))
                     (extracted-url (json-null-to-nil (gethash "url" data)))
                     (list-name (json-null-to-nil (gethash "list_name" data)))
                     (list-section (json-null-to-nil (gethash "list_section" data)))
                     (raw-list-items (json-null-to-nil (gethash "list_items" data)))
                     (list-items (when (and raw-list-items (vectorp raw-list-items))
                                   (loop for item across raw-list-items
                                         when (hash-table-p item)
                                         collect (list :title (json-null-to-nil (gethash "title" item))
                                                       :section (json-null-to-nil (gethash "section" item))))))
                     (result (list :title (json-null-to-nil (gethash "task_title" data))
                                   :description (json-null-to-nil (gethash "description" data))
                                   :category (json-null-to-nil (gethash "category" data))
                                   :priority (let ((p (gethash "priority" data)))
                                              (cond
                                                ((or (string-equal p "high") (string-equal p "high") (string-equal p "A")) :high)
                                                ((or (string-equal p "medium") (string-equal p "medium") (string-equal p "B")) :medium)
                                                ((or (string-equal p "low") (string-equal p "low") (string-equal p "C")) :low)
                                                (t :medium)))
                                   :scheduled-date scheduled-date
                                   :due-date due-date
                                   :repeat-interval repeat-interval
                                   :repeat-unit repeat-unit
                                   :url extracted-url
                                   :list-name list-name
                                   :list-section list-section
                                   :list-items list-items
                                   :location-info location-info)))
                (llog:info "Successfully parsed enrichment response"
                           :title (getf result :title)
                           :category (getf result :category)
                           :priority (getf result :priority)
                           :scheduled-date (when scheduled-date
                                            (lt:format-rfc3339-timestring nil scheduled-date))
                           :due-date (when due-date
                                      (lt:format-rfc3339-timestring nil due-date))
                           :has-location (if location-info "yes" "no")
                           :repeat-interval repeat-interval
                           :repeat-unit repeat-unit
                           :url extracted-url
                           :list-name list-name
                           :list-section list-section)
                (when location-info
                  (llog:debug "Location info extracted"
                              :location-name (getf location-info :name)
                              :location-address (getf location-info :address)
                              :location-phone (getf location-info :phone)
                              :location-map-url (getf location-info :map-url)))
                result))
      (t
                (llog:warn "Parsed data is not a hash table"
                           :data-type (type-of data))
                nil))))
    (error (e)
      (llog:error "Failed to parse enrichment response"
                  :error-type (type-of e)
                  :error-message (format nil "~A" e)
                  :response response)
      nil)))

(defun enrich-todo-input (raw-title &optional raw-notes)
  "Enrich a raw TODO input string using the LLM.
   RAW-TITLE is the task title, RAW-NOTES is optional description/notes.
   Returns a plist with enriched fields, or NIL if enrichment fails or is disabled."
  (llog:info "Enrichment request received"
             :raw-title raw-title
             :raw-notes raw-notes
             :title-length (length raw-title)
             :enrichment-enabled *enrichment-enabled*
             :provider *llm-provider*)

  ;; Check preconditions - Ollama doesn't need API key, others do
  (unless (and *enrichment-enabled*
               (or (eql *llm-provider* :ollama) *llm-api-key*)
               (> (length raw-title) 0))
    (llog:debug "Enrichment skipped - preconditions not met"
                :enabled *enrichment-enabled*
                :provider *llm-provider*
                :has-key (if *llm-api-key* "yes" "no")
                :title-length (length raw-title))
    (return-from enrich-todo-input nil))

  (handler-case
      (let ((completer (make-completer)))
        (unless completer
          (llog:warn "Failed to create LLM completer" :provider *llm-provider*)
          (return-from enrich-todo-input nil))

        ;; Extract URL from title or notes and fetch metadata
        (let* ((combined-text (format nil "~A ~@[~A~]" raw-title raw-notes))
               (detected-url (extract-url-from-text combined-text))
               (url-metadata (when detected-url (fetch-url-metadata detected-url)))
               (url-context-str
                 (when url-metadata
                   (format nil "~%~%URL Content (fetched from shared link):~%URL: ~A~@[~%Page Title: ~A~]~@[~%Page Description: ~A~]"
                           (getf url-metadata :url)
                           (getf url-metadata :title)
                           (getf url-metadata :description)))))

        (let* ((user-context (load-user-context))
               (list-defs-text (format-list-definitions-for-prompt))
               (system-prompt (get-enrichment-system-prompt list-defs-text))
               (user-input (if (and raw-notes (> (length raw-notes) 0))
                               (format nil "title=\"~A\" notes=\"~A\"" raw-title raw-notes)
                               (format nil "title=\"~A\" notes=\"\"" raw-title)))
               (prompt (format nil "~A~@[~%~%User Context:~%~A~]~@[~A~]~%~%~A"
                               system-prompt user-context url-context-str user-input)))

          (llog:debug "Context status"
                      :has-user-context (if user-context "yes" "no")
                      :user-context-length (if user-context (length user-context) 0)
                      :detected-url detected-url
                      :has-url-metadata (if url-metadata "yes" "no"))

          (llog:debug "Sending API request"
                      :prompt-length (length prompt)
                      :max-tokens 2048
                      :response-format (get-json-response-format)
                      :provider *llm-provider*
                      :model *llm-model*)

          (llog:info "Calling LLM API..."
                     :provider *llm-provider*
                     :model *llm-model*
                     :user-input user-input)

          (let* ((start-time (get-internal-real-time))
                 (response (completions:get-completion completer prompt
                                                       :max-tokens 2048
                                                       :response-format (get-json-response-format)))
                 (end-time (get-internal-real-time))
                 (elapsed-ms (/ (* 1000 (- end-time start-time)) internal-time-units-per-second)))

            (llog:info "Gemini API response received"
                       :elapsed-ms elapsed-ms
                       :response-length (length response))

            (llog:debug "Raw API response"
                        :response response)

            (let ((result (parse-enrichment-response response)))
              (if result
                  (llog:info "Enrichment completed successfully"
                             :original-title raw-title
                             :original-notes raw-notes
                             :enriched-title (getf result :title)
                             :enriched-desc (getf result :description)
                             :category (getf result :category)
                             :priority (getf result :priority)
                             :elapsed-ms elapsed-ms)
                  (llog:warn "Enrichment parsing failed"
                             :raw-title raw-title
                             :response response))
              result)))))
    (error (e)
      (llog:error "Enrichment API call failed"
                  :error-type (type-of e)
                  :error-message (format nil "~A" e)
                  :raw-title raw-title)
      nil)))

(defun category-to-tag (category)
  "Convert a category string to a lowercase tag."
  (when category
    (string-downcase category)))

;;── Org-Mode Import ───────────────────────────────────────────────────────────

(defparameter *import-system-prompt-template*
  "Role: You are an Org-Mode File Parser and Task Enrichment Assistant. Your goal is to extract TODO items from an org-mode file and enrich each one.

Today's date is: ~A

Primary Objectives:
- Parse the org-mode file content and identify all TODO/action items
- SKIP any items that are marked as DONE, COMPLETED, CANCELLED, or similar completed states
- For each non-completed item, extract and enrich the information
- Return a JSON array of enriched TODO items

Output Schema (JSON):
Return ONLY a valid JSON object with a \"todos\" key containing an array. Each item in the array should have:
- task_title: (Concise, action-oriented title starting with a verb)
- description: (Any notes or body text associated with the item, or null)
- category: (One of: Work, Personal, Health, Finance, Home, Family, Shopping, Travel, Learning, Other)
- priority: (high for urgent/important, medium for important, low for can wait - infer from org priority cookies like [#A], [#B], [#C])
- scheduled_date: (ISO 8601 date from SCHEDULED: property, or null)
- due_date: (ISO 8601 date from DEADLINE: property, or null)
- list_name: (If this item belongs on a user-defined list rather than being a task, return the exact list name. Null if regular task.)
- list_section: (If list_name is set and the list has sections, the appropriate section. Null otherwise.)
- location: (Object with business/place info if mentioned, or null)
  - name: (Business or place name)
  - address: (Address if known, otherwise null)
  - phone: (Phone if known, otherwise null)
  - map_url: (Google Maps URL, or null)
  - website: (Website if known, otherwise null)
~A
Org-Mode Parsing Rules:
- TODO keywords: TODO, NEXT, WAITING, HOLD, IN-PROGRESS, STARTED, etc. are active items
- DONE keywords: DONE, COMPLETED, CANCELLED, CANCELED, ARCHIVED are completed - SKIP THESE
- Priority cookies: [#A] = high/high, [#B] = medium/medium, [#C] = low/low
- SCHEDULED: <date> means when to start working on it -> scheduled_date
- DEADLINE: <date> means when it must be done -> due_date
- Parse org dates like <2026-01-20 Mon> or <2026-01-20 Mon 14:00>
- Include any body text under a heading as the description
- Tags like :work:home: can inform category

Example Input:
* TODO [#A] Call dentist for appointment
SCHEDULED: <2026-01-20 Mon>
:PROPERTIES:
:LOCATION: Downtown Dental
:END:
Need to schedule cleaning

* DONE Buy groceries
CLOSED: [2026-01-15 Wed]

* TODO Review quarterly report
DEADLINE: <2026-01-25 Fri>
:work:

Example Output:
{\"todos\": [
  {\"task_title\": \"Call Dentist for Appointment\", \"description\": \"Need to schedule cleaning\", \"category\": \"Health\", \"priority\": \"high\", \"scheduled_date\": \"2026-01-20\", \"due_date\": null, \"location\": {\"name\": \"Downtown Dental\", \"address\": null, \"phone\": null, \"map_url\": \"https://www.google.com/maps/search/?api=1&query=Downtown+Dental\", \"website\": null}}, ; lint:suppress max-line-length
  {\"task_title\": \"Review Quarterly Report\", \"description\": null, \"category\": \"Work\", \"priority\": \"medium\", \"scheduled_date\": null, \"due_date\": \"2026-01-25\", \"location\": null} ; lint:suppress max-line-length
]}

Note: The \"Buy groceries\" item was skipped because it was marked DONE.

Respond with ONLY the JSON object, no markdown formatting or additional text.")

(defun get-import-system-prompt (&optional list-definitions-text)
  "Get the import system prompt with today's date and optional list definitions filled in."
  (format nil *import-system-prompt-template*
          (lt:format-timestring nil (lt:now)
                               :format '(:long-weekday ", " :long-month " " :day ", " :year))
          (or list-definitions-text "")))

(defun parse-import-response (response)
  "Parse the JSON response from the org-mode import.
   Returns a list of plists, each with :title, :description, :category, :priority, etc.
   Returns NIL if parsing fails."
  (llog:debug "Parsing import response"
              :response-length (length response))
  (handler-case
      (let ((json-str (extract-json-from-response response)))
        (llog:debug "Extracted JSON for import"
                    :json-length (length json-str))
        (let ((data (jzon:parse json-str)))
          (cond ((hash-table-p data) (let ((todos-array (gethash "todos" data)))
                (cond ((and todos-array (vectorp todos-array)) (let ((result nil))
                      (loop for item across todos-array
                            when (hash-table-p item)
                            do (let* ((location-info (parse-location-info (gethash "location" item)))
                                      (scheduled-date (parse-iso-date
                                                       (json-null-to-nil (gethash "scheduled_date" item))))
                                      (due-date (parse-iso-date
                                                 (json-null-to-nil (gethash "due_date" item))))
                                      (todo-data (list :title (json-null-to-nil (gethash "task_title" item))
                                                      :description (json-null-to-nil (gethash "description" item))
                                                      :category (json-null-to-nil (gethash "category" item))
                                                      :priority (let ((p (gethash "priority" item)))
                                                                  (cond
                                                                    ((string-equal p "high") :high)
                                                                    ((string-equal p "medium") :medium)
                                                                    ((string-equal p "low") :low)
                                                                    (t :medium)))
                                                      :scheduled-date scheduled-date
                                                      :due-date due-date
                                                      :list-name (json-null-to-nil (gethash "list_name" item))
                                                      :list-section (json-null-to-nil (gethash "list_section" item))
                                                      :location-info location-info)))
                                 (push todo-data result)))
                      (llog:info "Parsed import response"
                                 :num-todos (length result))
                      (nreverse result)))
      (t
                      (llog:warn "No todos array in import response")
                      nil))))
      (t
                (llog:warn "Import response is not a hash table")
                nil))))
    (error (e)
      (llog:error "Failed to parse import response"
                  :error-type (type-of e)
                  :error-message (format nil "~A" e))
      nil)))

(defun import-org-file (filename)
  "Import an org-mode file and return a list of enriched TODO plists.
   Returns NIL if import fails."
  (llog:info "Starting org-mode import" :filename filename)

  ;; Check preconditions - Ollama doesn't need API key, others do
  (unless (and *enrichment-enabled*
               (or (eql *llm-provider* :ollama) *llm-api-key*))
    (llog:warn "Import requires enrichment to be enabled with valid provider config")
    (return-from import-org-file nil))

  (handler-case
      (let ((file-content (uiop:read-file-string filename)))
        (llog:debug "Read org file"
                    :filename filename
                    :content-length (length file-content))

        (when (zerop (length file-content))
          (llog:warn "Org file is empty" :filename filename)
          (return-from import-org-file nil))

        (let ((completer (make-completer)))
          (unless completer
            (llog:warn "Failed to create LLM completer for import" :provider *llm-provider*)
            (return-from import-org-file nil))

          (let* ((user-context (load-user-context))
                 (list-defs-text (format-list-definitions-for-prompt))
                 (system-prompt (get-import-system-prompt list-defs-text))
                 (user-input (format nil "Parse and enrich the following org-mode file:~%~%~A" file-content))
                 (prompt (if user-context
                             (format nil "~A~%~%User Context:~%~A~%~%~A"
                                     system-prompt user-context user-input)
                             (format nil "~A~%~%~A" system-prompt user-input))))

            (llog:debug "Sending import request to API"
                        :prompt-length (length prompt))

            (let* ((start-time (get-internal-real-time))
                   ;; Use large token limit - org files can have many TODOs
                   ;; Each TODO item needs ~300-500 tokens in the response
                   (response (completions:get-completion completer prompt
                                                         :max-tokens 32768
                                                         :response-format (get-json-response-format)))
                   (end-time (get-internal-real-time))
                   (elapsed-ms (/ (* 1000 (- end-time start-time)) internal-time-units-per-second)))

              (llog:info "Import API response received"
                         :elapsed-ms elapsed-ms
                         :response-length (length response))

              (parse-import-response response)))))
    (error (e)
      (llog:error "Import failed"
                  :filename filename
                  :error-type (type-of e)
                  :error-message (format nil "~A" e))
      nil)))
