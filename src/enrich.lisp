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
    (if (probe-file config-file)
        (handler-case
            (with-open-file (in config-file :direction :input)
              (let ((config (read in nil nil)))
                (llog:info "Loaded config from file" :path (namestring config-file))
                config))
          (error (e)
            (llog:warn "Failed to read config file, using defaults" :error (format nil "~A" e))
            (default-config)))
        (progn
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
    ((eq *llm-provider* :ollama)
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

(defparameter *enrichment-system-prompt-template*
  "Role: You are a Task Optimization Assistant. Your goal is to transform rough, fragmented user input into structured, actionable, and categorized TODO items.

Today's date is: ~A

Primary Objectives:
- Clarification: Expand abbreviations and fix ALL typos in both title and notes.
- Enrichment: Assign a logical category, priority level, and estimated time requirement.
- Date Extraction: Parse any date/time references into scheduled_date and due_date.
- Contextualization: Make the task title concise and action-oriented.
- Location Awareness: When a business, place, or location is mentioned, extract useful contact info.

Output Schema (JSON):
Return ONLY a valid JSON object with these keys:
- task_title: (Concise, action-oriented title starting with a verb)
- description: (Clean up any typos in the user's notes, or null if no notes provided)
- category: (One of: Work, Personal, Health, Finance, Home, Family, Shopping, Travel, Learning, Other)
- priority: (P1 for urgent/important, P2 for important, P3 for can wait)
- estimated_minutes: (Integer estimate)
- scheduled_date: (ISO 8601 date when task should be worked on, e.g. \"2026-01-20\" or \"2026-01-20T14:00:00\", or null)
- due_date: (ISO 8601 date when task must be completed by, e.g. \"2026-01-25\", or null)
- location: (Object with business/place info, or null if no location mentioned)
  - name: (Business or place name, properly formatted)
  - address: (Full street address if known or can be inferred, otherwise null)
  - phone: (Phone number if you know it, otherwise null)
  - map_url: (Google Maps search URL in format: https://www.google.com/maps/search/?api=1&query=URL+Encoded+Name+and+City)
  - website: (Business website URL if you know it, otherwise null)

Date Extraction Rules:
- SCHEDULED (scheduled_date): When user mentions doing something ON a date (\"dentist on tuesday\", \"meeting friday 3pm\")
- DEADLINE (due_date): When user mentions something is DUE BY a date (\"report due friday\", \"deadline jan 20\")
- Parse relative dates like \"tomorrow\", \"next tuesday\", \"this friday\" using today's date
- Include time if mentioned (\"3pm\" -> \"T15:00:00\")
- If only a day is mentioned without context, assume it's the SCHEDULED date
- If a date seems like both (appointment), use scheduled_date

Rules for Processing:
- Action Verbs: Always start the task_title with a verb (Schedule, Buy, Call, Fix, Review, etc.)
- Fix ALL typos and spelling errors in both title and description/notes
- Smart Defaults: If no time is mentioned, estimate based on task type (Email = 5 mins, Meeting = 30 mins, Project = 60 mins)
- Keep descriptions brief and professional
- Location Detection: If the task mentions a business, restaurant, doctor, store, venue, or any physical location, populate the location object with as much info as you can provide
- Phone Calls: If the task involves calling someone (title contains \"call\" or implies phone contact), include the phone number in the description field if you know it, but do NOT guess why the user is calling - just provide the number (e.g., \"Phone: (555) 123-4567\")

Examples:
Input: title=\"dentist next tues\" notes=\"dr tam @ lawernce dentust\" -> {\"task_title\": \"Schedule Dentist Appointment\", \"description\": \"Dr. Tam @ Lawrence Dentist\", \"category\": \"Health\", \"priority\": \"P2\", \"estimated_minutes\": 60, \"scheduled_date\": \"2026-01-21\", \"due_date\": null, \"location\": {\"name\": \"Lawrence Family Dentist\", \"address\": null, \"phone\": null, \"map_url\": \"https://www.google.com/maps/search/?api=1&query=Lawrence+Family+Dentist\", \"website\": null}}
Input: title=\"report due friday\" notes=\"quarterly sales\" -> {\"task_title\": \"Complete Quarterly Sales Report\", \"description\": \"Quarterly sales\", \"category\": \"Work\", \"priority\": \"P1\", \"estimated_minutes\": 120, \"scheduled_date\": null, \"due_date\": \"2026-01-24\", \"location\": null}
Input: title=\"dinner at joes pizza\" notes=\"6pm friday\" -> {\"task_title\": \"Dinner at Joe's Pizza\", \"description\": null, \"category\": \"Personal\", \"priority\": \"P2\", \"estimated_minutes\": 90, \"scheduled_date\": \"2026-01-24T18:00:00\", \"due_date\": null, \"location\": {\"name\": \"Joe's Pizza\", \"address\": null, \"phone\": null, \"map_url\": \"https://www.google.com/maps/search/?api=1&query=Joe%27s+Pizza\", \"website\": null}}
Input: title=\"call dr smith\" notes=\"\" -> {\"task_title\": \"Call Dr. Smith\", \"description\": null, \"category\": \"Health\", \"priority\": \"P2\", \"estimated_minutes\": 10, \"scheduled_date\": null, \"due_date\": null, \"location\": null}

Respond with ONLY the JSON object, no markdown formatting or additional text.")

(defun get-enrichment-system-prompt ()
  "Get the enrichment system prompt with today's date filled in."
  (format nil *enrichment-system-prompt-template*
          (lt:format-timestring nil (lt:now)
                               :format '(:long-weekday ", " :long-month " " :day ", " :year))))

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

(defun parse-enrichment-response (response)
  "Parse the JSON response from the LLM enrichment.
   Returns a plist with :title, :description, :category, :priority, :estimated-minutes, :location-info
   or NIL if parsing fails."
  (llog:debug "Parsing enrichment response"
              :response-length (length response)
              :response-preview (subseq response 0 (min 200 (length response))))
  (handler-case
      (let ((json-str (extract-json-from-response response)))
        (llog:debug "Extracted JSON string"
                    :json-length (length json-str)
                    :json-content json-str)
        (let ((data (jzon:parse json-str)))
          (if (hash-table-p data)
              (let* ((location-info (parse-location-info (gethash "location" data)))
                     (scheduled-date (parse-iso-date (json-null-to-nil (gethash "scheduled_date" data))))
                     (due-date (parse-iso-date (json-null-to-nil (gethash "due_date" data))))
                     (result (list :title (json-null-to-nil (gethash "task_title" data))
                                   :description (json-null-to-nil (gethash "description" data))
                                   :category (json-null-to-nil (gethash "category" data))
                                   :priority (let ((p (gethash "priority" data)))
                                              (cond
                                                ((string-equal p "P1") :high)
                                                ((string-equal p "P2") :medium)
                                                ((string-equal p "P3") :low)
                                                (t :medium)))
                                   :estimated-minutes (let ((m (gethash "estimated_minutes" data)))
                                                       (if (numberp m) (truncate m) 15))
                                   :scheduled-date scheduled-date
                                   :due-date due-date
                                   :location-info location-info)))
                (llog:info "Successfully parsed enrichment response"
                           :title (getf result :title)
                           :category (getf result :category)
                           :priority (getf result :priority)
                           :estimated-minutes (getf result :estimated-minutes)
                           :scheduled-date (when scheduled-date
                                            (lt:format-rfc3339-timestring nil scheduled-date))
                           :due-date (when due-date
                                      (lt:format-rfc3339-timestring nil due-date))
                           :has-location (if location-info "yes" "no"))
                (when location-info
                  (llog:debug "Location info extracted"
                              :location-name (getf location-info :name)
                              :location-address (getf location-info :address)
                              :location-phone (getf location-info :phone)
                              :location-map-url (getf location-info :map-url)))
                result)
              (progn
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
               (or (eq *llm-provider* :ollama) *llm-api-key*)
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

        (let* ((user-context (load-user-context))
               (system-prompt (get-enrichment-system-prompt))
               (user-input (if (and raw-notes (> (length raw-notes) 0))
                               (format nil "title=\"~A\" notes=\"~A\"" raw-title raw-notes)
                               (format nil "title=\"~A\" notes=\"\"" raw-title)))
               (prompt (if user-context
                           (format nil "~A~%~%User Context (use this to better understand the user):~%~A~%~%User input: ~A"
                                   system-prompt user-context user-input)
                           (format nil "~A~%~%User input: ~A" system-prompt user-input))))

          (llog:debug "User context status"
                      :has-context (if user-context "yes" "no")
                      :context-length (if user-context (length user-context) 0))

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
                             :estimated-minutes (getf result :estimated-minutes)
                             :elapsed-ms elapsed-ms)
                  (llog:warn "Enrichment parsing failed"
                             :raw-title raw-title
                             :response response))
              result))))
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
- priority: (P1 for urgent/important, P2 for important, P3 for can wait - infer from org priority cookies like [#A], [#B], [#C])
- estimated_minutes: (Integer estimate based on task complexity)
- scheduled_date: (ISO 8601 date from SCHEDULED: property, or null)
- due_date: (ISO 8601 date from DEADLINE: property, or null)
- location: (Object with business/place info if mentioned, or null)
  - name: (Business or place name)
  - address: (Address if known, otherwise null)
  - phone: (Phone if known, otherwise null)
  - map_url: (Google Maps URL, or null)
  - website: (Website if known, otherwise null)

Org-Mode Parsing Rules:
- TODO keywords: TODO, NEXT, WAITING, HOLD, IN-PROGRESS, STARTED, etc. are active items
- DONE keywords: DONE, COMPLETED, CANCELLED, CANCELED, ARCHIVED are completed - SKIP THESE
- Priority cookies: [#A] = P1/high, [#B] = P2/medium, [#C] = P3/low
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
  {\"task_title\": \"Call Dentist for Appointment\", \"description\": \"Need to schedule cleaning\", \"category\": \"Health\", \"priority\": \"P1\", \"estimated_minutes\": 10, \"scheduled_date\": \"2026-01-20\", \"due_date\": null, \"location\": {\"name\": \"Downtown Dental\", \"address\": null, \"phone\": null, \"map_url\": \"https://www.google.com/maps/search/?api=1&query=Downtown+Dental\", \"website\": null}},
  {\"task_title\": \"Review Quarterly Report\", \"description\": null, \"category\": \"Work\", \"priority\": \"P2\", \"estimated_minutes\": 60, \"scheduled_date\": null, \"due_date\": \"2026-01-25\", \"location\": null}
]}

Note: The \"Buy groceries\" item was skipped because it was marked DONE.

Respond with ONLY the JSON object, no markdown formatting or additional text.")

(defun get-import-system-prompt ()
  "Get the import system prompt with today's date filled in."
  (format nil *import-system-prompt-template*
          (lt:format-timestring nil (lt:now)
                               :format '(:long-weekday ", " :long-month " " :day ", " :year))))

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
          (if (hash-table-p data)
              (let ((todos-array (gethash "todos" data)))
                (if (and todos-array (vectorp todos-array))
                    (let ((result nil))
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
                                                                    ((string-equal p "P1") :high)
                                                                    ((string-equal p "P2") :medium)
                                                                    ((string-equal p "P3") :low)
                                                                    (t :medium)))
                                                      :estimated-minutes (let ((m (gethash "estimated_minutes" item)))
                                                                          (if (numberp m) (truncate m) 15))
                                                      :scheduled-date scheduled-date
                                                      :due-date due-date
                                                      :location-info location-info)))
                                 (push todo-data result)))
                      (llog:info "Parsed import response"
                                 :num-todos (length result))
                      (nreverse result))
                    (progn
                      (llog:warn "No todos array in import response")
                      nil)))
              (progn
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
               (or (eq *llm-provider* :ollama) *llm-api-key*))
    (llog:warn "Import requires enrichment to be enabled with valid provider config")
    (return-from import-org-file nil))

  (handler-case
      (let ((file-content (uiop:read-file-string filename)))
        (llog:debug "Read org file"
                    :filename filename
                    :content-length (length file-content))

        (when (= (length file-content) 0)
          (llog:warn "Org file is empty" :filename filename)
          (return-from import-org-file nil))

        (let ((completer (make-completer)))
          (unless completer
            (llog:warn "Failed to create LLM completer for import" :provider *llm-provider*)
            (return-from import-org-file nil))

          (let* ((user-context (load-user-context))
                 (system-prompt (get-import-system-prompt))
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
