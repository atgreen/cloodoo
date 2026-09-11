;;; theme.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Colour theme support.  A theme maps the 16 ANSI palette slots to
;;; concrete colors.  The default "terminal" theme passes the plain ANSI
;;; codes through, so the user's terminal scheme decides the actual
;;; colors.  Hex-palette themes resolve to truecolor or 256-color SGR
;;; codes depending on terminal capability, and degrade to the plain
;;; ANSI slot on 16-color terminals.  The "mono" theme maps every slot
;;; to no color at all, which is also how NO_COLOR is honored.

(in-package #:cloodoo)

;;── Palette Slots ─────────────────────────────────────────────────────────────

(defparameter +ansi-fg-codes+
  '((:black . "30") (:red . "31") (:green . "32") (:yellow . "33")
    (:blue . "34") (:magenta . "35") (:cyan . "36") (:white . "37")
    (:bright-black . "90") (:bright-red . "91") (:bright-green . "92")
    (:bright-yellow . "93") (:bright-blue . "94") (:bright-magenta . "95")
    (:bright-cyan . "96") (:bright-white . "97"))
  "Plain ANSI foreground SGR codes for each palette slot.")

(defparameter +ansi-bg-codes+
  '((:black . "40") (:red . "41") (:green . "42") (:yellow . "43")
    (:blue . "44") (:magenta . "45") (:cyan . "46") (:white . "47")
    (:bright-black . "100") (:bright-red . "101") (:bright-green . "102")
    (:bright-yellow . "103") (:bright-blue . "104") (:bright-magenta . "105")
    (:bright-cyan . "106") (:bright-white . "107"))
  "Plain ANSI background SGR codes for each palette slot.")

;;── Theme Definitions ─────────────────────────────────────────────────────────

(defstruct color-theme
  name      ; string key stored in settings
  label     ; display name for the status message
  palette)  ; :passthrough, :none, or alist of (slot . hex-string)

(defparameter *themes*
  (list
   (make-color-theme
    :name "terminal" :label "Terminal" :palette :passthrough)
   (make-color-theme
    :name "catppuccin-mocha" :label "Catppuccin Mocha"
    :palette '((:black . "#45475a") (:red . "#f38ba8") (:green . "#a6e3a1")
               (:yellow . "#f9e2af") (:blue . "#89b4fa") (:magenta . "#f5c2e7")
               (:cyan . "#94e2d5") (:white . "#bac2de")
               (:bright-black . "#7f849c") (:bright-red . "#f38ba8")
               (:bright-green . "#a6e3a1") (:bright-yellow . "#f9e2af")
               (:bright-blue . "#89b4fa") (:bright-magenta . "#f5c2e7")
               (:bright-cyan . "#94e2d5") (:bright-white . "#a6adc8")))
   (make-color-theme
    :name "dracula" :label "Dracula"
    :palette '((:black . "#21222c") (:red . "#ff5555") (:green . "#50fa7b")
               (:yellow . "#f1fa8c") (:blue . "#bd93f9") (:magenta . "#ff79c6")
               (:cyan . "#8be9fd") (:white . "#f8f8f2")
               (:bright-black . "#6272a4") (:bright-red . "#ff6e6e")
               (:bright-green . "#69ff94") (:bright-yellow . "#ffffa5")
               (:bright-blue . "#d6acff") (:bright-magenta . "#ff92df")
               (:bright-cyan . "#a4ffff") (:bright-white . "#ffffff")))
   (make-color-theme
    :name "nord" :label "Nord"
    :palette '((:black . "#3b4252") (:red . "#bf616a") (:green . "#a3be8c")
               (:yellow . "#ebcb8b") (:blue . "#81a1c1") (:magenta . "#b48ead")
               (:cyan . "#88c0d0") (:white . "#e5e9f0")
               (:bright-black . "#616e88") (:bright-red . "#bf616a")
               (:bright-green . "#a3be8c") (:bright-yellow . "#ebcb8b")
               (:bright-blue . "#81a1c1") (:bright-magenta . "#b48ead")
               (:bright-cyan . "#8fbcbb") (:bright-white . "#eceff4")))
   (make-color-theme
    :name "gruvbox-dark" :label "Gruvbox Dark"
    :palette '((:black . "#282828") (:red . "#cc241d") (:green . "#98971a")
               (:yellow . "#d79921") (:blue . "#458588") (:magenta . "#b16286")
               (:cyan . "#689d6a") (:white . "#a89984")
               (:bright-black . "#928374") (:bright-red . "#fb4934")
               (:bright-green . "#b8bb26") (:bright-yellow . "#fabd2f")
               (:bright-blue . "#83a598") (:bright-magenta . "#d3869b")
               (:bright-cyan . "#8ec07c") (:bright-white . "#ebdbb2")))
   (make-color-theme
    :name "solarized-light" :label "Solarized Light"
    :palette '((:black . "#073642") (:red . "#dc322f") (:green . "#859900")
               (:yellow . "#b58900") (:blue . "#268bd2") (:magenta . "#d33682")
               (:cyan . "#2aa198") (:white . "#eee8d5")
               (:bright-black . "#93a1a1") (:bright-red . "#cb4b16")
               (:bright-green . "#859900") (:bright-yellow . "#b58900")
               (:bright-blue . "#268bd2") (:bright-magenta . "#6c71c4")
               (:bright-cyan . "#2aa198") (:bright-white . "#fdf6e3")))
   (make-color-theme
    :name "mono" :label "Monochrome" :palette :none))
  "Available themes, in T-key cycling order.")

;;── Active Theme State ────────────────────────────────────────────────────────

(defvar *current-theme* nil
  "The active color-theme struct.")

(defvar *theme-fg-table* (make-hash-table :test 'eq)
  "Resolved SGR foreground code per palette slot for the active theme.")

(defvar *theme-bg-table* (make-hash-table :test 'eq)
  "Resolved SGR background code per palette slot for the active theme.")

(defun theme-fg (slot)
  "Return the active theme's foreground SGR code for SLOT, or NIL (no color)."
  (gethash slot *theme-fg-table*))

(defun theme-bg (slot)
  "Return the active theme's background SGR code for SLOT, or NIL (no color)."
  (gethash slot *theme-bg-table*))

(defun find-theme (name)
  "Find a theme by NAME, defaulting to the terminal theme."
  (or (find name *themes* :key #'color-theme-name :test #'equal)
      (first *themes*)))

(defun %resolve-hex (hex foreground)
  "Resolve HEX to the best SGR code the terminal supports, or NIL on
   16-color terminals so the caller can fall back to the plain ANSI slot."
  (case (tui:detect-color-support)
    (:truecolor (tui:parse-hex-color hex :foreground foreground))
    (:256color (tui:color-256 (tui:hex-to-ansi256 hex) :foreground foreground))
    (otherwise nil)))

(defun activate-theme (name)
  "Make the theme named NAME active by resolving its palette into the
   fg/bg lookup tables.  Unknown names activate the terminal theme."
  (let ((theme (find-theme name)))
    (clrhash *theme-fg-table*)
    (clrhash *theme-bg-table*)
    (let ((palette (color-theme-palette theme)))
      (unless (eql palette :none)
        (loop for (slot . ansi-fg) in +ansi-fg-codes+
              for ansi-bg = (cdr (assoc slot +ansi-bg-codes+))
              for hex = (and (listp palette) (cdr (assoc slot palette)))
              do (setf (gethash slot *theme-fg-table*)
                       (or (and hex (%resolve-hex hex t)) ansi-fg))
                 (setf (gethash slot *theme-bg-table*)
                       (or (and hex (%resolve-hex hex nil)) ansi-bg)))))
    (setf *current-theme* theme)
    theme))

(defun no-color-requested-p ()
  "True when the NO_COLOR environment variable is present and non-empty."
  (let ((no-color (uiop:getenv "NO_COLOR")))
    (and no-color (plusp (length no-color)))))

(defun load-theme-setting ()
  "Activate the saved theme (or the default).  NO_COLOR forces monochrome."
  (if (no-color-requested-p)
      (activate-theme "mono")
      (activate-theme (or (ignore-errors (db-load-setting "theme"))
                          "terminal"))))

(defun cycle-theme ()
  "Activate, persist, and return the next theme in *themes*."
  (let* ((pos (or (position *current-theme* *themes*) 0))
         (next (nth (mod (1+ pos) (length *themes*)) *themes*)))
    (activate-theme (color-theme-name next))
    (ignore-errors (db-save-setting "theme" (color-theme-name next)))
    next))

;; Default to the terminal theme so colors work before any DB is available.
(activate-theme "terminal")
