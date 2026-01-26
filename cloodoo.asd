;;; cloodoo.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(asdf:defsystem #:cloodoo
  :description "Personal TODO system with a retro TUI"
  :author      "Anthony Green <green@moxielogic.com>"
  :license     "MIT"
  :version     "0.1.0"
  :depends-on (:tuition
               :com.inuoe.jzon
               :local-time
               :str
               :clingon
               :alexandria
               :hunchentoot
               :easy-routes
               :version-string
               :completions
               :cl-dotenv
               :llog
               :bordeaux-threads
               :sqlite
               :md5
               :dexador
               :ag-grpc
               :ag-proto
               :cl-x509
               :cl-qrencode)
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "model")
                             (:file "grpc-proto")
                             (:file "storage")
                             (:file "db")
                             (:file "certs")
                             (:file "enrich")
                             (:file "update")
                             (:file "components")
                             (:file "view")
                             (:file "server")
                             (:file "sync")
                             (:file "cli")
                             (:file "main"))))
  :build-operation "program-op"
  :build-pathname "cloodoo"
  :entry-point "cloodoo:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
