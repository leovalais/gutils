;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem :gutils
  :name "gutils"
  :version "0.1"
  :maintainer "Léo Valais"
  :author "Léo Valais"
  :licence "MIT"
  :description "Utilities for graphics programming."

  :depends-on (:cl-netpbm :array-operations)

  :serial t
  :components ((:file "package")
               (:file "vector")
               (:file "matrix")
               (:file "color")
               (:file "image")))
