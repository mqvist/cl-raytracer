;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:ray-tracer-asd
  (:use :cl :asdf))

(in-package :ray-tracer-asd)

(defsystem :ray-tracer
  :name "ray-tracer"
  :components ((:file "ray-tracer"
                      :depends-on ("package" "slabs" "math" "color"))
               (:file "slabs"
                      :depends-on ("package" "heap" "math"))
               (:file "math"
                      :depends-on ("package"))
               (:file "color"
                      :depends-on ("package"))
               (:file "heap"
                      :depends-on ("package"))
               (:file "package")))