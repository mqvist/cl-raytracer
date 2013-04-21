(defpackage :ray-tracer
  (:use :common-lisp )
  (:export #:render-scene))

(setf *read-default-float-format* 'double-float)
