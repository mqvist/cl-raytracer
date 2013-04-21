(in-package :ray-tracer)

(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0)))
(proclaim '(inline color color-add color-mul color-scale))

(defstruct (color (:constructor color (r g b)))
  (r 0.0 :type double-float)
  (g 0.0 :type double-float)
  (b 0.0 :type double-float))

(defun color-add (c1 c2)
  (declare (color c1 c2))
  (color (+ (color-r c1) (color-r c2))
         (+ (color-g c1) (color-g c2))
         (+ (color-b c1) (color-b c2))))

(defun color-mul (c1 c2)
  (declare (color c1 c2))
  (color (* (color-r c1) (color-r c2))
         (* (color-g c1) (color-g c2))
         (* (color-b c1) (color-b c2))))

(defun color-scale (color scalar)
  (declare (color color)
           (double-float scalar))
  (color (* (color-r color) scalar)
         (* (color-g color) scalar)
         (* (color-b color) scalar)))

(defparameter white (color 1.0 1.0 1.0))
(defparameter black (color 0.0 0.0 0.0))

