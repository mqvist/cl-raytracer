(in-package :ray-tracer)

;; (declaim (optimize (speed 3) (safety 0) (debug 0) (space 0)))
;; (proclaim '(inline vector3 vec3-add vec3-sub vec3-dot vec3-cross
;;             vec3-scale vec3-normalize square make-ray))

(defstruct (vector3 (:conc-name v3)
                    (:constructor vector3 (x y z)))
  (x 0.0 :type double-float)
  (y 0.0 :type double-float)
  (z 0.0 :type double-float))

(defun vec3-add (v1 v2)
  (declare (type vector3 v1 v2))
  (vector3 (+ (v3x v1) (v3x v2))
           (+ (v3y v1) (v3y v2))
           (+ (v3z v1) (v3z v2))))

(defun vec3-sub (v1 v2)
  (declare (type vector3 v1 v2))
  (vector3 (- (v3x v1) (v3x v2))
           (- (v3y v1) (v3y v2))
           (- (v3z v1) (v3z v2))))

(defun vec3-dot (v1 v2)
  (declare (type vector3 v1 v2)
           (values double-float))
  (the double-float (+ (* (v3x v1) (v3x v2))
                       (* (v3y v1) (v3y v2))
                       (* (v3z v1) (v3z v2)))))

(defun vec3-cross (v1 v2)
  (declare (type vector3 v1 v2))
  (vector3 (- (* (v3y v1) (v3z v2)) (* (v3z v1) (v3y v2)))
           (- (* (v3z v1) (v3x v2)) (* (v3x v1) (v3z v2)))
           (- (* (v3x v1) (v3y v2)) (* (v3y v1) (v3x v2)))))

(defun vec3-scale (vec scalar)
  (declare (type vector3 vec)
           (type double-float scalar))
  (vector3 (* (v3x vec) scalar)
           (* (v3y vec) scalar)
           (* (v3z vec) scalar)))

(defun vec3-normalize (vec)
  (declare (type vector3 vec))
  (let ((vector-length (sqrt (vec3-dot vec vec))))
    (declare (type double-float vector-length))
    (vec3-scale vec (/ 1.0 vector-length))))

(defun square (a)
  (declare (type double-float a))
  (the double-float (* a a)))

(defstruct ray
  (origin (vector3 0.0 0.0 0.0) :type vector3)
  (direction (vector3 0.0 0.0 1.0) :type vector3))

