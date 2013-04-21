(in-package :ray-tracer)

(defparameter *background-color* black)
(defparameter *ambient-color* (color 0.1 0.1 0.1))

(defparameter *scene* nil)
(defparameter *intersection-tolerance* 1e-6)
(defparameter *slab-normals* (list (vector3 1.0 0.0 0.0)
                                   (vector3 0.0 1.0 0.0)
                                   (vector3 0.0 0.0 1.0)))

(defparameter *verbose* nil)

(defstruct shape-statistics
  (shape-type nil :type symbol)
  (tests 0 :type fixnum)
  (hits 0 :type fixnum)
  (misses 0 :type fixnum))

(defstruct statistics
  (rendering-start-time -1)
  (rendering-end-time -1)
  (primary-rays 0 :type fixnum)
  (shadow-feelers 0 :type fixnum)
  (shape-statistics nil :type list))

(defparameter *statistics* (make-statistics))

(defun report-statistics (&optional (statistics *statistics*))
  (format t "Rendering time : ~F seconds~%" (calculate-elapsed-time-in-seconds
                                             (statistics-rendering-start-time statistics)
                                             (statistics-rendering-end-time statistics)))
  (format t "Primary rays   : ~A~%" (statistics-primary-rays statistics))
  (format t "Shadow feelers : ~A~%~%" (statistics-shadow-feelers statistics))
  (format t "Shape      Tests      Hits       Misses~%")
  (dolist (elem (statistics-shape-statistics statistics))
    (let ((shape-statistics (cdr elem)))
      (format t "~10A ~10A ~10A ~10A~%"
              (shape-statistics-shape-type shape-statistics)
              (shape-statistics-tests shape-statistics)
              (shape-statistics-hits shape-statistics)
              (shape-statistics-misses shape-statistics)))))

(defun calculate-elapsed-time-in-seconds (start end)
  (/ (- end start) internal-time-units-per-second))

(defun start-rendering-time-measurement ()
  (setf (statistics-rendering-start-time *statistics*) (get-internal-real-time)))

(defun end-rendering-time-measurement ()
  (setf (statistics-rendering-end-time *statistics*) (get-internal-real-time)))

(defun log-primary-ray ()
  (incf (statistics-primary-rays *statistics*)))

(defun log-shadow-feeler ()
  (incf (statistics-shadow-feelers *statistics*)))

(defun log-intersection-test (shape-type)
  (incf (shape-statistics-tests (get-shape-type-statistics shape-type))))

(defun log-intersection-miss (shape-type)
  (incf (shape-statistics-misses (get-shape-type-statistics shape-type))))

(defun log-intersection-hit (shape-type)
  (incf (shape-statistics-hits (get-shape-type-statistics shape-type))))

(defun get-shape-type-statistics (shape-type)
  (declare (symbol shape-type))
  (cdr
   (let* ((shape-statistics-list (statistics-shape-statistics *statistics*))
          (shape-statistics (assoc shape-type shape-statistics-list)))
     (if (null shape-statistics)
         ;; Create a statistics structure for this shape type and add it to the
         ;; beginning of the association list.
         (car (setf (statistics-shape-statistics *statistics*)
                    (append (list (cons shape-type
                                        (make-shape-statistics :shape-type shape-type)))
                            shape-statistics-list)))
         shape-statistics))))

(defstruct object
  shape
  (color (color 1.0 0.0 0.0) :type color)
  (ambient-coefficient 0.1 :type double-float))

(defstruct sphere
  (location (vector3 0.0 0.0 0.0) :type vector3)
  (radius 1.0 :type double-float))

(defstruct plane
  (normal (vector3 0.0 1.0 0.0) :type vector3)
  (distance-from-origin 0.0 :type double-float))

(defstruct camera
  (location (vector3 0.0 0.0 0.0)       :type vector3)
  (viewing-vector (vector3 0.0 0.0 0.0) :type vector3)
  (right-vector (vector3 0.0 0.0 0.0)   :type vector3)
  (up-vector (vector3 0.0 0.0 0.0)      :type vector3))

(defstruct isect
  (distance 0.0 :type double-float)
  (location (vector3 0.0 0.0 0.0) :type vector3)
  object)

(defstruct light
  (location (vector3 0.0 0.0 0.0) :type vector3)
  (color (color) :type color))

(defstruct scene
  objects
  (unbound-objects nil)
  (bound-hierarchy nil)
  lights)

(defun look-at (location look-at-point up-vector focal-length)
  (let* ((viewing-vector (vec3-scale
                          (vec3-normalize
                           (vec3-sub look-at-point location))
                          focal-length))
         (right-vector (vec3-cross viewing-vector up-vector))
         (up-vector (vec3-cross right-vector viewing-vector)))
    (make-camera :location location
                 :viewing-vector viewing-vector
                 :right-vector right-vector
                 :up-vector up-vector)))

(defun render-scene (*scene* camera image-width image-height &optional (build-bound-hierarchy t))
  (declare (type fixnum image-width image-height))
  (let ((image (make-array (list image-width image-height) :element-type 'color))
        (primary-ray (make-ray))
        (*statistics* (make-statistics)))
    (declare (type (array color (* *)) image))
    (prepare-scene *scene* build-bound-hierarchy)
    (start-rendering-time-measurement)
    (dotimes (y image-height)
      (when (and *verbose* (= (mod y 10) 0))
        (format t "Rendering line ~A~%" y))
      (dotimes (x image-width)
        (declare (inline make-primary-ray))
        (update-primary-ray primary-ray camera x y image-width image-height)
        (log-primary-ray)
        (setf (aref image x y)
              (shade-intersection
               (closest-intersection
                (intersect-scene-objects primary-ray))
               (scene-lights *scene*)))))
    (end-rendering-time-measurement)
    (report-statistics)
    image))

(defun prepare-scene (scene build-bound-hierarchy)
  (cond (build-bound-hierarchy
         (format t "Building bound hierarchy... ")
         (multiple-value-bind (bound-hierarchy unbound-objects)
             (build-bound-hierarchy (scene-objects scene) *slab-normals*)
           (format t "Done~%")
           (setf (scene-bound-hierarchy scene) bound-hierarchy)
           (setf (scene-unbound-objects scene) unbound-objects)))
        (t
         (setf (scene-bound-hierarchy scene) nil)
         (setf (scene-unbound-objects scene) (scene-objects scene)))))

(defun closest-intersection (intersection-list)
  (first (sort intersection-list #'< :key #'isect-distance)))

(defun intersect-scene-objects (ray)
  (remove nil (cons (when (scene-bound-hierarchy *scene*)
                      (intersect-bound-hierarchy (scene-bound-hierarchy *scene*) *slab-normals* ray))
                    (intersect-object-list (scene-unbound-objects *scene*) ray))))

(defun intersect-object-list (objects ray)
  (mapcar #'(lambda (obj) (intersect-object obj ray))
          objects))

(defun some-intersections (ray)
  (first (remove-if #'(lambda (isect) (< (isect-distance isect)
                                         (the double-float *intersection-tolerance*)))
                             (intersect-scene-objects ray))))

;; (defun some-intersections (objects ray)
;;   (declare (type list objects)
;;            (type ray ray))
;;   (some #'(lambda (obj)
;;             (let ((isect (intersect-object obj ray)))
;;               (when (and isect
;;                          (> (isect-distance isect) (the double-float *intersection-tolerance*)))
;;                 isect)))
;;         objects))

(defun intersect-object (object ray)
  (let* ((shape (object-shape object))
         (shape-type (type-of shape))
         (distance (funcall (etypecase shape
                              (sphere #'intersect-sphere)
                              (plane #'intersect-plane))
                            shape ray)))
    (log-intersection-test shape-type)
    (cond ((null distance)
           (log-intersection-miss shape-type)
           nil)
          (t
           (log-intersection-hit shape-type)
           (make-isect :distance distance
                       :location (vec3-add (ray-origin ray)
                                           (vec3-scale (ray-direction ray) distance))
                       :object object)))))

(defun update-primary-ray (ray camera image-x image-y image-width image-height)
  (declare (type ray ray)
           (type camera camera)
           (type fixnum image-x image-y image-width image-height))
  (let ((u (* (- (/ (float image-x 1.0d0) image-width) 0.5)
              (/ (float image-width 1.0d0) image-height)))
        (v (- (/ (float image-y 1.0d0) image-height) 0.5)))
    (declare (double-float u v))
    (setf (ray-origin ray) (the vector3 (camera-location camera))
          (ray-direction ray) (the vector3 (calculate-primary-ray-direction
                               (camera-viewing-vector camera)
                               (camera-right-vector camera)
                               (camera-up-vector camera)
                               u v)))))
;;               (vec3-normalize
;;                           (vec3-add
;;                            (camera-viewing-vector camera)
;;                            (vec3-add (vec3-scale (camera-right-vector camera) u)
;;                                      (vec3-scale (camera-up-vector camera) v)))))))

(defun make-primary-ray (camera image-x image-y image-width image-height)
  (declare (type camera camera)
           (type fixnum image-x image-y image-width image-height))
  (let ((u (* (- (/ image-x image-width) 0.5)
              (/ image-width image-height)))
        (v (- (/ image-y image-height) 0.5)))
    (declare (double-float u v))
    (make-ray :origin (camera-location camera)
              :direction (calculate-primary-ray-direction
                          (camera-viewing-vector camera)
                          (camera-right-vector camera)
                          (camera-up-vector camera)
                          u v))))
;;               (vec3-normalize
;;                           (vec3-add
;;                            (camera-viewing-vector camera)
;;                            (vec3-add (vec3-scale (camera-right-vector camera) u)
;;                                      (vec3-scale (camera-up-vector camera) v)))))))

(defun calculate-primary-ray-direction (viewing-direction right-vector up-vector u v)
  (declare (type vector3 viewing-direction right-vector up-vector)
           (type double-float u v)
           (values vector3))
  (vec3-normalize (vector3 (+ (v3x viewing-direction) (+ (* (v3x right-vector) u)
                                                         (* (v3x up-vector) v)))
                           (+ (v3y viewing-direction) (+ (* (v3y right-vector) u)
                                                         (* (v3y up-vector) v)))
                           (+ (v3z viewing-direction) (+ (* (v3z right-vector) u)
                                                         (* (v3z up-vector) v))))))


;; (defun intersect-sphere (sphere ray)
;;   (let* ((eye-to-sphere (vec3-sub (sphere-location sphere) (ray-origin ray)))
;;          (v (vec3-dot eye-to-sphere (ray-direction ray)))
;;          (disc (- (square (sphere-radius sphere))
;;                   (- (vec3-dot eye-to-sphere eye-to-sphere) (square v)))))
;;     (if (< disc 0)
;;         nil
;;         (make-isect :location (vec3-add (ray-origin ray)
;;                                         (vec3-scale (ray-direction ray) (- v (sqrt disc))))
;;                     :sphere sphere))))

(defun intersect-sphere (sphere ray)
  (let* ((ray-origin->sphere-location (vec3-sub (sphere-location sphere) (ray-origin ray)))
         (sphere-distance-squared (vec3-dot ray-origin->sphere-location ray-origin->sphere-location))
         (radius-squared (* (sphere-radius sphere) (sphere-radius sphere)))
         (test (- sphere-distance-squared radius-squared))
         (l (vec3-dot ray-origin->sphere-location (ray-direction ray))))
    (if (and (> test 0) (< l 0))
        ;; Outside of sphere and facing away.
        nil
        (let ((half-chord-squared (+ (- radius-squared sphere-distance-squared) (* l l))))
          (cond ((< half-chord-squared 0)
                 ;; Ray misses.
                 nil)
                ((< test 0)
                 ;; Ray origin is inside (can't miss).
                 (+ l (sqrt half-chord-squared)))
                (t
                 ;; Ray origin is outside and ray hits.
                 (- l (sqrt half-chord-squared))))))))

(defun make-sphere-slabs (sphere slab-normals)
  (make-slab-array (mapcar #'(lambda (slab-normal)
                               (make-sphere-slab sphere slab-normal))
                           slab-normals)))

(defun make-sphere-slab (sphere slab-normal)
  (let ((projected-location (vec3-dot slab-normal (sphere-location sphere)))
        (radius (sphere-radius sphere)))
    (make-slab :near (- projected-location radius)
               :far (+ projected-location radius))))

(defun intersect-plane (plane ray)
  (let* ((cos-ray-normal-angle (vec3-dot (ray-direction ray) (plane-normal plane)))
         (ray-origin-projected-on-plane-normal (vec3-dot (ray-origin ray) (plane-normal plane)))
         (ray-origin-distance-from-plane (- ray-origin-projected-on-plane-normal
                                            (plane-distance-from-origin plane))))
    (if (= cos-ray-normal-angle 0.0) ; Is ray parallel to the plane?
        nil
        (let ((distance (/ (- ray-origin-distance-from-plane) cos-ray-normal-angle)))
          (if (< distance 0.0)
              nil
              distance)))))

(defun compute-object-normal (object point)
  (let ((shape (object-shape object)))
    (typecase shape
        (sphere (compute-sphere-normal shape point))
        (plane (plane-normal shape)))))

(defun compute-sphere-normal (sphere point)
  (vec3-normalize (vec3-sub point (sphere-location sphere))))

(defun shade-intersection (intersection lights)
  (if (not intersection)
      *background-color*
      (let* ((object (isect-object intersection))
             (point (isect-location intersection))
             (normal (compute-object-normal object point)))
        (color-add
         (compute-ambient-light object)
         (color-mul (object-color object)
                    (reduce #'color-add
                            (mapcar #'(lambda (light)
                                        (compute-diffuse-light point normal light))
                                    lights)))))))

(defun compute-ambient-light (object)
  (color-scale *ambient-color* (object-ambient-coefficient object)))

(defun compute-diffuse-light (point normal light)
  (let ((cos-incident-angle (vec3-dot normal
                                      (vec3-normalize
                                       (vec3-sub (light-location light) point)))))
    ;; Is light behind or blocked by some object?
    (if (or (<= cos-incident-angle 0)   
            (light-blocked? light point))
        black
        (color-scale (light-color light) cos-incident-angle))))

(defun light-blocked? (light location)
  (declare (type light light)
           (type vector3 location))
  (let ((shadow-feeler (make-ray :origin location
                                 :direction (vec3-normalize (vec3-sub (light-location light) location)))))
    (log-shadow-feeler)
    (some-intersections shadow-feeler)))


(defun write-targa (filename image)
  (declare (type (simple-array color *) image))
  (with-open-file (out filename :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :element-type 'unsigned-byte)
    (let ((image-width (array-dimension image 0))
          (image-height (array-dimension image 1)))
      (write-targa-header out image-width image-height)
      (dotimes (y image-height)
        (dotimes (x image-width)
          (let ((color (aref image x y)))
            (write-byte (color-component-to-ubyte (color-b color)) out)
            (write-byte (color-component-to-ubyte (color-g color)) out)
            (write-byte (color-component-to-ubyte (color-r color)) out)))))))

(defun color-component-to-ubyte (component)
  (declare (type double-float component))
  (let ((clamped-component (min (max component 0.0) 1.0)))
    (multiple-value-bind (quotient remainder)
        (round (* clamped-component 255))
      (the unsigned-byte quotient))))

(defun write-targa-header (stream image-width image-height)
  (write-byte 0 stream)
  (write-byte 0 stream)
  (write-byte 2 stream)
  (write-byte 0 stream)
  (write-byte 0 stream)
  (write-byte 0 stream)
  (write-byte 0 stream)
  (write-byte 0 stream)
  (write-byte 0 stream)
  (write-byte 0 stream)
  (write-byte 0 stream)
  (write-byte 0 stream)
  (write-byte (logand image-width #x00FF) stream)
  (write-byte (floor (/ (logand image-width #xFF00) 255)) stream)
  (write-byte (logand image-height #x00FF) stream)
  (write-byte (floor (/ (logand image-height #XFF00) 255)) stream)
  (write-byte 24 stream)
  (write-byte 0 stream))

(defun test (image-width image-height &optional (bound-objects t))
  (let ((camera (look-at (vector3 2.0 2.0 20.0)
                         (vector3 0.0 0.0 0.0)
                         (vector3 0.0 1.0 0.0)
                         1.0))
        (lights (list (make-light :location (vector3 -5.0 5.0 5.0)
                                  :color (color 0.0 0.0 1.0))
                      (make-light :location (vector3 5.0 5.0 5.0)
                                  :color (color 1.0 0.0 0.0))
                      (make-light :location (vector3 0.0 5.0 -5.0)
                                  :color (color 0.0 1.0 0.0))))
        (shapes (list (make-sphere :location (vector3 -2.0 0.0 0.0) :radius 1.0)
                      (make-sphere :location (vector3 0.0 1.0 0.0) :radius 1.0)
                      (make-sphere :location (vector3 2.0 0.0 0.0) :radius 1.0)
                      (make-plane :distance-from-origin -1.0))))
    (write-targa "/Users/mika/projects/lisp/ray-tracer/test.tga"
                 (render-scene (make-scene :objects (mapcar #'(lambda (shape) (make-object :shape shape :color white)) shapes)
                                           :lights lights)
                               camera image-width image-height bound-objects))))

(defun test-2 (image-width image-height &optional (bound-objects t))
  (let ((camera (look-at (vector3 8.0 2.0 5.0)
                         (vector3 0.0 0.0 0.0)
                         (vector3 0.0 1.0 0.0)
                         1.0))
        (lights (list (make-light :location (vector3 -5.0 5.0 5.0)
                                  :color (color 0.0 0.0 1.0))
                      (make-light :location (vector3 5.0 5.0 5.0)
                                  :color (color 1.0 0.0 0.0))
                      (make-light :location (vector3 0.0 5.0 -5.0)
                                  :color (color 0.0 1.0 0.0))))
        (shapes (let ((l nil))
                  (dotimes (row 10)
                    (dotimes (col 10)
                      (setq l (cons (make-sphere :location (vector3 (+ -5.0 col) (+ -5.0 row) 0.0)
                                                 :radius 0.5)
                                    l))))
                  l)))
    (write-targa "/Users/mika/projects/lisp/ray-tracer/test.tga"
                 (render-scene (make-scene :objects (mapcar #'(lambda (shape) (make-object :shape shape :color white)) shapes)
                                           :lights lights)
                               camera image-width image-height bound-objects))))
