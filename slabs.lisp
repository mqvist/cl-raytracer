(in-package :ray-tracer)

(defstruct candidate
  (distance-estimate 0.0 :type double-float)
  object)

(defun intersect-bound-hierarchy (root slab-normals ray)
  (declare (type bound root)
           (type list slab-normals)
           (type ray ray))
  (let* ((ray-slab-values (calculate-ray-slab-intersection-values ray slab-normals))
         (candidates (make-candidate-prioq root ray-slab-values))
         (nearest-isect-so-far nil))
    (do ((candidate (prioq-pop candidates) (prioq-pop candidates)))
        ;; Do until out of candidates or the top candidate is farther than the
        ;; nearest object hit.
        ((or (null candidate)
             (and nearest-isect-so-far
                  (> (candidate-distance-estimate candidate)
                     (isect-distance nearest-isect-so-far)))))
      (let ((obj (candidate-object candidate)))
        (cond ((object-p obj)
               ;; Candidate is leaf, i.e., obj is an object.
               (let ((intersection (intersect-object obj ray)))
                 (when (and intersection
                            (> (isect-distance intersection) (the double-float *intersection-tolerance*))
                            (or (null nearest-isect-so-far)
                                (< (isect-distance intersection)
                                   (isect-distance nearest-isect-so-far))))
                   (setq nearest-isect-so-far intersection))))
              (t
               ;; Obj is a list of bounds.
               (dolist (bound obj)
                 (declare (type bound bound))
                 (let ((distance-estimate (intersect-slabs (bound-slabs bound) ray-slab-values)))
                   (when distance-estimate
                     (prioq-insert candidates (make-candidate :distance-estimate distance-estimate
                                                              :object (bound-children bound))))))))))
    nearest-isect-so-far))

(defun calculate-ray-slab-intersection-values (ray slab-normals)
  (let ((array (make-array (length slab-normals) :element-type 'cons :initial-element (cons nil nil))))
    (dotimes (i (length slab-normals))
      (setf (aref array i)
            (cons (vec3-dot (nth i slab-normals) (ray-origin ray))
                  (/ 1.0 (vec3-dot (nth i slab-normals) (ray-direction ray))))))
    array))

(defun make-candidate-prioq (bound ray-slab-values)
  (make-prioq #'(lambda (c1 c2) (< (candidate-distance-estimate c1)
                                   (candidate-distance-estimate c2)))
              (when bound
                (let ((distance-estimate
                       (intersect-slabs (bound-slabs bound) ray-slab-values)))
                  (when distance-estimate
                    (list (make-candidate :distance-estimate distance-estimate
                                          :object (bound-children bound))))))))

(defun intersect-slabs (slabs ray-slab-values)
  (declare (type (simple-array slab *) slabs)
           (type (simple-array cons *) ray-slab-values))
  (let ((slab-near (slab-near (aref slabs 0)))
        (slab-far (slab-far (aref slabs 0)))
        (s-value (car (aref ray-slab-values 0)))
        (t-value (cdr (aref ray-slab-values 0))))
    (declare (type double-float slab-near slab-far s-value t-value))
    (let ((near (if (< t-value 0.0)
                    (* (- slab-far s-value) t-value)
                    (* (- slab-near s-value) t-value)))
          (far (if (< t-value 0.0)
                   (* (- slab-near s-value) t-value)
                   (* (- slab-far s-value) t-value))))
      (declare (type double-float near far))
      (dotimes (count (- (length slabs) 1))
        (let ((slab (aref slabs (+ count 1)))
              (value-pair (aref ray-slab-values (+ count 1))))
          (declare (type slab slab)
                   (type cons value-pair))
          (let ((slab-near (slab-near slab))
                (slab-far (slab-far slab))
                (s-value (car value-pair))
                (t-value (cdr value-pair)))
            (declare (type double-float slab-near slab-far s-value t-value))
            (let ((near-2 (if (< t-value 0.0)
                              (* (- slab-far s-value) t-value)
                              (* (- slab-near s-value) t-value)))
                  (far-2 (if (< t-value 0.0)
                             (* (- slab-near s-value) t-value)
                             (* (- slab-far s-value) t-value))))
              (declare (type double-float near-2 far-2))
              (setq near (max near near-2))
              (setq far (min far far-2))
              ;; Can we quit early?
              (when (> near far)
                 (return))
              ))))
      (cond ((or (> near far)
                 (< far 0.0))
             nil)
            ((> near 0.0)
             near)
            (t
             0.0)))))
         
;; (defun calculate-near-far-intersection-distances (near far s-value t-value)
;;   (declare (type double-float near far s-value t-value)
;;            (values double-float double-float))
;;   (if (< t-value 0.0)
;;       ;; Swap near and far.
;;       (values (* (- far s-value) t-value)
;;               (* (- near s-value) t-value))
;;       (values (* (- near s-value) t-value)
;;               (* (- far s-value) t-value))))

;;   (let* ((slabs (fix-slabs slabs ray-slab-values))
;;          (near (apply #'max (calculate-slab-intersection-distances (mapcar #'slab-near slabs) ray-slab-values)))
;;          (far (apply #'min (calculate-slab-intersection-distances (mapcar #'slab-far slabs) ray-slab-values))))
;;     (cond ((or (> near far)
;;                (< far 0.0))
;;            nil)
;;           ((> near 0.0)
;;            near)
;;           (t
;;            0.0))))


;; (defun fix-slabs (slabs ray-slab-values)
;;   (mapcar #'(lambda (slab values) (if (< (cdr values) 0.0)
;;                                       (make-slab :near (slab-far slab)
;;                                                  :far (slab-near slab))
;;                                       slab))
;;           slabs ray-slab-values))

;; (defun calculate-slab-intersection-distances (slab-distances ray-slab-values)
;;   (mapcar #'(lambda (distance values) (* (- distance (car values)) (cdr values)))
;;           slab-distances ray-slab-values))

(defparameter *bound-tree-branching-factor* 2)

(defstruct bound
  (slabs nil :type (simple-array slab *))
  children)

(defstruct slab
  (near 0.0 :type double-float)
  (far 0.0 :type double-float))

(defun make-slab-array (slabs)
  (declare (type list slabs))
  (let ((array (make-array (length slabs) :element-type 'slab :initial-element (make-slab))))
    (dotimes (i (length slabs))
      (setf (aref array i) (nth i slabs)))
    array))

(defun build-bound-hierarchy (objects slab-normals)
  (multiple-value-bind (boundable-objects unboundable-objects)
      (split-list #'object-boundable? objects)
    (values 
     (build-bound-tree (bound-objects boundable-objects slab-normals))
     unboundable-objects)))

(defun split-list (pred list)
  (values (remove-if-not pred list)
          (remove-if pred list)))

(defun get-unboundable-objects (objects)
  (remove-if-not #'(lambda (obj) (eq (type-of (object-shape obj)) 'plane))
                 objects))

(defun object-boundable? (object)
  (eq (type-of (object-shape object)) 'sphere))

(defun bound-objects (objects slab-normals)
  (mapcar #'(lambda (obj)
              (make-bound :slabs
                          (etypecase (object-shape obj)
                            (sphere (make-sphere-slabs (object-shape obj) slab-normals)))
                          :children obj))
          objects))

(defun build-bound-tree (nodes)
  (declare (type list nodes))
  (cond ((< (length nodes) 2)
         (first nodes))
        ((< (length nodes) (the fixnum *bound-tree-branching-factor*))
         (combine-bounds nodes))
        (t
         (build-bound-tree
          (mapcar #'combine-bounds
                  (group-list-elements nodes *bound-tree-branching-factor*))))))

(defun combine-bounds (bounds)
  (cond ((null bounds)
         (break))
        ((= (length bounds) 1)
         (first bounds))
        (t
         (make-bound :slabs (combine-slab-arrays (mapcar #'bound-slabs bounds))
                     :children bounds))))

(defun combine-slab-arrays (slab-arrays)
  (make-slab-array (apply #'mapcar #'combine-slabs (mapcar #'array-to-list slab-arrays))))

(defun array-to-list (array)
  (let ((list nil))
    (dotimes (i (length array))
      (setq list (cons (aref array i) list)))
    (nreverse list)))

(defun combine-slabs (&rest slabs)
  (make-slab :near (apply #'min (mapcar #'slab-near slabs))
             :far  (apply #'max (mapcar #'slab-far  slabs))))

(defun group-list-elements (list group-size)
  (if (<= (length list) group-size)
      (list list)
      (cons (subseq list 0 group-size)
            (group-list-elements (subseq list group-size) group-size))))
         

