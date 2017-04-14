(in-package #:raytracer)

(defconstant eps 1e-6)

(defun sq (x) (* x x))

(defun mag (x y z)
  (sqrt (+ (sq x) (sq y) (sq z))))

(defun unit-vector (x y z)
  (let ((d (mag x y z)))
    (if (/= d 0)
      (values (/ x d) (/ y d) (/ z d))
      (values 0 0 0))))

(defstruct (point (:conc-name nil))
  x y z)

(defun point-equal (p1 p2)
  (and (< (- (x p1) (x p2) eps))
       (< (- (y p1) (y p2) eps))
       (< (- (z p1) (z p2) eps))))

(defun same-direction (v1 v2)
  (multiple-value-bind (x1 y1 z1) (unit-vector (x v1) (y v1) (z v1))
    (multiple-value-bind (x2 y2 z2) (unit-vector (x v2) (y v2) (z v2))
      (let ((uv1 (make-point :x x1 :y y1 :z z1))
            (uv2 (make-point :x x2 :y y2 :z z2)))
        (point-equal uv1 uv2)))))

(defun distance (p1 p2)
  (mag (- (x p1) (x p2))
       (- (y p1) (y p2))
       (- (z p1) (z p2))))


(defun apply-2-points (op p1 p2)
  (make-point :x (funcall op (x p1) (x p2))
              :y (funcall op (y p1) (y p2))
              :z (funcall op (z p1) (z p2))))

(defun vec (p1 p2)
  (apply-2-points #'- p1 p2))

(defun sub-point (p1 p2)
  (vec p1 p2))

(defun add-point (p1 p2)
  (apply-2-points #'+ p1 p2))

(defun mul-vec (sca v)
  (make-point :x (* sca (x v))
              :y (* sca (y v))
              :z (* sca (z v))))

(defun neg-vec (v)
  (make-point :x (- (x v))
              :y (- (y v))
              :z (- (z v))))

(defun dot-product (v1 v2)
  (+ (* (x v1) (x v2)) (* (y v1) (y v2)) (* (y v1) (y v2))))

(defun cross-product (v1 v2)
  (let ((x1 (x v1)) (y1 (y v1)) (z1 (z v1))
        (x2 (x v2)) (y2 (y v2)) (z2 (z v2)))
    (make-point :x (- (* y1 z2) (* z1 y2))
                :y (- (* z1 x2) (* x1 z2))
                :z (- (* x1 y2) (* y1 x2)))))

(defun minroot (a b c)
  (if (zerop a)
      (/ (- c) b)
      (let ((disc (- (sq b) (* 4 a c))))
        (unless (minusp disc)
          (let ((discrt (sqrt disc)))
            (min (/ (+ (- b) discrt) (* 2 a))
                 (/ (- (- b) discrt) (* 2 a))))))))

(defmacro mvprint (&rest rest)
  `(progn
     ,@(mapcar #'(lambda (x) `(print ,x)) rest)))

;; luminosity method
(defun to-greyscale (r g b)
  (values (* 0.21 r) (* 0.72 g) (* 0.07 b)))

