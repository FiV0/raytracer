(in-package #:raytracer)

(defconstant eps 1e-6)

(defun sq (x) (* x x))

(defun mag (v)
  (sqrt (+ (sq (x v)) (sq (y v)) (sq (z v)))))

;; (defstruct (vector (:print-function print-vector))
;;   x y z)
;;
;; (defun print-vector (v stream depth)
;;   (declare (ignore depth))
;;   (format stream "#v(~A,~A,~A)" (vector-x v) (vector-y) (vector-y)))

(defun unit-vector (v)
  (let ((d (mag v)))
    (if (/= d 0)
      (make-point :x (/ (x v) d) :y (/ (y v) d) :z (/ (z v) d))
      (make-point :x 0 :y 0 :z 0))))

(defstruct (point (:conc-name nil))
  x y z)

(defun construct-point (x y z)
  (make-point :x x :y y :z z))

(defun point-equal (p1 p2)
  (and (< (- (x p1) (x p2) eps))
       (< (- (y p1) (y p2) eps))
       (< (- (z p1) (z p2) eps))))

(defun same-direction (v1 v2)
  (point-equal (unit-vector v1) (unit-vector v2)))

(defun distance (p1 p2)
  (mag (sub-point p1 p2)))

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

(defmacro mvprint (output-stream &rest rest)
  `(progn
     ,@(mapcar #'(lambda (x)
                   `(let ((lst (multiple-value-list ,x)))
                      (mapcar #'(lambda (y)
                                  (print y ,output-stream))
                              lst)))
               rest)
     (values)))

(defun mvprint-test ()
  (mvprint 1 (values 2 3) 4))





