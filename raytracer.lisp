;;;; raytracer.lisp

(in-package #:raytracer)

;;; "raytracer" goes here. Hacks and glory await!

(defstruct color r b g)

(defstruct surface color)

(defparameter *world* nil)
(defparameter *light-sources* (list (make-point :x 300 :y -2000 :z -1200)
                                    (make-point :x -10000 :y -2000 :z -1200)))

(defconstant eye (make-point :x 0 :y 0 :z 200))

(defun tracer (pathname &optional (res 1) (color nil))
  (with-open-file (p pathname :direction :output :if-exists :supersede)
    (format p "P3 ~A ~A 255" (* res 100) (* res 100))
    (let ((inc (/ res)))
      (do ((y -50 (+ y inc)))
          ((< (- 50 y) inc))
        (do ((x -50 (+ x inc)))
            ((< (- 50 x) inc))
            (multiple-value-bind (r b g)
              (if color (color-at x y) (to-greyscale (color-at x y)))
              (print r p)
              (print g p)
              (print b p)))))))

(defun color-at (x y)
  (multiple-value-bind (xr yr zr)
                       (unit-vector (- x (x eye))
                                    (- y (y eye))
                                    (- 0 (z eye)))
    (sendray eye xr yr zr)))

(defun sendray (pt xr yr zr)
  (multiple-value-bind (o int) (first-hit pt xr yr zr)
    (let ((r 0) (b 0) (g 0))
      (when int
        (dolist (light-source *light-sources*)
          (multiple-value-bind (lxr lyr lzr)
            (unit-vector (- (x int) (x light-source) )
                         (- (y int) (y light-source) )
                         (- (z int) (z light-source) ))
            (multiple-value-bind (fo fint) (first-hit light-source lxr lyr lzr)
              (when (and (not (null o)) (eq fo o) (point-equal fint int))
                (let ((lam (lambert o int lxr lyr lzr)))
                  (incf r (* lam (color-r (surface-color o))))
                  (incf b (* lam (color-b (surface-color o))))
                  (incf g (* lam (color-g (surface-color o))))))))))
      (values (round r) (round b) (round g)))))

(defun first-hit (pt xr yr zr)
  (let (surface hit dist)
    (dolist (o *world*)
      (let* ((h (intersect o pt xr yr zr)))
        (when h
          (let ((d (distance h pt)))
            (when (or (null dist) (< d dist))
              (setf surface o hit h dist d))))))
    (values surface hit)))

(defun lambert (o int xr yr zr)
  (multiple-value-bind (xn yn zn) (normal o int)
    (max 0 (+ (* xr xn) (* yr yn) (* zr zn)))))

(defstruct (sphere (:include surface))
  radius center)

(defun defsphere (x y z r c)
  (let ((s (make-sphere
             :radius r
             :center (make-point :x x :y y :z z)
             :color  c)))
    (push s *world*)
    s))

(defstruct (plane (:include surface))
  p1 p2 p3 nor)

(defun defplane (p1 p2 p3 c)
  (let* ((v1 (vec p2 p1))
         (v2 (vec p3 p1))
         (n1 (cross-product v1 v2))
         (toeye (multiple-value-bind (x y z)
                  (unit-vector
                    (- (x eye) (x p1))
                    (- (y eye) (y p1))
                    (- (z eye) (z p1)))
                  (make-point :x x :y y :z z)))
         (n2 (if (<= (dot-product n1 toeye) 0) n1 (neg-vec n1)))
         (pl (make-plane
               :p1 p1
               :p2 p2
               :p3 p3
               :nor n2
               :color c)))
    (push pl *world*)
    pl))

(defun intersect (o pt xr yr zr)
  (funcall (typecase o
             (sphere #'sphere-intersect)
             (plane #'plane-intersect))
           o pt xr yr zr))

(defun sphere-intersect (s pt xr yr zr)
  (let* ((c (sphere-center s))
         (n (minroot (+ (sq xr) (sq yr) (sq zr))
                     (* 2 (+ (* (- (x pt) (x c)) xr)
                             (* (- (y pt) (y c)) yr)
                             (* (- (z pt) (z c)) zr)))
                     (+ (sq (- (x pt) (x c)))
                        (sq (- (y pt) (y c)))
                        (sq (- (z pt) (z c)))
                        (- (sq (sphere-radius s)))))))
    (if n
        (make-point :x  (+ (x pt) (* n xr))
                    :y  (+ (y pt) (* n yr))
                    :z  (+ (z pt) (* n zr))))))

(defun plane-intersect (pl pt xr yr zr)
  (let ((dir (make-point :x xr :y yr :z zr)))
   (when (not (= (dot-product (plane-nor pl) dir) 0))
     (let* ((nor (plane-nor pl))
            (nom (dot-product (sub-point (plane-p1 pl) pt) nor))
            (denom (dot-product nor dir))
            (x (/ nom denom)))
       (when (>= x 0)
         (add-point pt (mul-vec x dir)))))))

(defun normal (o pt)
  (funcall (typecase o
             (sphere #'sphere-normal)
             (plane #'plane-normal))
           o pt))

(defun sphere-normal (s pt)
  (let ((c (sphere-center s)))
    (unit-vector (- (x c) (x pt))
                 (- (y c) (y pt))
                 (- (z c) (z pt)))))

(defun plane-normal (p pt)
  (let ((n (plane-nor p)))
    (unit-vector (x n)
                 (y n)
                 (z n))))

(defun ray-test (&optional (res 1) (filename "spheres.pgm"))
  (setf *world* nil)
;;   (defsphere 0 -300 -1200 200 .8)
;;   (defsphere -80 -150 -1200 200 .7)
  (defsphere 100 0 -1200 200 (make-color :r 0 :b 46 :g 184))
  (defsphere -100 0 -1200 200 (make-color :r 255 :b 0 :g 255))
  (let ((p1 (make-point :x 0 :y 300 :z 0))
        (p2 (make-point :x 300 :y 300 :z 0))
        (p3 (make-point :x 0 :y 300 :z 300)))
    (defplane p1 p2 p3 (make-color :r 128 :b 128 :g 128)))
;;   (do ((x -2 (1+ x)))
;;       ((> x 2))
;;     (do ((z 2 (1+ z)))
;;         ((> z 7))
;;       (defsphere (* x 200) 300 (* z -400) 40 .75)))
  (tracer (make-pathname :name filename) res 1))

(ray-test 5 "spheres_color.ppm")
