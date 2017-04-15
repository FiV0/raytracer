;;;; raytracer.lisp

(in-package #:raytracer)

;;; "raytracer" goes here. Hacks and glory await!

(defstruct surface color)

(defparameter *world* nil)
(defparameter *light-sources* (list (make-point :x 300 :y -2000 :z -1200)
                                    (make-point :x -10000 :y -2000 :z -1200)))

(defparameter *specular-exponent* 100)

(defconstant eye (make-point :x 0 :y 0 :z 200))

(defun tracer (pathname &optional (res 1) (color nil))
  (with-open-file (p pathname :direction :output :if-exists :supersede)
    (format p "P3 ~A ~A 255" (* res 100) (* res 100))
    (let ((inc (/ res)))
      (do ((y -50 (+ y inc)))
          ((< (- 50 y) inc))
        (do ((x -50 (+ x inc)))
            ((< (- 50 x) inc))
            (print
              (if color
                (color-at x y)
                (to-greyscale (color-at x y))) p))))))

(defun color-at (x y)
  (let ((dir (unit-vector
               (construct-point (- x (x eye))
                                (- y (y eye))
                                (- 0 (z eye))))))
    (sendray eye (x dir) (y dir) (z dir))))

(defun sendray (pt xr yr zr)
  (multiple-value-bind (o int) (first-hit pt xr yr zr)
    (let ((r 0) (g 0) (b 0))
      (when int
        (dolist (light-source *light-sources*)
          ;; diffuse
          (let* ((ligth-dir
                  (unit-vector
                    (construct-point
                      (- (x int) (x light-source))
                      (- (y int) (y light-source))
                      (- (z int) (z light-source)))))
                 (lxr (x ligth-dir))
                 (lyr (y ligth-dir))
                 (lzr (z ligth-dir)))
            (multiple-value-bind (fo fint)
              (first-hit light-source lxr lyr lzr)
              (when (and (not (null o)) (eq fo o) (point-equal fint int))
                (let ((lam (lambert o int lxr lyr lzr)))
                  (incf r (* lam (color-r (surface-color o))))
                  (incf b (* lam (color-b (surface-color o))))
                  (incf g (* lam (color-g (surface-color o))))))))
          ;; specular
          ))
      (make-color :r (round r) :g (round g) :b (round b)))))

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


(defun ray-test (&optional (res 1) (filename "spheres.pgm") (color nil))
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
  (tracer (make-pathname :name filename) res color))

(ray-test 5 "spheres_color.ppm" T)

