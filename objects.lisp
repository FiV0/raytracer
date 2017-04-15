(in-package #:raytracer)

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
         (toeye (unit-vector
                  (construct-point
                    (- (x eye) (x p1))
                    (- (y eye) (y p1))
                    (- (z eye) (z p1)))))
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
  (let* ((c (sphere-center s))
         (uv (unit-vector
               (construct-point
                 (- (x c) (x pt))
                 (- (y c) (y pt))
                 (- (z c) (z pt))))))
    (values (x uv) (y uv) (z uv))))

(defun plane-normal (p pt)
  (let* ((n (plane-nor p))
         (uv (unit-vector
               (construct-point (x n)
                                (y n)
                                (z n)))))
    (values (x uv) (y uv) (z uv))))

