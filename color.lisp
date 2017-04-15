(in-package #:raytracer)

(defstruct (color (:print-function print-color)) r g b)

(defun print-color (c stream depth)
  (declare (ignore depth))
  (format stream "~A ~A ~A" (color-r c) (color-g c) (color-b c)))

;; luminosity method
(defun to-greyscale (color)
  (let ((greyvalue (round (+ (* 0.21 (color-r color))
                             (* 0.72 (color-g color))
                             (* 0.07 (color-b color))))))
    (make-color :r greyvalue :g greyvalue :b greyvalue)))

