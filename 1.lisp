(defpackage #:test
  (:use #:cl #:vecto))

(in-package #:test)

(defun fac (n)
  (if (= n 0)
      1
      (* n (fac (decf n)))))



(defun feedlike-icon (file size-x size-y)
  (with-canvas (:width size-x :height size-y)
    (set-rgb-fill 1.0 0.65 0.3)
    (rounded-rectangle 0 0 size-x size-y 10 10)
    (fill-path)
    (set-rgb-fill 1.0 1.0 1.0)
    (centered-circle-path 20 20 10)
    (fill-path)
    (flet ((quarter-circle (x y radius)
             (move-to (+ x radius) y)
             (arc x y radius 0 (/ pi 2))))
      (set-rgb-stroke 1.0 1.0 1.0)
      (set-line-width (/ size-x 7))
      (quarter-circle 20 20 (/ size-x 2.5))
      (stroke)
      (quarter-circle 20 20 (/ size-x 1.5))
      (stroke))
    (rounded-rectangle 5 5 (- size-x 10) (- size-y 10) 7 7)
    (set-gradient-fill 50 90
                       1.0 1.0 1.0 0.7
                       50 20
                       1.0 1.0 1.0 0.0)
    (set-line-width 2)
    (set-rgba-stroke 1.0 1.0 1.0 0.1)
    (fill-and-stroke)
    (save-png file)))


