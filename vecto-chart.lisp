(cl:defpackage :vecto-chart
  (:use :cl :vecto :flexi-streams)
  (:export render-png-stream))

(in-package :vecto-chart)

(defvar *offset-x* 10)
(defvar *offset-y* 10)

(defun draw-axis (size-x size-y)
  (set-rgb-fill 0.95 0.95 0.95)
  (rectangle 0 0 size-x size-y)
  (fill-path)
  (set-line-cap :square)
  (set-line-width 0.35)
  (move-to 10 10)
  (line-to 10 (- size-y 10))
  (move-to 10 10)
  (line-to (- size-x 10) 10)
  (stroke))

(defconstant +series-colors+
  (list '(0.6 0.5 0.9)
        '(0.3 0.6 0.7)
        '(0.5 0.3 0.8)
        '(0.9 0.4 0.3)
        '(0.4 0.8 0.2)))

(defun add-data (data)
  (move-to *offset-x* *offset-y*)
  (apply #'set-rgb-stroke (nth (random (length +series-colors+)) +series-colors+))
  (set-line-width 1)
  (let ((offset 10))
    (loop for i in data do
         (incf offset 10)
         (line-to (+ *offset-x* offset) (+ i *offset-y*)))
    (stroke)))


(defun render-png-stream (size-x size-y data)
  (with-canvas (:width size-x :height size-y)
    (draw-axis size-x size-y)
    (add-data data)
    (flexi-streams:with-output-to-sequence (png-stream)
      (save-png-stream png-stream)
      png-stream)))


;(defclass chart ()
;  (size-x size-y data))

;(defgeneric draw-chart (chart)
;  "Generic method drawing chart")

;(defclass pie-chart (chart)
;  )

;(defclass line-chart (chart)
;  )


;(defmethod draw-chart ((chart pie-chart))
;  )

;(defmethod draw-chart ((chart line-chart))
;  )
