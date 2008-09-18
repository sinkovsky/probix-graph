(cl:defpackage :vecto-chart
  (:use :cl :vecto :flexi-streams)
  (:export render-png-stream))

(in-package :vecto-chart)

(defclass chart ()
  ((width :accessor chart-width)
   (height :accessor chart-height)
   (data :accessor chart-data)))


(defvar *offset-x* 10)
(defvar *offset-y* 10)

(defconstant +series-colors+
  (list '(0.6 0.5 0.9)
        '(0.3 0.6 0.7)
        '(0.5 0.3 0.8)
        '(0.9 0.4 0.3)
        '(0.4 0.8 0.2)))

(defmethod draw-axis ((chart chart))
  (let ((width (chart-width chart))
        (height (chart-height chart)))
    (set-rgb-fill 0.95 0.95 0.95)
    (rectangle 0 0 width height)
    (fill-path)
    (set-line-cap :square)
    (set-line-width 0.35)
    (move-to 10 10)
    (line-to 10 (- height 10))
    (move-to 10 10)
    (line-to (- width 10) 10)
    (stroke)))


(defmethod add-data ((chart chart) data)
  (move-to *offset-x* *offset-y*)
  (apply #'set-rgb-stroke (nth (random (length +series-colors+)) +series-colors+))
  (set-line-width 1)
  (let ((offset 10))
    (loop for i in data do
         (incf offset 10)
         (line-to (+ *offset-x* offset) (+ i *offset-y*)))
    (stroke)))


(defmethod render-png-stream ((chart chart))
  (with-canvas (:width (chart-width chart) :height (chart-height chart))
    (draw-axis chart)
    (add-data (chart-data chart))
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
