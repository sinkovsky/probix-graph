(cl:defpackage :vecto-chart
  (:use :cl :vecto :flexi-streams)
  (:export render-png-stream add-data draw-axis with-line-chart))

(in-package :vecto-chart)

(defclass chart ()
  ((width :accessor chart-width :initarg :width :initform 400)
   (height :accessor chart-height :initarg :height :initform 300)
   (data :accessor chart-data :initarg :data :initform nil)))

(defclass line-chart (chart)
  ())

(defclass pie-chart (chart)
  ())


(defmacro with-pie-chart ((&key width height) &body body)
 `(let ((*current-chart* (make-instance 'pie-chart
				      :width ,width
				      :height ,height)))
	(with-canvas (:width ,width :height ,height)
	  ,@body)))

(defvar *current-chart* nil)

(defmacro with-line-chart ((&key width height) &body body)
  `(let ((*current-chart* (make-instance 'line-chart
									   :width ,width
									   :height ,height)))
		 (with-canvas (:width ,width :height ,height)
		   ,@body)))

(defvar *offset-x* 10)
(defvar *offset-y* 10)

(defparameter +series-colors+
  (list '(0.6 0.5 0.9)
        '(0.3 0.6 0.7)
        '(0.5 0.3 0.8)
        '(0.9 0.4 0.3)
        '(0.4 0.8 0.2)))

(defun draw-axis ()
  (draw-axis* *current-chart*))

(defmethod draw-axis* ((chart line-chart))
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

(defun add-data (data)
  (add-data* *current-chart* data))
  
(defmethod add-data* ((chart chart) data)
  ; add data validation here
  (push data (chart-data chart)))

(defmacro calculate-min-max (&body body)
  `(defmethod calculate-min-max ((chart line-chart))
	 (unless chart-have-x-y
	   ,(loop for fn in '(min max) do
			 `(setf (,fn chart)
				   (apply #',fn (apply #'append (chart-data chart))))
			 `(setf (,fn chart)
				   (apply #',fn (apply #'append (chart-data chart))))
			 `(setf (chart-data-min-x chart) 1)
			 `(setf (chart-data-max-x chart) (length (car (chart-data chart))))))))

(calculate-min-max)

(defmethod calculate-min-max ((chart line-chart))
  (unless chart-have-x-y
	(setf (chart-data-min-y chart)
		  (apply #'min (apply #'append (chart-data chart))))
	(setf (chart-data-max-y chart)
		  (apply #'max (apply #'append (chart-data chart))))
	(setf (chart-data-min-x chart) 1)
	(setf (chart-data-max-y chart) (length (car (chart-data chart)))))

  (when chart-have-x-y
	(setf (chart-data-min-x chart)
		  (apply #'min (apply #'car (apply #'append (chart-data chart)))))
	(setf (chart-data-min-y chart)
		  (apply #'min (apply #'cdr (apply #'append (chart-data chart)))))
	(setf (chart-data-max-x chart)
		  (apply #'max (apply #'car (apply #'append (chart-data chart)))))
	(setf (chart-data-max-y chart)
		  (apply #'max (apply #'cdr (apply #'append (chart-data chart)))))))

(defun render-png-stream ()
  (render-png-stream* *current-chart*))

(defmethod render-png-stream* ((chart line-chart))
; detecting if we have consistent data
  (cond ((every #'consp (chart-data chart))
		 (setf (chart-have-x-y chart) 1))
		((every #'numberp (chart-data chart))
		 (setf (chart-have-x-y chart) nil)))

; detect max and min values in data
  
  
  (loop for dataset in (chart-data chart) do
	   (move-to *offset-x* *offset-y*)
	   (apply #'set-rgb-stroke (nth (random (length +series-colors+)) +series-colors+))
	   (set-line-width 1)
	   (let ((offset 10))
		 (loop for i in dataset do
			  (incf offset 10)
			  (line-to (+ *offset-x* offset) (+ i *offset-y*))))
	   (stroke))
  (flexi-streams:with-output-to-sequence (png-stream)
	(save-png-stream png-stream)
	png-stream))
