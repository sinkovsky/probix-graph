(cl:defpackage :vecto-chart
  (:use :cl :vecto :flexi-streams)
  (:export render-png-stream add-data draw-axis with-line-chart))

(in-package :vecto-chart)

(defclass chart ()
  ((width :accessor chart-width :initarg :width :initform 400)
   (height :accessor chart-height :initarg :height :initform 300)
   (data :accessor chart-data :initarg :data :initform nil)))

(defclass line-chart (chart)
  ((have-x-y :accessor chart-have-x-y :initform nil)
   (min-x :accessor chart-data-min-x :initform 0)
   (min-y :accessor chart-data-min-y :initform 0)
   (max-x :accessor chart-data-max-x :initform 0)
   (max-y :accessor chart-data-max-y :initform 0)
   (ratio-x :accessor chart-ratio-x :initform 1)
   (ratio-y :accessor chart-ratio-y :initform 1)))

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


(define-condition wrong-argument-structure (condition)
  ())

(defmethod add-data* ((chart chart) data)
  ; add data validation here

  ; 1) checking if data is empty
  (if (eq data nil)
	  (cond ((every #'numberp data)
			 (setf (chart-have-x-y chart) nil))
			((every #'consp data)
			 (setf (chart-have-x-y chart) T))))

	  ; 2) checking what we have list of numbers or list of conses
	  ; and checking if this is consistent with data that we have
  (if (or 
	   (and (every #'numberp data) (chart-have-x-y chart))
	   (and (every #'consp data) (not (chart-have-x-y chart)))
	   (not (or (not (every #'numberp data)) (not (every #'consp data)))))
	  (error 'wrong-argument-structure))
  
  (push data (chart-data chart)))

(defmacro calculate-min-max* (&body body)
  `(defmethod calculate-min-max ((chart line-chart))
	 (unless (chart-have-x-y chart)
	   ,@(loop for fn in '(min max)
			  collect
			  `(setf (,(intern (format nil "CHART-DATA-~A-Y" fn)) chart)
					 (apply #',fn (apply #'append (chart-data chart)))))
	   (setf (chart-data-min-x chart) 1)
	   (setf (chart-data-max-x chart)
			 (apply #'max
					(mapcar #'length (chart-data chart)))))
	 (when (chart-have-x-y chart)
	   ,@(loop for fn in '(min max)
		   collect 
			 `(setf (,(intern (format nil "CHART-DATA-~A-X" fn)) chart)
					(apply #',fn
						   (apply #'car (apply #'append (chart-data chart)))))
		   collect
			 `(setf (,(intern (format nil "CHART-DATA-~A-Y" fn)) chart)
					 (apply #',fn
							(apply #'cdr
								   (apply #'append (chart-data chart)))))))))

(calculate-min-max*)

(defun render-png-stream ()
  (render-png-stream* *current-chart*))

(defmethod calculate-ratios ((chart line-chart))
  (setf (chart-ratio-x chart)
		(/ (- (chart-width chart)
			  (* 10 *offset-x*))
		   (- (chart-data-max-x chart)
			  (chart-data-min-x chart))))
  (setf (chart-ratio-y chart)
		(/ (- (chart-height chart)
			  (* 10 *offset-y*))
		   (- (chart-data-max-y chart)
			  (chart-data-min-y chart)))))

(defmethod dx->x ((chart line-chart) x)
  (* (chart-ratio-x chart)
	 x))

(defmethod dy->y ((chart line-chart) y)
  (* (chart-ratio-y chart)
	 y))

(defmethod render-png-stream* ((chart line-chart))
  ; calculating min-max-values for all data
  (calculate-min-max chart)
  ; calculate ratios for given chart
  (calculate-ratios chart)

  ; iterating through data and drawing line
  (loop for dataset in (chart-data chart) do
	   (move-to *offset-x* *offset-y*)
	   (apply #'set-rgb-stroke
			  (nth (random (length +series-colors+)) +series-colors+))
	   (set-line-width 1)
	   (let ((offset 1))
		 (if (chart-have-x-y chart)
			 (loop for i in dataset do
					(line-to
					 (+ (dx->x chart (car i) *offset-x*))
					 (+ (dy->y chart (cdr i) *offset-y*))))
			 (loop for i in dataset do
				  (line-to
				   (+ *offset-x* (dx->x chart offset))
				   (+ (dy->y chart i) *offset-y*))
				  (incf offset 1))))
	   (stroke))
  (flexi-streams:with-output-to-sequence (png-stream)
	(save-png-stream png-stream)
	png-stream))

