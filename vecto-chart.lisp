(cl:defpackage :vecto-chart
  (:use :cl :vecto :flexi-streams)
  (:export #:render-png-stream #:add-data #:with-line-chart #:draw-axis))

(in-package :vecto-chart)

(defclass chart ()
  ((width :accessor chart-width :initarg :width :initform 400)
   (height :accessor chart-height :initarg :height :initform 300)
   (data :accessor chart-data :initarg :data :initform nil)
   (offset :accessor chart-offset :initarg :offset :initform 0))))

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
										 :height ,height
										 :offset (* ,height 0.05))))
	 (with-canvas (:width ,width :height ,height)
	   ,@body)))



(defparameter +series-colors+
  (list '(0.6 0.5 0.9)
        '(0.3 0.6 0.7)
        '(0.5 0.3 0.8)
        '(0.9 0.4 0.3)
        '(0.4 0.8 0.2)))

(defun draw-axis ()
  (draw-axis* *current-chart*))

(defmethod draw-axis* ((chart line-chart))
  (with-slots (width height offset) chart
    (set-rgb-fill 0.95 0.95 0.95)
    (rectangle 0 0 width height)
    (fill-path)
    (set-line-cap :square)
	;; (set-line-width 0.35)
    (move-to offset offset)
    (line-to offset (- height offset))
    (move-to offset offset)
    (line-to (- width offset) offset)
    (stroke)))


(define-condition wrong-argument-structure (condition)
  ())

;;; helper that uses lexical variable as a chart instance
(defun add-data (data)
  (add-data* *current-chart* data))

;;; CHART method to add more data to chart
;;; It validates data for consistency and
;;; pushes it to data slot
(defmethod add-data* ((chart chart) data)
  ;; add data validation here

  ;; 1) checking if data is empty
  (if (eq data nil)
	  (cond ((every #'numberp data)
			 (setf (chart-have-x-y chart) nil))
			((every #'consp data)
			 (setf (chart-have-x-y chart) T))))

  ;; 2) checking what we have list of numbers or list of conses
  ;; and checking if this is consistent with data that we have
  (if (or 
	   (and (every #'numberp data) (chart-have-x-y chart))
	   (and (every #'consp data) (not (chart-have-x-y chart)))
	   (not (or (not (every #'numberp data)) (not (every #'consp data)))))
	  (error 'wrong-argument-structure))
  
  (push data (chart-data chart)))


;;; Macro to avoid lot of boilerplate code
;;; that generates CHART class method
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

;; generating method
(calculate-min-max*)

;;; VECTO method that calculates ratios for x and y axis
(defmethod calculate-ratios ((chart line-chart))
  (with-slots (width height offset ratio-x ratio-y min-x min-y max-x max-y) chart
	(setf ratio-x
		  (/ (- width
				(* 2 offset) ; from left and right
				10) ; offset from the end of X axis
			 (- max-x min-x)))
	(setf ratio-y
		  (/ (- height
				(* 2 offset) ; from top and bottom
				10) ; from the end of Y axis
			 (- max-y min-y )))))


;;; Method to convert data values to pixel values for X axis
(defmethod dx->x ((chart line-chart) x)
  (+ (chart-offset chart) 
	 (* (chart-ratio-x chart)
		(- x (chart-data-min-x chart)))))

;;; Method to convert data values to pixel values for Y axis
(defmethod dy->y ((chart line-chart) y)
  (+ (chart-offset chart)
	 (* (chart-ratio-y chart)
		(- y (chart-data-min-y chart)))))


;;; short helper
(defun render-png-stream ()
  (render-png-stream* *current-chart*))

;;; Main drawing method
;;; returns png data stream
(defmethod render-png-stream* ((chart line-chart))
  (let ((data (chart-data chart))
		(have-x-y (chart-have-x-y chart)))
	;; calculating min-max-values for all data
	(calculate-min-max chart)
	;; calculate ratios for given chart
	(calculate-ratios chart)

	;; iterating through data and drawing line
	(loop for dataset in data do
		 (if have-x-y
			 (move-to
			  (1+ (dx->x chart (caar dataset)))
			  (1+ (dy->y chart (cadr dataset))))
			 (move-to
			  (1+ (dx->x chart 1))
			  (1+ (dy->y chart (car dataset)))))
		 (pop dataset)
		 (apply #'set-rgb-stroke
				(nth (random (length +series-colors+)) +series-colors+))
		 (set-line-width 1)
		 (set-line-join :round)
		 (set-line-cap :round)
		 (let ((current-x 2))
		   (if have-x-y
			   (loop for item in dataset do
					(line-to
					 (dx->x chart (car item))
					 (dy->y chart (cdr item))))
			   (loop for item in dataset do
					(line-to
					 (dx->x chart current-x)
					 (dy->y chart item))
					(incf current-x))))
		 (stroke))
	(flexi-streams:with-output-to-sequence (png-stream)
	  (save-png-stream png-stream)
	  png-stream)))

