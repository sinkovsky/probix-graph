(require 'cl-who)
(require 'hunchentoot)
(require 'vecto)
(require 'cl-json)

(defpackage :graph-server
  (:use :cl :cl-who :hunchentoot :vecto :json :flexi-streams))

(in-package :graph-server)

(defvar *offset-x* 10)
(defvar *offset-y* 10)

(defun json-form-handler ()
    (with-html-output-to-string
      (*standard-output* nil :prologue t :indent nil)
    (:html
     (:head
      (:title "Probix graph server"))
     (:body
      (:p "Pass the JSON here:")
      (:form :method "post" :action "/image"
             (:textarea :name "json" :cols 60 :rows 15 (format t "~A" (post-parameter "json")))
             (:br)
             (:select :name "output-format"
                      (:option :value "png" "PNG") (:option :value "debug" "DEBUG"))
             (:br)
             (:input :type :submit))))))


(define-condition json-parse-error (error)
  ((message :initarg :message)))

(defun parse-json (json)
  (handler-case
      (progn
        (json-bind (size data) json
          (json-bind (x y) size
            (if (not (and x y data (listp data)))
                (error 'json-parse-error :message
                       (format nil "Size: ~A, x: ~A, y: ~A, data: ~A" size x y data))
                (list :data data :size-x x :size-y y)))))
    (error () nil)))

(defun json-post-handler ()
  (let (decoded-json)
    (handler-case
        (progn 
          (setf decoded-json (parse-json (post-parameter "json")))
          (cond ((string= (post-parameter "output-format") "png")
                 (image-output-png decoded-json))
                ((string= (post-parameter "output-format") "debug")
                 (image-output-debug decoded-json))))
      (json-parse-error (perror)
        (output-error-json perror)))))

(defun output-error-json (error)
    (setf (content-type) "text/plain")
    (with-html-output-to-string
        (*standard-output* nil :prologue nil :indent nil)
      (format t "Error: ~A ~A" error (message error))))

(defun image-output-png (json)
  (setf (content-type) "image/png")
  (let ((data (getf json :data))
        (size-x (getf json :size-x))
        (size-y (getf json :size-y))
        (offset 10))
    (with-canvas (:width size-x :height size-y)
      (set-rgb-fill 0.95 0.95 0.95)
      (rectangle 0 0 size-x size-y)
      (fill-path)
      (set-line-cap :square)
      (set-line-width 0.35)
      (move-to 10 10)
      (line-to 10 (- size-y 10))
      (move-to 10 10)
      (line-to (- size-x 10) 10)
      (stroke)
      (move-to *offset-x* *offset-y*)
      (set-rgb-stroke 0.6 0.5 0.9)
      (set-line-width 1)
      (loop for i in data do
           (incf offset 10)
           (line-to (+ *offset-x* offset) (+ i *offset-y*)))
      (stroke)
      (flexi-streams:with-output-to-sequence (png-stream)
      (save-png-stream png-stream)
      png-stream))))
  
(defun image-output-debug (json)
  (with-html-output-to-string (*standard-output* nil :prologue nil :indent nil)
    (format t "DECODED JSON: ~A" json)))

(push (create-prefix-dispatcher "/json-form" 'json-form-handler) *dispatch-table*)
(push (create-prefix-dispatcher "/image" 'json-post-handler) *dispatch-table*)


