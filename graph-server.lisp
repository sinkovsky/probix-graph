(defpackage :graph-server
  (:use :cl :cl-who :hunchentoot :vecto :json :flexi-streams :vecto-chart)
  (:export start-server stop-server))

(in-package :graph-server)

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


;(define-condition json-parse-error (condition)
;  ((message :initarg :message :accessor message)))

(defun parse-json (json)
  (json-bind (size data) json
    (json-bind (x y) size
      (if (not (and x y data (listp data)))
          (error 'json-parse-error :message
                 (format nil "Size: ~A, x: ~A, y: ~A, data: ~A" size x y data))
          (list :data data :size-x x :size-y y)))))

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

(defun output-error-json (err)
    (setf (content-type) "text/plain")
    (with-html-output-to-string
        (*standard-output* nil :prologue nil :indent nil)
      (format t "Error: ~A" err)))

(defun image-output-png (json)
  (setf (content-type) "image/png")
  (let ((data (getf json :data))
        (size-x (getf json :size-x))
        (size-y (getf json :size-y)))
    (vecto-chart:render-png-stream size-x size-y data)))
  
(defun image-output-debug (json)
  (with-html-output-to-string (*standard-output* nil :prologue nil :indent nil)
    (format t "DECODED JSON: ~A" json)))

(defun json-data-handler ()
  "[[10, 20], [20, 30], [30, 40], [40,20]]")

(push (create-prefix-dispatcher "/json-form" 'json-form-handler) *dispatch-table*)
(push (create-prefix-dispatcher "/image" 'json-post-handler) *dispatch-table*)
(push (create-prefix-dispatcher "/json-data" 'json-data-handler) *dispatch-table*)

