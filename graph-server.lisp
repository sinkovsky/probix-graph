(require 'cl-who)
(require 'hunchentoot)
(require 'vecto)
(require 'cl-json)

(defpackage :graph-server
  (:use :cl :cl-who :hunchentoot :vecto :json :flexi-streams))

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
             (:input :type :submit))
      (:p
       (format t "POST: ~A" (post-parameters)))
      (:p
       (let ((decoded-json nil))
         (ignore-errors
           (setf decoded-json (decode-json-from-string
                               (post-parameter "json"))))
         (format t "DECODED JSON: ~A"
                 (if (not decoded-json)
                     "Error in decoding JSON"
                     decoded-json))))))))

(defun json-post-handler ()
  (cond ((string= (post-parameter "output-format") "png")
         ())
        ((string= (post-parameter "output-format") "html") ())))

(defvar *offset-x* 10)
(defvar *offset-y* 10)

(defun build-graph ()
  (setf (content-type) "image/png")
  (with-canvas (:width 500 :height 300)
    (set-rgb-fill 0.95 0.95 0.95)
    (rectangle 0 0 500 300)
    (fill-path)
    (set-line-cap :square)
    (set-line-width 0.35)
    (move-to 10 10)
    (line-to 10 290)
    (move-to 10 10)
    (line-to 490 10)
    (stroke)
    (move-to *offset-x* *offset-y*)
    (set-rgb-stroke 0.6 0.5 0.9)
    (set-line-width 1)
    (let ((data #(10 20 30 70 60 40 41 76 23 54 23 54 12 125 34 65 23 229 150 139 90 40 50 30 50 20 30 180 150 90 30 45 90 110 130 20 150 160 130 140 40 30 20 10))
          (offset 10))
      (loop for i across data do
           (incf offset 10)
           (line-to (+ *offset-x* offset) (+ i *offset-y*))))
    (stroke)
    (flexi-streams:with-output-to-sequence (png-stream)
      (save-png-stream png-stream)
      png-stream)))

(push (create-prefix-dispatcher "/json-form" 'json-form-handler) *dispatch-table*)
(push (create-prefix-dispatcher "/image" 'json-post-handler) *dispatch-table*)
(push (create-prefix-dispatcher "/demo/graph.png" 'build-graph) *dispatch-table*)

