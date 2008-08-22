(require 'cl-who)
(require 'hunchentoot)
(require 'vecto)
(require 'cl-json)

(defpackage :test-page
  (:use :cl :cl-who :hunchentoot :vecto :json :flexi-streams))

(in-package :test-page)

(defun demo-handler ()
  (with-html-output-to-string
      (*standard-output* nil :prologue t :indent nil)
    (:html
     (:head
      (:title "Probix graph server"))
     (:body
      (:p "Pass the JSON here:")
      (:form :method "post"
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
                     decoded-json))))
      (:p
       (:img :src "/demo/graph.png"))))))

(defun build-graph ()
  (setf (content-type) "image/png")
  (with-canvas (:width 100 :height 100)
    (set-rgb-fill 1.0 0.65 0.3)
    (rounded-rectangle 0 0 100 100 10 10)
    (fill-path)
    (set-rgb-fill 1.0 1.0 1.0)
    (centered-circle-path 20 20 10)
    (fill-path)
    (flet ((quarter-circle (x y radius)
             (move-to (+ x radius) y)
             (arc x y radius 0 (/ pi 2))))
      (set-rgb-stroke 1.0 1.0 1.0)
      (set-line-width 15)
      (quarter-circle 20 20 30)
      (stroke)
      (quarter-circle 20 20 60)
      (stroke))
    (rounded-rectangle 5 5 90 90 7 7)
    (set-gradient-fill 50 90
                       1.0 1.0 1.0 0.7
                       50 20
                       1.0 1.0 1.0 0.0)
    (set-line-width 2)
    (set-rgba-stroke 1.0 1.0 1.0 0.1)
    (fill-and-stroke)
    (flexi-streams:with-output-to-sequence (png-stream)
      (save-png-stream png-stream)
      png-stream)))

;    (let ((png-stream (make-instance 'sb-gray:fundamental-binary-input-stream)))
;      (save-png-stream png-stream)
;      png-stream)))

(push (create-prefix-dispatcher "/demo" 'demo-handler) *dispatch-table*)
(push (create-prefix-dispatcher "/demo/graph.png" 'build-graph) *dispatch-table*)

