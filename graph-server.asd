(defpackage :graph-server-asd
  (:use :cl :asdf))

(in-package :graph-server-asd)

(defsystem "graph-server"
  :description "graph-server: Graph server for Probix"
  :version "0.1"
  :author "Sergey Sinkovskiy <glorybox.away@gmail.com>"
  :license "Public Domain"
  :depends-on ("vecto-chart" "flexi-streams" "hunchentoot" "cl-who" "cl-json")
  :components ((:file "graph-server")))
