(defpackage :threading-asd
  (:use :cl :asdf))

(in-package :threading-asd)

(defsystem :threading
  :name "threading"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "Value threading macros for Common Lisp."
  :serial t
  :components ((:file "threading")))
