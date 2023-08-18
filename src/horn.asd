(require 'asdf)
(ql:quickload :cl-ppcre)

(defpackage horn-system
  (:use :cl :asdf :cl-ppcre))

(asdf:defsystem "horn"
  :description "An Org parser prototype"
  :version "0.0"
  :author "ARVA"
  :licence "MIT"
  :components (
               (:module "globals"
                        :serial t
                        :components (
               (:file "state-methods")
               (:file "state" :depends-on ("state-methods"))
                                     ))
               (:module "classes"
                        :serial t
                        :components (
               (:file "horn-node" )
               (:file "glitter-node")
                                     )
                        :depends-on ("globals"))
               (:module "lib"
                        :serial t
                        :components (
               (:file "stream")
               (:file "functions")
               (:file "type-checker")
               (:file "parser" :depends-on ("stream" "functions" "type-checker"))
                                     )
                        :depends-on ("globals" "classes"))
               (:module "debug"
                        :serial t
                        :components (
               (:file "formatter" :depends-on ()
                                     ))
                        :depends-on ("globals" "classes" "lib"))
                        
               (:file "index" :depends-on ("globals" "classes" "lib" "debug"))))
