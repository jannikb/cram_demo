; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem cram-saphari-review-2
  :name "cram-saphari-review-2"
  :author "Georg Bartels <georg.bartels@cs.uni-bremen.de>"
  :version "0.1"
  :maintainer "Georg Bartels <georg.bartels@cs.uni-bremen.de>"
  :licence "BSD"
  :description "CRAM code for Saphari Year 2 review"
  :depends-on (:cram-language
               :designators 
               :cram-language-designator-support
               :cram-reasoning
               :roslisp-utilities
               :roslisp
               :saphari_msgs-msg
               :saphari_msgs-srv
               :cl-human-shapes
               :cl-3d-shapes
               :cl-transforms
               :visualization_msgs-msg
               :equipment_msgs-msg
               :cl-tf
               :gsll)
  :components
  ((:module "src2"
            :components
            ((:file "package")
             (:file "msg-parser" :depends-on ("package"))
             (:file "util" :depends-on ("package"))
             (:file "direction" :depends-on ("package" "util"))
             (:file "fluents" :depends-on ("package" "util" "msg-parser"))
             (:file "visualization" :depends-on ("package" "util" "fluents" "direction"))))))