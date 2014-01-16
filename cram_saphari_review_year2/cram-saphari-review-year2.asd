; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem cram-saphari-review-year2
  :name "cram-saphari-review-year2"
  :author "Georg Bartels <georg.bartels@cs.uni-bremen.de>"
  :version "0.1"
  :maintainer "Georg Bartels <georg.bartels@cs.uni-bremen.de>"
  :licence "BSD"
  :description "CRAM code for Saphari Year 2 review"
  :depends-on (:cram-language 
               :designators 
               :process-modules
               :boxy-manipulation-process-module)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "plan-lib" :depends-on ("package"))))))