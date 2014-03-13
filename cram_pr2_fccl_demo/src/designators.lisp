;;; Copyright (c) 2014, Georg Bartels <georg.bartels@cs.uni-bremen.de>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;; * Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.
;;; * Redistributions in binary form must reproduce the above copyright
;;; notice, this list of conditions and the following disclaimer in the
;;; documentation and/or other materials provided with the distribution.
;;; * Neither the name of the Institute for Artificial Intelligence/
;;; Universitaet Bremen nor the names of its contributors may be used to 
;;; endorse or promote products derived from this software without specific 
;;; prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :pr2-fccl-demo)

;;;
;;; AUX KNOWROB CALLS
;;;

(defun knowrob-pouring-description ()
  (query-motion-description 
   "motion:'PouringSomethingFromBottle'"
   "knowrob:'BottleCap'"
   "knowrob:'PancakeMaker'"))

(defun knowrob-flipping-description ()
  (warn "Lookup of pancake flipping not implemented, yet.")
  nil)

;;;
;;; ASSEMBLING CONTROLLER CALLS
;;;

(defun constraint-controller-start ()
  (lambda (motion-phase)
    (cram-fccl:command-motion (get-left-arm-fccl-controller) motion-phase)))

(defun constraint-controller-stop ()
  (lambda () (cram-fccl:cancel-motion (get-left-arm-fccl-controller))))

;;;
;;; ASSEMBLING FLUENTS
;;;

(defun constraint-controller-finished-fluent ()
  (cram-fccl:get-constraints-fulfilled-fluent (get-left-arm-fccl-controller)))

;;;
;;; AUXILIARY FACTS FOR OUR DESIGNATORS
;;;

(def-fact-group pr2-fccl-demo-designators (action-desig)
  
  (<- (action-desig ?desig (?motion ?controller-start ?controller-stop ?controller-fluent))
    (constraints-desig? ?desig)
    (desig-prop ?desig (to pour))
    (lisp-fun knowrob-pouring-description ?motion)
    (lisp-fun constraint-controller-start ?controller-start)
    (lisp-fun constraint-controller-stop ?controller-stop)
    (lisp-fun constraint-controller-finished-fluent ?controller-fluent))

  (<- (action-desig ?desig (?motion ?controller-start ?controller-stop ?controller-fluent))
    (constraints-desig? ?desig)
    (desig-prop ?desig (to flip))
    (lisp-fun knowrob-flipping-description ?motion)
    (lisp-fun constraint-controller-start ?controller-start)
    (lisp-fun constraint-controller-stop ?controller-stop)
    (lisp-fun constraint-controller-finished-fluent ?controller-fluent)))