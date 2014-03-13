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

(cpl-impl:def-cram-function pour-pancake-mix ()
  (with-designators ((desig (action `((type constraints) (to pour)))))
    (perform-constraint-desig desig)))

(cpl-impl:def-cram-function flip-pancake ()
  (with-designators ((desig (action `((type constraints) (to flip)))))
    (perform-constraint-desig desig)))

(cpl-impl:def-cram-function perform-constraint-desig (desig)
  (destructuring-bind (motions start-controller stop-controller finished-fluent)
      (reference desig)
    (ensure-vel-controllers)
    (loop for motion in motions do
      (cram-language:pursue
        (funcall start-controller motion)
        (cram-language:whenever ((cram-language:pulsed finished-fluent))
          (when (cram-language-implementation:value finished-fluent)
            (funcall stop-controller)))))))
  

;; SOME NAIVE POURING PLANS.
;; TO TEST THEM, MAKE SURE TO HAVE STARTED THE LAUNCH-FILE:
;;  <launch-file>

;; (def-top-level-cram-function pouring ()
;;   "Entry point to top-level plan for pouring."
;;   (with-process-modules-running (pr2-fccl-process-module)
;;     (with-designators ((oven (object '((type oven))))
;;                        (pancake-bottle (object '((type pancake-bottle)))))
;;       (achieve `(pour-on-object ,pancake-bottle ,oven)))))

;; (def-goal (achieve (pour-on-object ?target-obj ?source-obj))
;;   "Abstract high-level plan to pour some stuff from `?source-obj' onto `?target-obj'."
;;   (with-designators
;;     ((pouring-action (action `((type constraints)
;;                                (to pour)
;;                                (obj-acted-with ,?source-obj)
;;                                (obj-acted-on ,?target-obj)))))
;;     (equate pouring-action (reason-about-action pouring-action))
;;     ;(perform pouring-action)
;;     ))

;; (def-cram-function reason-about-action (?action-desig)
;;   "Function which enriches `?action-desig' with the constraint specs needed to perform it."
;;   (with-desig-props (obj-acted-with obj-acted-on) ?action-desig
;;     ;; all this could go to prolog pattern matching in some reasoning module
;;     (assert (eq (desig-prop-value ?action-desig 'type) 'constraints)
;;             () "Expected action ~a to have property `type' equal to `constraints'.~%"
;;             ?action-desig)
;;     (assert (eq (desig-prop-value ?action-desig 'to) 'pour)
;;             () "Expected action ~a to have property `to' equal to `pour'.~%"
;;             ?action-desig)
;;     (assert obj-acted-with () "Action ~a needs an `obj-acted-with' property.~%"
;;             ?action-desig)
;;     (assert obj-acted-on () "Action ~a needs an `obj-acted-on' property.~%"
;;             ?action-desig)
;;     (assert (eq (desig-prop-value obj-acted-with 'type) 'pancake-bottle)
;;             () "Expected object ~a to have property `type' equal to `pancake-bottle'.~%"
;;             obj-acted-with)
;;     (assert (eq (desig-prop-value obj-acted-on 'type) 'oven)
;;             () "Expected object ~a to have property `type' equal to `oven'.~%"
;;             obj-acted-on)
;;     ;; hand-coding the feature and constraint descriptions for the motion
;;     (let* ((bottle-top (make-plane-feature "bottle-cover-top" "/pancake_bottle" 
;;                                            :position (make-3d-vector 0 0 0.0825)
;;                                            :normal (make-3d-vector 0 0 0.03)))
;;            (bottle-axis (make-line-feature  "bottle-main-axis" "/pancake_bottle"
;;                                             :direction (make-3d-vector 0 0 0.1)))
;;            (oven-center (make-plane-feature "oven-center" "/pancake"
;;                                             :normal (make-3d-vector 0 0 0.1)))
;;            (top-distance-constraint
;;              (make-distance-constraint
;;               "distance bottle-top to oven-center" bottle-top oven-center 0.03 0.07))
;;            (top-height-constraint
;;              (make-height-constraint
;;               "height bottom-top over oven-center" bottle-top oven-center 0.25 0.3))
;;            (bottle-upright-constraint
;;              (make-perpendicular-constraint
;;               "bottle upright" bottle-axis oven-center 0.95 1.2))
;;            (bottle-pointing-at-oven-center
;;              (make-pointing-at-constraint
;;               "bottle pointing oven center" bottle-axis oven-center -0.1 0.1))
;;            (bottle-tilting-down
;;              (make-perpendicular-constraint
;;               "bottle tilting down" bottle-axis oven-center -0.2 -0.1))
;;            (constraints-phase1 (list top-distance-constraint
;;                                      top-height-constraint
;;                                      bottle-upright-constraint))
;;            (constraints-phase2 (list top-distance-constraint
;;                                      top-height-constraint
;;                                      bottle-pointing-at-oven-center
;;                                      bottle-tilting-down)))
;;       ;; update our designators
;;       (equate obj-acted-with 
;;               (desig::copy-designator
;;                obj-acted-on
;;                :new-description `((features (feature ,oven-center)))))
;;       (equate obj-acted-with 
;;               (desig::copy-designator
;;                obj-acted-with
;;                :new-description `((features
;;                                    (feature ,bottle-top)
;;                                    (feature ,bottle-axis)))))
;;       (equate ?action-desig 
;;               (desig::copy-designator
;;                ?action-desig
;;                :new-description `((movement-plan
;;                                    (movement (constraints ,constraints-phase1))
;;                                    (movement (constraints ,constraints-phase2))
;;                                    (movement (constraints ,constraints-phase1)))))))))