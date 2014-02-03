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

(in-package :cram-saphari-review-year2)

(defparameter *start-config*
  #(0.809 1.13 -0.121 -1.389 0.477 0.621 -0.12)
  "Start configuration for the reflexxes demo.")

(defparameter *end-config*
  #(-0.623 0.772 -0.903 -0.672 0.657 0.621 -0.02)
  "End configuration for the reflexxes demo.")

(defparameter *pre-grasp-dummy-config*
  #(-1.556 1.167 -0.843 -1.905 -0.582 0.451 -0.073)
  "Dummy config to test grasping motions.")

(defparameter *high-joint-speed*
  (make-array 7 :initial-element 0.5))
(defparameter *low-joint-speed*
  (make-array 7 :initial-element 0.25))

(defparameter *high-joint-acc*
  (make-array 7 :initial-element 1))
(defparameter *low-joint-acc*
  (make-array 7 :initial-element 0.5))

(cpl:def-top-level-cram-function grasping-demo ()
  (init-arms)
  (move-down-until-touch)
  (cram-wsg50:close-gripper *wsg50* :width 16)
  (move-up)
  (cram-wsg50:open-gripper *wsg50*))

(cpl:def-cram-function init-arms ()
  (let ((goal (make-joint-impedance-goal :joint-goal *pre-grasp-dummy-config*)))
    (command-beasty *arm* goal)))

(cpl:def-cram-function move-down-until-touch ()
  (let ((safety (make-safety-settings
                 (list 
                  (make-beasty-reflex :CONTACT :SOFT-STOP)))))
    (pursue
      (cpl:wait-for (cpl:not (cpl:eql *collision-fluent* :NO-CONTACT)))
      (loop do
        (move-arm-down 0.05 safety)))
    (move-arm-down -0.01 safety)))

(cpl:def-cram-function move-up ()
  (let ((safety (make-safety-settings
                 (list 
                  (make-beasty-reflex :CONTACT :IGNORE)
                  (make-beasty-reflex :LIGHT-COLLISION :SOFT-STOP)))))
    (move-arm-down -0.07 safety)))

(cpl:def-top-level-cram-function human-perception-demo ()
  (let ((distance-fluent (fl-funcall #'calculate-human-distance *human-fluent*))
        (distance-threshold 1.5)
        (goal1 (make-joint-impedance-goal :joint-goal #(-2.5 0.53 0 -1 0 0.53 0)))
        (goal2 (make-joint-impedance-goal :joint-goal #(-2.5 1.23 0 -1.3 0 0.53 0)))
        (safety (make-safety-settings
                 (list
                  (make-beasty-reflex :CONTACT :IGNORE)
                  (make-beasty-reflex :LIGHT-COLLISION :JOINT-IMP)
                  (make-beasty-reflex :STRONG-COLLISION :SOFT-STOP)
                  (make-beasty-reflex :SEVERE-COLLISION :HARD-STOP)))))
    (pursue
      (:tag motion-task
        (cpl:retry-after-suspension
          (loop for i to 2 do
            (seq
              (command-beasty *arm* goal1 safety)
              (command-beasty *arm* goal2 safety)))))
      (whenever ((pulsed distance-fluent))
        (when (cpl:value (cpl:< distance-fluent distance-threshold))
          (cpl:with-task-suspended (motion-task)
            (stop-beasty *arm*)
            (cpl-impl:wait-for (cpl:> distance-fluent distance-threshold))))))))
              
(cpl:def-top-level-cram-function reflex-demo ()
  (loop for i to 1 do
    (seq
      (reflex-motion *start-config*)
      (reflex-motion *end-config*))))

(cpl:def-cram-function reflex-motion (goal-config)
  (let ((goal 
          (make-joint-impedance-goal
           :joint-goal goal-config
           :max-joint-vel *high-joint-speed*
           :max-joint-acc *high-joint-acc*))
        (safety-settings
          (make-safety-settings
           (list
            (make-beasty-reflex :CONTACT :IGNORE)
            (make-beasty-reflex :LIGHT-COLLISION :JOINT-IMP)
            (make-beasty-reflex :STRONG-COLLISION :SOFT-STOP)
            (make-beasty-reflex :SEVERE-COLLISION :HARD-STOP)))))
    (pursue
      (:tag motion-execution
        (retry-after-suspension
          (format t "MOTION-EXECUTION~%")
          (command-beasty *arm* goal safety-settings)))
      (whenever ((pulsed *collision-fluent*))
        (case (value *collision-fluent*)
          (:CONTACT
           (with-task-suspended (motion-execution)
             (format t "SLOWING-DOWN TASK~%")
             (setf (max-joint-vel goal) *low-joint-speed*)
             (setf (max-joint-acc goal) *low-joint-acc*)
             (cancel-command *arm*)))
          ((:LIGHT-COLLISION :STRONG-COLLISION :SEVERE-COLLISION)
           (with-task-suspended (motion-execution)
             (format t "WAITING ON TASK~%")
             (format t "ENCOUNTERED: ~a~%~%" (value *collision-fluent*))
             (stop-beasty *arm*)
             (setf (max-joint-vel goal) *low-joint-speed*)
             (setf (max-joint-acc goal) *low-joint-acc*)
             (cpl:sleep* 2))))))))