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

(defun move-gripper-closer (offset &optional (safety cram-beasty::*default-safety-settings*))
  (declare (type number offset))
  (let ((goal (make-instance
               'cartesian-impedance-control-parameters
               :goal-pose (query-tf-for-offset-pose offset))))
    (command-beasty *arm* goal safety)))

(defun query-tf-for-offset-pose (offset)
  (cl-tf:transform-pose
   *tf*
   :pose (cl-tf:make-pose-stamped
          "left_gripper"
          0
          (cl-transforms:make-3d-vector 0 0 offset)
          (cl-transforms:make-identity-rotation))
   :target-frame "calib_left_arm_base_link"))

(defun move-arm-down (offset safety)
  (declare (type number offset))
  (let ((goal (make-joint-impedance-goal
               :joint-goal (query-ik-relative-to-ee 
                            offset 
                            (joint-values (value (state *arm*)))))))
    (command-beasty *arm* goal safety)))

(defun retry-ik-query (condition)
  (declare (ignore condition))
  ;; (invoke-restart 'swank::retry)
  (invoke-restart 'roslisp::reconnect)
  )

(defun query-ik-relative-to-ee (offset current-configuration)
  (declare (type number offset)
           (type vector current-configuration))
  ;;; the persistent-service has a flaky connection
  ;;; just retry sending the request if there was a connectivity error
  (handler-bind ((roslisp::service-call-error #'retry-ik-query))
    (cram-ik-proxy:get-ik
     *ik-proxy*
     (cl-tf:transform-pose
      *tf*
      :pose (cl-tf:make-pose-stamped
             "left_arm_7_link"
             0
             (cl-transforms:make-3d-vector 0 0 offset)
             (cl-transforms:make-identity-rotation))
      :target-frame "left_arm_1_link")
     current-configuration)))