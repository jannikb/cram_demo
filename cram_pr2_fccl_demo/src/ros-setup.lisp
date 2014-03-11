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

(defparameter *l-arm-base-link* "torso_lift_link")
(defparameter *l-arm-tip-link* "l_gripper_tool_frame")
(defparameter *l-arm-kinematic-chain* 
  (cram-fccl:make-kinematic-chain *l-arm-base-link* *l-arm-tip-link*))

(defparameter *l-arm-fccl-controller-action-name* "/l_arm_fccl_controller/command")
(defparameter *l-arm-fccl-controller* nil)

(defparameter *l-arm-joint-names*
  #("l_upper_arm_roll_joint"
    "l_shoulder_pan_joint"
    "l_shoulder_lift_joint"
    "l_forearm_roll_joint"
    "l_elbow_flex_joint"
    "l_wrist_flex_joint"
    "l_wrist_roll_joint"))

(defparameter *l-arm-pouring-start-config*
  #(0 0 0 0 0 0 0))

(defun ensure-left-arm-controller ()
  (unless *l-arm-fccl-controller*
    (setf *l-arm-fccl-controller*
          (cram-fccl:make-fccl-action-client 
           *l-arm-fccl-controller-action-name* *l-arm-kinematic-chain*))))

(defun get-left-arm-controller ()
  (ensure-left-arm-controller)
  *l-arm-fccl-controller*)