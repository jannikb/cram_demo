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

(defvar *arm* nil
  "Variable holding the interface to the Beasty controller of the arm.")
(defvar *collision-fluent* nil
  "Fluent indicating whether a collision was signalled from the arm.")

(defparameter *beasty-action-name* "/BEASTY"
  "ROS name of the Beasty action server for the arm.")
(defparameter *simulation-flag* nil
  "Flag indicating whether the LWR is a simulated arm.")
(defparameter *tool-weight* 1.8
  "Weight of the tool mounted to the LWR in kg.")
(defparameter *tool-com* (cl-transforms:make-3d-vector 0.0 0.0 0.17)
  "Center of mass of tool mounted on the arm.")
(defparameter *tool-ee-offset* (cl-transforms:make-transform
                               (cl-transforms:make-3d-vector 0.0 0.0 0.266)
                               (cl-transforms:make-quaternion
                                0 0 -0.38268 0.92388)))
  "Transform from TCP frame to EE frame.")
(defparameter *arm-tool*
  (make-instance 'beasty-tool :mass *tool-weight* :com *tool-com* 
                              :ee-transform *tool-ee-offset*)
  "Modelling of tool mounted on LWR.")
(defparameter *gravity-vector* #(-7.358 4.248 4.905 0 0 0) ;#(0 0 9.81 0 0 0)
  "_NEGATIV_ 6D acceleration vector indicating in which direction gravity is acting on the
  arm. NOTE: Is expressed w.r.t. to the base-frame of the left arm. First translational
 (x,y,z), then rotational (x,y,z) acceleration.")  
(defparameter *arm-base-config*
  (make-instance 'beasty-base :base-acceleration *gravity-vector*)
  "Modelling of mounting of LWR to the table.")
(defparameter *arm-config*
  (make-instance 'beasty-robot 
                 :simulation-flag *simulation-flag* 
                 :tool-configuration *arm-tool* 
                 :base-configuration *arm-base-config*)
  "Modelling of entire initial configuration of the LWR.")
(defparameter *arm-base-frame-id* "/left_arm_base_link"
  "TF frame-id of base of the LWR arm.")

(defparameter *ptu* nil
  "Variable holding interface of PTU moving the head.")
(defparameter *ptu-action-name* "/ptu"
  "ROS action-name of the PTU moving the head.")

(defparameter *ik-proxy* nil
  "Variable holding interface to IK proxy for the arm.")
(defparameter *ik-proxy-service-name* "/lwr_ik"
  "ROS namespace in which IK solver advertises its service.")

(defparameter *wsg50* nil
  "Variable holding the interface of the WSG50 gripper on the left arm.")
(defparameter *wsg50-namespace* "/wsg_50"
  "ROS namespace in which the gripper controller advertises its services.")

(defun init-arm ()
  "Inits connection to beasty controller of LWR arm."
  (unless *arm* 
    (setf *arm* (make-beasty-interface *beasty-action-name* *arm-config* nil)))
  (when *arm*
    (setf *collision-fluent* (fl-funcall #'get-strongest-collision (state *arm*))))
  (unless *ik-proxy*
    (setf *ik-proxy* (cram-ik-proxy:make-ik-proxy-interface *ik-proxy-service-name*))))

(defun cleanup-arm ()
  "Stops LWR arm, and closes connection to beasty action server."
  (when *arm*
    (cleanup-beasty-interface *arm*)
    (setf *arm* nil))
  (when *collision-fluent*
    (setf *collision-fluent* nil))
  (when *ik-proxy*
    (cram-ik-proxy:cleanup-ik-proxy-interface *ik-proxy*)
    (setf *ik-proxy* nil)))

(defun init-ptu ()
  (unless *ptu*
    (setf *ptu* (cram-ptu:make-ptu-interface *ptu-action-name*))))

(defun cleanup-ptu ()
  (when *ptu*
    (cram-ptu:cleanup-ptu-interface *ptu*)
    (setf *ptu* nil)))

(defun init-wsg50 ()
  (unless *wsg50*
    (setf *wsg50* (cram-wsg50:make-wsg50-interface *wsg50-namespace*))))

(defun cleanup-wsg50 ()
  (when *wsg50*
    (setf *wsg50* nil)))