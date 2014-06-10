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

(in-package :controller-experiments)

;;;
;;; READING IN YAML DESCRIPTION OF PR2 FINGERTIP
;;; PRESSURE SENSORS
;;;

(defparameter *pr2-pressure-sensor-description-dir*
  "/home/georg/ros/hydro/catkin_ws/src/iai_robots/iai_pr2_description/sensors")

(defparameter *pr2-pressure-sensor-description-filename*
  "pr2-fingertip-pressure-sensors.yaml")

(defun read-pressure-sensor-descriptions (directory filename)
  (post-process-gripper-descriptions
   (parse-fingertips-yaml directory filename)))

(defun parse-fingertips-yaml (directory filename)
  (yaml:parse (make-pathname :directory directory :name filename)))

(defun post-process-gripper-descriptions (gripper-descrs)
  (maphash (lambda (gripper-key gripper-descr)
             (setf (gethash gripper-key gripper-descrs)
                   (post-process-gripper-description gripper-descr)))
           gripper-descrs)
  gripper-descrs)

(defun post-process-gripper-description (gripper-descr)
  (maphash (lambda (finger-key finger-descr)
             (setf (gethash finger-key gripper-descr)
                   (post-process-finger-description finger-descr)))
           gripper-descr)
  gripper-descr)

(defun post-process-finger-description (finger-descr)
  (maphash (lambda (sensor-key sensor-descr)
             (setf (gethash sensor-key finger-descr)
                   (post-process-sensor-description sensor-descr)))
           finger-descr)
  finger-descr)

(defun post-process-sensor-description (sensor-descr)
  (let ((center (get-association sensor-descr "center"))
        (halfside1 (get-association sensor-descr "halfside1"))
        (halfside2 (get-association sensor-descr "halfside2")))
    (if (and center halfside1 halfside2)
        (add-associations sensor-descr 
                          "center" (post-process-point-descr center)
                          "halfside1" (post-process-point-descr halfside1)
                          "halfside2" (post-process-point-descr halfside2))
        (progn
          (warn "Sensor description has missing keys. Not post processing.")
          sensor-descr))))

(defun post-process-point-descr (point-descr)
  (let ((x (get-association point-descr "x"))
        (y (get-association point-descr "y"))
        (z (get-association point-descr "z")))
    (if (and x y z)
        (cl-transforms:make-3d-vector x y z)
        (progn
          (warn "Point description has missing keys. Not transforming.")
          point-descr))))