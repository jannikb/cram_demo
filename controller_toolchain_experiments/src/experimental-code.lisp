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

(defparameter *streamer* nil)

(defun joint-controller-streamer-experiment ()
  (setf *streamer* (get-controller-streamer))
  (roslisp-streamer:start-ros-streamer *streamer*))

(defun get-controller-streamer ()
  (let ((desired-joint-state
          (make-instance 'cl-robot-models:joint-state
                         :joint-name "l_elbow_flex_joint"
                         :joint-position -1.1)))
    (roslisp-streamer:make-ros-streamer
     :in-topic-descr (roslisp-streamer:make-ros-topic-description
                      :lisp-package "sensor_msgs-msg"
                      :msg-type "jointstate"
                      :topic "/joint_states")
     :out-topic-descr (roslisp-streamer:make-ros-topic-description
                       :lisp-package "std_msgs-msg"
                       :msg-type "float64"
                       :topic "/l_elbow_flex_velocity_controller/command")
    :function (lambda (joint-states)
                (cl-robot-controllers:compute-command 
                 (cl-robot-controllers:make-pid-controller
                  :p-controller (cl-robot-controllers:make-p-controller :p-gain 1000)
                  :i-controller (cl-robot-controllers:make-i-controller :i-gain 450
                                                                        :i-max 400
                                                                        :i-min -400
                                                                        :dt 0.004)
                  :d-controller (cl-robot-controllers:make-d-controller :d-gain 100)) 
                 (serapeum:get-association 
                  joint-states (cl-robot-models:joint-name desired-joint-state))
                 desired-joint-state)))))