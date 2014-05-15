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

(in-package :roslisp-streamer)

;;;
;;; IDEA BEHIND ROS STREAMERS:
;;;
;;;   Certain setups, e.g. controllers, image segmentation, call for a ROS
;;;   setup with a single topic in and a single topic out connection. I'd like
;;;   to refer to this as a streaming application:
;;;
;;;                 -----------------
;;;   /in_topic --> | STREAMING APP | --> /out_topic
;;;                 -----------------
;;;
;;;   Internally, the streaming application itself typically consists of 3 steps:
;;;     (1) convert the ROS message into another LISP object
;;;     (2) apply a function on this object
;;;     (3) convert the result of that computation back to a ROS message
;;;
;;;   I call this the callback of the stream which is sandwiched between a subscriber
;;;   and a publisher. This file provides functionality to describe ROS topic
;;;   connections, and create/start/stop ROS streamers.
;;;
;;;   A quick example:
;;;
;;;     (let ((in-descr
;;;             (make-ros-topic-description
;;;              :lisp-package "std_msgs-msg"
;;;              :msg-type "float64"
;;;              :topic "in_topic"))
;;;           (out-descr
;;;             (make-ros-topic-description
;;;              :lisp-package "std_msgs-msg"
;;;              :msg-type "string"
;;;              :topic "out_topic")))
;;;       (defparameter *streamer* 
;;;         (make-ros-streamer :in-topic-descr in-descr 
;;;                            :out-topic-descr out-descr 
;;;                            :function #'prin1-to-string)))
;;;
;;;     (start-ros-streamer *streamer*)
;;;     (ros-streamer-up-p *streamer*)
;;;       --> T
;;;
;;;     Meanwhile, the stream is running. One can analyse the setup from
;;;     the console:
;;;       rostopic echo /out_topic
;;;       rostopic pub /in_topic std_msgs/Float64 "ata: 1.0}"
;;;
;;;     (stop-ros-streamer *streamer*)
;;;     (ros-streamer-running-p *streamer*)
;;;       --> NIL
;;;
;;;     Now, all topics are down:
;;;       rostopic list
;;;

(defstruct ros-topic-description
  "A class describing a ROS topic (from a LISP perspective)."
  (lisp-package (error "Must supply :lisp-package in ros-topic-description.")
   :read-only t :type string)
  (msg-type (error "Must supply :msg-type in ros-topic-description.")
   :read-only t :type string)
  (topic (error "Must supply :topic in ros-topic-description.")
   :read-only t :type string))

(defun msg-type-symbol (topic-descr)
  "Returns the symbol denoting the message type of `topic-descr'.
 Assumes corresponding message packages have been loaded."
  (declare (type ros-topic-description topic-descr))
  (with-slots (lisp-package msg-type) topic-descr
    (intern (string-upcase msg-type) (string-upcase lisp-package))))

(defun advertise-topic (topic-descr)
  "Publishes to `topic-descr'. Assumes corresponding message packages
 have been loaded."
  (declare (type ros-topic-description topic-descr))
  (advertise (ros-topic-description-topic topic-descr)
             (msg-type-symbol topic-descr)))

(defun subscribe-topic (topic-descr callback)
  "Makes a subscription to `topic-descr' with `callback'. Assumes that
 corresponding message packages have been loaded."
  (subscribe
   (ros-topic-description-topic topic-descr)
   (msg-type-symbol topic-descr) callback))
           
(defstruct ros-streamer
  "Class holding all necessary data to setup a ROS streamer."
  (function (error "Must supply :function in ros-streamer.")
   :read-only t :type function)
  (in-topic-descr (error "Must supply :in-topic-descr in ros-streamer.")
   :read-only t :type ros-topic-description)
  (out-topic-descr (error "Must supply :out-topic-descr in ros-streamer.")
   :read-only t :type ros-topic-description)
  (publication nil :read-only nil)
  (subscription nil :read-only nil))
  
;;;
;;; EXPORTED API
;;;

(defun ros-streamer-up-p (ros-streamer)
  "Predicate to check whether `ros-streamer' has all communication up
 to stream data."
  (declare (type ros-streamer ros-streamer))
  (and (ros-streamer-out-up-p ros-streamer)
       (ros-streamer-in-up-p ros-streamer)))

(defun start-ros-streamer (ros-streamer)
  "Starts all communication of `ros-streamer'."
  (declare (type ros-streamer ros-streamer))
  (start-out-communication ros-streamer)
  (start-in-communication ros-streamer))

(defun stop-ros-streamer (ros-streamer)
 "Stops all communication of `ros-streamer'."
  (declare (type ros-streamer ros-streamer))
  (stop-in-communication ros-streamer)
  (stop-out-communication ros-streamer))

(define-condition ros-streamer-error (simple-error) ())

;;;
;;; INTERNAL API
;;;

(defun ros-streamer-out-up-p (ros-streamer)
  "Predicate to check whether the publisher of `ros-streamer' has
 been advertised."
  (declare (type ros-streamer ros-streamer))
  (not (not (ros-streamer-publication ros-streamer))))

(defun ros-streamer-in-up-p (ros-streamer)
  "Predicate to check whether the subscription of `ros-streamer'
 has been established."
  (declare (type ros-streamer ros-streamer))
  (not (not (ros-streamer-subscription ros-streamer))))

(defun start-out-communication (ros-streamer)
  "Starts the publisher of `ros-streamer'. If it is already
 running, then nothing happens."
  (declare (type ros-streamer ros-streamer))
  (unless (ros-streamer-out-up-p ros-streamer)
    (with-slots (publication out-topic-descr) ros-streamer
      (setf publication (advertise-topic out-topic-descr)))))

(defun start-in-communication (ros-streamer)
  "Starts the subscription of `ros-streamer'. If the subscription
 was already up, nothing happens. Note: Assumes that the publisher
 is already running. Also assumes that the corresponding message
 conversion packages have been loaded."
  (declare (type ros-streamer ros-streamer))
  (unless (ros-streamer-out-up-p ros-streamer)
    (error 
     'ros-streamer
     :format-control "Out-comm of ROS-streamer not running when starting in-comm: ~a."
     :format-arguments (list ros-streamer)))
  (unless (ros-streamer-in-up-p ros-streamer)
    (with-slots (in-topic-descr out-topic-descr
                 subscription publication function) ros-streamer
      (setf subscription (subscribe-topic 
                          in-topic-descr
                          (lambda (msg)
                            (publish publication
                                     (to-msg (funcall function (from-msg msg))
                                             (msg-type-symbol out-topic-descr)))))))))
        
(defun stop-in-communication (ros-streamer)
  "Stops the subscription of `ros-streamer', if necessary."
  (declare (type ros-streamer ros-streamer))
  (when (ros-streamer-in-up-p ros-streamer)
    (with-slots (subscription) ros-streamer
      (unsubscribe subscription)
      (setf subscription nil))))

(defun stop-out-communication (ros-streamer)
  "Stops the publisher of `ros-streamer', if necessary.
 Note: Assume that the subscription of `ros-streamer' is already
 done."
  (declare (type ros-streamer ros-streamer))
  (when (ros-streamer-in-up-p ros-streamer)
    (error 
     'ros-streamer
     :format-controller "In-comm of ROS-streamer still running when stopping out-comm: ~a"
     :format-arguments (list ros-streamer)))
  (when (ros-streamer-out-up-p ros-streamer)
    (with-slots (publication out-topic-descr) ros-streamer
      (unadvertise (ros-topic-description-topic out-topic-descr))
      (setf publication nil))))