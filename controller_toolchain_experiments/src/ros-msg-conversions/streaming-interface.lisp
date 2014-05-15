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

(in-package :msg-conversions)

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
;;;         (create-ros-streamer in-descr out-descr #'prin1-to-string)))
;;;
;;;     (start-ros-streamer *streamer*)
;;;     (ros-streamer-running-p *streamer*)
;;;       --> T
;;;     Meanwhile, the stream is running, and one can analyse the setup from the
;;;     console:
;;;       rostopic echo /out_topic
;;;       rostopic pub /in_topic std_msgs/Float64 "ata: 1.0}"
;;;
;;;     (stop-ros-streamer *streamer*)
;;;     (ros-streamer-running-p *streamer*)
;;;       --> NIL
;;;
;;;     (start-ros-streamer *streamer*)
;;;     (ros-streamer-running-p *streamer*)
;;;       --> T
;;;     (cleanup-ros-streamer *streamer*)
;;;
;;;     Now, all topics are down and *streamer* should be discarded:
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
 Assumes corresponding message conversions have been loaded."
  (declare (type ros-topic-description topic-descr))
  (with-slots (lisp-package msg-type) topic-descr
    (intern (string-upcase msg-type) (string-upcase lisp-package))))

(defun advertise-publication (topic-descr)
  "Advertises the topic as described in `topic-descr'. Assumes corresponding
 message package and message conversions have been loaded."
  (declare (type ros-topic-description topic-descr))
  (with-slots (topic) topic-descr
    (values (advertise topic (msg-type-symbol topic-descr)) topic)))
           
(defstruct ros-streamer
  "Class holding all necessary data to setup a ROS streamer."
  (callback (error "Must supply :callback in ros-streamer.")
   :read-only t :type function)
  (out-topic (error "Must supply :out-topic in ros-streamer.")
   :read-only t :type string)
  (subscription-descr (error "Must supply :subscription-descr in ros-streamer.")
   :read-only t :type ros-topic-description)
  (subscription nil :read-only nil))
  
(defun create-ros-streamer (in-topic-descr out-topic-descr function)
  "Creates a ROS streamer with topic descriptions `in-topic-descr' and
 `out-topic-descr'. `Function' denotes a function designator which is the
 actual computation to be performed."
  (declare (type ros-topic-description in-topic-descr out-topic-descr)
           (type function function))
  (multiple-value-bind (publication out-topic) 
      (advertise-publication out-topic-descr)
    (let ((callback (lambda (msg)
                      (publish publication
                               (to-msg (funcall function (from-msg msg))
                                       (msg-type-symbol out-topic-descr))))))
      (make-ros-streamer
       :callback callback :out-topic out-topic :subscription-descr in-topic-descr))))
       
(defun cleanup-ros-streamer (ros-streamer)
  "Terminates all ROS connections of `ros-streamer.' NOTE: Renders `ros-streamer'
 useless for any further communication. Should be discared afterwards."
  (declare (type ros-streamer ros-streamer))
  (when (ros-streamer-running-p ros-streamer)
    (stop-ros-streamer ros-streamer))
  (unadvertise (ros-streamer-out-topic ros-streamer)))

(defun ros-streamer-running-p (ros-streamer)
  "Predicate to check whether `ros-streamer' has all communication set up
 to stream data."
  (declare (type ros-streamer ros-streamer))
  (not (not (ros-streamer-subscription ros-streamer))))

(defun start-ros-streamer (ros-streamer)
  "Starts all communication of `ros-streamer'."
  (declare (type ros-streamer ros-streamer))
  (unless (ros-streamer-running-p ros-streamer)
    (with-slots (callback subscription-descr) ros-streamer
      (setf (ros-streamer-subscription ros-streamer)
            (subscribe 
             (ros-topic-description-topic subscription-descr) 
             (msg-type-symbol subscription-descr) callback)))))
           
(defun stop-ros-streamer (ros-streamer)
 "Stops all communication of `ros-streamer'."
  (declare (type ros-streamer ros-streamer))
  (when (ros-streamer-running-p ros-streamer)
    (with-slots (subscription) ros-streamer
      (unsubscribe subscription)
      (setf subscription nil))))