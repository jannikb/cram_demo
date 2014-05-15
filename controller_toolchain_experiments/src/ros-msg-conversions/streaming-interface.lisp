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
;;;             (make-instance
;;;              'ros-topic-description
;;;              :lisp-package "std_msgs-msg"
;;;              :msg-type "float64"
;;;              :topic "in_topic"))
;;;           (out-descr
;;;             (make-instance
;;;              'ros-topic-description
;;;              :lisp-package "std_msgs-msg"
;;;              :msg-type "string"
;;;              :topic "out_topic")))
;;;       (defparameter *streamer* (make-ros-streamer in-descr out-descr #'prin1-to-string)))
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
;;;     Now, all topics are down and *streamer* should be discarded.
;;;

(defclass ros-topic-description () 
  ((lisp-package :initarg :lisp-package 
                 :initform (error "Must supply :lisp-package in ros-topic-description.")
                 :reader lisp-package :type string)
   (msg-type :initarg :msg-type
             :initform (error "Must supply :msg-type in ros-topic-description.")
             :reader msg-type :type string)
   (topic :initarg :topic
          :initform (error "Must supply :topic in ros-topic-description.")
          :reader topic :type string))
  (:documentation "A class describing a ROS topic (from a LISP perspective)."))

(defun msg-type-symbol (topic-descr)
  (declare (type ros-topic-description topic-descr))
  (with-slots (lisp-package msg-type) topic-descr
    (intern (string-upcase msg-type) (string-upcase lisp-package))))

(defun advertise-publication (topic-descr)
  (declare (type ros-topic-description topic-descr))
  (with-slots (topic) topic-descr
    (values (advertise topic (msg-type-symbol topic-descr)) topic)))
           
(defclass ros-streamer () 
  ((callback :initarg :callback 
             :initform (error "Must supply :callback in ros-streamer.")
             :reader callback :type function)
   (out-topic :initarg :out-topic
              :initform (error "Must supply :out-topic in ros-streamer.")
              :reader out-topic :type string)
   (subscription-descr :initarg :subscription-descr
                       :initform (error "Must supply :subscription-descr in ros-streamer.")
                       :reader subscription-descr :type ros-topic-description)
   (subscription :initform nil :accessor subscription :type subscriber))
  (:documentation "Class holding all necessary data to setup a ROS streamer."))

(defun make-ros-streamer (in-topic-descr out-topic-descr function)
  (declare (type ros-topic-description in-topic-descr out-topic-descr)
           (type function function))
  (multiple-value-bind (publication out-topic) 
      (advertise-publication out-topic-descr)
    (let ((callback (lambda (msg)
                      (publish publication
                               (to-msg (funcall function (from-msg msg))
                                       (msg-type-symbol out-topic-descr))))))
      (make-instance 
       'ros-streamer 
       :callback callback :out-topic out-topic :subscription-descr in-topic-descr))))
       
(defun cleanup-ros-streamer (ros-streamer)
  (declare (type ros-streamer ros-streamer))
  (when (ros-streamer-running-p ros-streamer)
    (stop-ros-streamer ros-streamer))
  (unadvertise (out-topic ros-streamer)))

(defun ros-streamer-running-p (ros-streamer)
  (declare (type ros-streamer ros-streamer))
  (not (not (subscription ros-streamer))))

(defun start-ros-streamer (ros-streamer)
  (declare (type ros-streamer ros-streamer))
  (unless (ros-streamer-running-p ros-streamer)
    (with-slots (callback subscription-descr) ros-streamer
      (setf (subscription ros-streamer)
            (subscribe 
             (topic subscription-descr) (msg-type-symbol subscription-descr) callback)))))
           
(defun stop-ros-streamer (ros-streamer)
  (declare (type ros-streamer ros-streamer))
  (when (ros-streamer-running-p ros-streamer)
    (unsubscribe (subscription ros-streamer))
    (setf (subscription ros-streamer) nil)))
