;;; Copyright (c) 2014, Jannik Buckelo <jannikbu@cs.uni-bremen.de>
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

(in-package :cram-saphari-review-2)

(defvar *human-subscriber* nil)

(defvar *equipment-subscriber* nil)

(defvar *visualization-publisher* nil)

;;(defvar *human-buffer* nil)

;; TODO: remove this eventually
(defvar *human-fluent*)
(defvar *equipment-fluent*)
(defun init ()
  (setf *human-fluent*
        (nth-value 1 (make-human-subscriber)))
  (setf *equipment-fluent*
        (nth-value 1 (make-equipment-subscriber)))
  (make-visualization-publisher))
(defun test ()
  (defparameter *buffer* (make-buffer-fluent *human-fluent* 5))
  (defparameter *direction* (fl-funcall (lambda (buffer)
                                          (when (> (length buffer) 2)
                                            (get-direction buffer :lefthand)))
                                          *buffer*)))
                                                           

(defun make-human-subscriber ()
  "Creates a subscriber for the topic '/saphari/human' and a fluent that is
filled with the content from that topic. Returns the subscriber and the fluent."
  (let ((human-fl (make-fluent)))
    (setf *human-subscriber* 
          (subscribe "/saphari/human" "saphari_msgs/Human" 
                     (lambda (msg) 
                       (setf (value human-fl) (from-msg msg)))))
    (values *human-subscriber* human-fl)))

(defun make-equipment-subscriber ()
  "Creates a subscriber for the topic '/detect_equipment' and a fluent that is
filled with the content from that topic. Returns the subscriber and the fluent."
  (let ((equipment-fl (make-fluent)))
    (save-head-shoulder-transform)
    (setf *equipment-subscriber* 
          (subscribe "/detect_equipment" "saphari_msgs/PerceivedEquipment" 
                     (lambda (msg) (setf (value equipment-fl) (from-msg msg)))))
    (values *equipment-subscriber* equipment-fl)))

(defun make-visualization-publisher ()
  "Creates and returns an advertiser for the topic 'visualization_msgs/Marker'."
  (setf *visualization-publisher* (advertise "/visualization_marker" "visualization_msgs/Marker")))

(defun cleanup ()
  "Unsubscribes from all topics"
  (when *human-subscriber* (unsubscribe *human-subscriber*))
  (setf *human-subscriber* nil)
  (when *equipment-subscriber* (unsubscribe *equipment-subscriber*))
  (setf *equipment-subscriber* nil)
  (setf *visualization-publisher* nil))


  