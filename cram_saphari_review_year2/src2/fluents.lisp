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


(defvar *sub-human* nil)

(defvar *sub-equipment* nil)

(defvar *visualization-pub* nil)


#|
(defparameter *left-hand* (make-fluent))

(defparameter *left-hand-position* (fl-funcall #'get-bodypart-position *left-hand*))

(defparameter *scalpel* (make-fluent))

(defparameter *scalpel-position* (fl-funcall #'get-equipment-position *scalpel*))

(defparameter *dist-left-hand-scalpel* (fl-funcall #'dist *left-hand-position* *scalpel-position*))

(defparameter *dist-pub* (fl-funcall (lambda (dist) (when *pub* (publish-msg *pub* :data dist))) *dist-left-hand-scalpel*))

|#

(defparameter *equipments* (list :bowl (make-fluent) :clamp_big (make-fluent) 
                                 :clamp_small (make-fluent) :scalpel (make-fluent) 
                                 :scissors (make-fluent)))

(defparameter *bodyparts* (list :lefthand (make-fluent) :righthand (make-fluent)))

(defvar *equipment-positions* nil)

(defvar *bodypart-positions* nil)

(defvar *bodypart-equipment-distances* nil)

(defvar *bodypart-directions* nil)

(defvar *point-buffer* nil)

(defun init ()
  (init2)

  ;; initialize the equipment position fluents
  (loop for (key value) on *equipments* by #'cddr
        do (push (fl-funcall #'get-equipment-position value)
                 *equipment-positions*)
           (push key *equipment-positions*))

  ;; initialize the bodypart position fluents
  (loop for (key value) on *bodyparts* by #'cddr
        do (push (fl-funcall #'get-bodypart-position value)
                 *bodypart-positions*)
           (push key *bodypart-positions*))  

  ;; initialize the fluents for the distance between a bodypart and a equipment
  (loop for (body-key body-value) on *bodypart-positions* by #'cddr
        do (let ((equip-dists nil))
             (loop for (equip-key equip-value) on *equipment-positions* by #'cddr
                   do (push (fl-funcall #'dist body-value equip-value)
                            equip-dists)
                      (push equip-key equip-dists))
             (push equip-dists *bodypart-equipment-distances*)
             (push body-key *bodypart-equipment-distances*)))

  (loop for (key value) on *bodypart-positions* by #'cddr
        do (let ((buffer (make-fluent)))
             (setf *point-buffer* 
                   (fl-funcall (lambda (elem) 
                                 (when elem
                                   (setf (value buffer) (add-to-buffer (value buffer) elem)))) 
                               value))
             (push (fl-funcall (lambda (b) (when (> (length b) 1) (line-fitting-3d b))) buffer) 
                   *bodypart-directions*)
             (push key *bodypart-directions*)))

  ;; set the subscriber and publisher
  (setf *sub-human* 
        (subscribe "/saphari/human" "saphari_msgs/Human" 
                   (lambda (msg) (set-fluents *bodyparts* msg
                                              #'get-bodypart-with-label
                                              'saphari_msgs-msg:bodypart)))
        *sub-equipment* 
        (subscribe "/detect_equipment" "saphari_msgs/PerceivedEquipment"
                   (lambda (msg) (set-fluents *equipments* msg
                                              #'get-equipment-with-label
                                              'saphari_msgs-msg:equipment)))
         *visualization-pub* (advertise "/visualization_marker" "visualization_msgs/Marker")))

(defun stop ()
  (setf *equipment-positions* nil)
  (setf *bodypart-positions* nil)
  (setf *bodypart-equipment-distances* nil)
  (setf *bodypart-directions* nil)
  (unsubscribe *sub-human*)
  (unsubscribe *sub-equipment*))

 

(defun set-fluents (plist msg get-part-fn msg-type)
  (loop for (key value) on plist by #'cddr
        do (let ((part (funcall get-part-fn msg (symbol-code msg-type key))))
             (when part
               (setf (value (getf plist key)) part)))))



