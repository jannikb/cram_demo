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

(defvar *human-perception* nil
  "ROS subscriber listening to results of human perception.")
(defvar *human-fluent* nil
  "Fluent holding currently perceived body parts.")

(defparameter *human-perception-topic* "/saphari/human"
  "ROS topic name on which perception publishes human body parts.")

(defun init-human-perception ()
  "Inits connection to human perception."
  (unless *human-fluent*
    (setf *human-fluent* (cpl-impl:make-fluent :value (make-instance 'human-body))))
  (unless *human-perception*
    (setf *human-perception* (subscribe *human-perception-topic*
                                        "saphari_msgs/Human"
                                        (lambda (msg)
                                          (setf (cpl-impl:value *human-fluent*)
                                                (from-msg msg)))))))

(defun cleanup-human-perception ()
  "Cleans up alls stuff related to human perception."
  (setf *human-fluent* nil)
  (setf *human-perception* nil))

(defgeneric from-msg (msg &key &allow-other-keys)
  (:documentation "Creates the corresponding Common Lisp datastructure to the ROS msg `msg'. Dispatch 
 between method is based on the type of `msg'. However, individual method may accept
 further key-parameter."))

(defmethod from-msg ((msg saphari_msgs-msg:human) &key &allow-other-keys)
  "Creates and returns an instance of type 'human-body' filled with content of `msg'."
  (with-fields (header bodyparts) msg
    (with-fields (frame_id) header
      (make-human-body 
       :body-parts (mapcar (lambda (bodypart)
                             (from-msg bodypart :frame-id frame_id))
                           (coerce bodyparts 'list))))))

(defmethod from-msg ((msg saphari_msgs-msg:bodypart) &key frame-id &allow-other-keys)
  "Creates and returns an instance of type 'human-body-part' filled with content from
 `msg' and `frame-id'."
  (with-fields (id label centroid radius) msg
    (make-human-body-part
     :shape (make-sphere :centroid (from-msg centroid) :radius radius :frame-id frame-id)
     :id id :label (get-body-part-symbol label))))
    
(defmethod from-msg ((msg geometry_msgs-msg:Point32) &key &allow-other-keys)
  "Creates and returns an instance of type '3d-vector' filled with content from `msg'."
  (with-fields (x y z) msg
    (cl-transforms:make-3d-vector x y z)))

(defun get-body-part-symbol (body-part-code)
  "Finds and returns the symbol corresponding to the body-part label `body-part-code' (type
 number), as defined in 'saphari_msgs-msg:BodyPart'. If nothing is found, returns 'nil'."
  (declare (type number body-part-code))
  (let ((body-part-symbol-code
          (rassoc body-part-code (symbol-codes 'saphari_msgs-msg:BodyPart))))
    (when body-part-symbol-code
      (car body-part-symbol-code))))

(defun calculate-human-distance (body)
  (declare (type cl-human-shapes::human-body body))
  (let* ((body-parts (body-parts body))
         (body-part-distances (mapcar (lambda (part)
                                        (cl-transforms:x (centroid (shape part)))) body-parts)))
    (reduce (lambda (a b) (if (< a b) a b)) body-part-distances :initial-value 100)))
