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

(defparameter *tf* nil)

(defun get-tf-listener ()
  (unless *tf* (setf *tf* (make-instance 'cl-tf:transform-listener)))
  *tf*)

(defun wait-for-and-lookup-transform (tf time source-frame target-frame)
  (cl-tf:wait-for-transform tf :time time :source-frame source-frame :target-frame target-frame)
  (cl-tf:lookup-transform tf :time time :source-frame source-frame :target-frame target-frame))

(defun get-head-shoulder-transform ()
  (wait-for-and-lookup-transform 
   (get-tf-listener) 0.0 "/head_xtion_rgb_optical_frame" "/shoulder_kinect_rgb_frame"))

(defun get-body-part (body body-part-label)
  "Returns the body part from the `body' with the label `body-part-label'."
  (find body-part-label (body-parts body) :key #'label))

(defun 3d-vector->list (3d-vector)
  "Converts the `3d-vector' into a list of numbers."
  (list (x 3d-vector) (y 3d-vector) (z 3d-vector)))

(defun list->3d-vector (list)
  "Expects a list of three numbers and converts it into a 3d-vector."
  (assert (= (length list) 3))
  (apply #'make-3d-vector list))

(defun make-buffer-fluent (fluent max-length)
  "Returns a fluent that contains a list with the `max-length' last states of `fluent'."
  (let ((buffer nil))
    (fl-funcall (lambda (fl)
                  (if (< (length (value buffer)) max-length)
                      (setf buffer (append buffer (list (value fl))))
                      (setf buffer (append (cdr buffer) (list (value fl))))))
                fluent)))

(defun distance-to-ray (ray point)
  "Returns the distance from the `point' to the line represented by `ray'."
  (let ((a-p (cl-transforms:v- (origin ray) point))
        (n (cl-transforms:v- (origin ray) (direction ray))))
    (cl-transforms:v-norm (cl-transforms:v- a-p (cl-transforms:v* n (cl-transforms:dot-product a-p n))))))

(defun is-behind-ray (ray point)
  "Returns nil if the `point' lays behind the plane created by the origin and direction of the `ray'."
  (let ((v1 (direction ray))
        (v2 (cl-transforms:v- point (origin ray))))
    (> 0
       (/ (cl-transforms:dot-product v1 v2) 
          (* (cl-transforms:v-norm v1) (cl-transforms:v-norm v2))))))

(defun equipments-in-direction (equipments ray)
  "Returns the `equipments' as an  alist associated with their distance to the `ray'. The list is sorted
by the distance with the equipment with the shortest distance as first element. The distance for equipments
that don't lay in the direction of the ray is nil."
  (let* ((head-shoulder-transform (get-head-shoulder-transform))
         (equip-dists (mapcar (lambda (equip)
                               (let ((point (cl-transforms:origin 
                                             (cl-transforms:transform head-shoulder-transform
                                                                      (pose-stamped equip)))))
                                 `(,(get-equipment-symbol (id equip)) . ,(unless (is-behind-ray ray point) 
                                                                           (distance-to-ray ray point)))))
                             equipments)))
    (sort equip-dists #'is-smaller-than :key #'cdr)))

(defun is-smaller-than (x1 x2)
  "Returns nil if `x1' is nil or `x1' >= `x2'."
  (when x1
    (if x2 (< x1 x2) t)))

