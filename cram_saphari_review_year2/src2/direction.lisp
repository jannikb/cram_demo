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

(defclass ray ()
  ((origin :initarg :origin
           :reader origin)
   (direction :initarg :direction
              :reader direction)
   (size :initarg :size
           :reader size))
  (:documentation "Representation of a ray. `origin' is the start and `direction' the 
direction of the ray."))

(defun get-direction (body-buffer body-part-label)
  "Returns a ray that represents the direction in which the body part is moving.
`body-buffer' is a list of at least 3 configurations of the body.
`body-part-label' is the label of the bodypart as a symbol, e.g. :lefthand."
  (let ((body-part-poses (mapcar (lambda (body)
                                   (get-body-part-centroid body body-part-label))
                                 body-buffer)))
    (fit-line body-part-poses)))

(defun fit-line (points)
  "Returns a ray which is fitted into the given points.
`points' is a list of at least 3 points.

To fit the ray into the points orthogonal distance regression is used to calculate the direction
and the centroid of the points marks the start of the ray."
  (let* ((centroid (cl-transforms:v* (reduce #'cl-transforms:v+ points) (/ 1 (length points))))
         (m (mapcar (lambda (point)
                      (3d-vector->list (cl-transforms:v- point centroid)))
            points)))
    (multiple-value-bind (ort-matrix singular-values singular-vectors)
        (gsl:sv-decomposition (grid:make-foreign-array 'double-float 
                                                       :dimensions '(3 (length points)) 
                                                       :initial-contents m))
      (declare (ignore ort-matrix))
      (multiple-value-bind (direction length) 
          (direction-and-length singular-values singular-vectors)
        (make-instance 'ray
                       :origin centroid
                       :direction direction
                       :size length)))))

(defun direction-and-length (singular-values singular-vectors)
  "Returns the singular vector which corresponds with the largest singular value.
`singular-values' list of the singular values.
`singular-vectors' list of the singula vectors which are list of numbers."
  (let ((dir-len (reduce (lambda (s1 s2) (if (> (car s1) (car s2)) s1 s2))
                         (mapcar (lambda (s-value s-vector) `(,s-value . ,s-vector)) 
                                 (grid:contents singular-values)
                                 (grid:contents (grid:transpose singular-vectors))))))
    (values (list->3d-vector (cdr dir-len)) (car dir-len))))
    
