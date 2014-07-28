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

(defparameter *head-kinect-transform* nil)

(defun init2 ()
  (unless *tf* (setf *tf* (make-instance 'cl-tf:transform-listener)))
  (cl-tf:wait-for-transform *tf* 
                            :source-frame "/head_xtion_rgb_optical_frame" 
                            :target-frame "/shoulder_kinect_rgb_frame")
  (setf *head-kinect-transform* (cl-tf:lookup-transform  *tf* 
                                                         :source-frame "/head_xtion_rgb_optical_frame" 
                                                         :target-frame "/shoulder_kinect_rgb_frame")))

(defun get-bodypart-with-label (human bodypart-label)
  (with-fields (bodyparts) human
    (find bodypart-label bodyparts 
          :key (lambda (part) (roslisp:with-fields (label) part label)))))

(defun get-bodypart-position (bodypart)
  (when bodypart
    (with-fields (centroid) bodypart
      centroid)))

(defun get-equipment-position (equipment)
  (when equipment
    (with-fields (pose) equipment
      (cl-transforms:origin (cl-transforms:transform-pose *head-kinect-transform* 
                                                          (cl-tf:msg->pose-stamped pose))))))

(defun get-equipment-with-label (equipments equipment-label)
  (with-fields (perceived) equipments
    (find equipment-label perceived
          :key (lambda (equipment) (roslisp:with-fields (id) equipment id)))))

(defun dist (point1 point2)
  (when (and point1 point2)
    (with-fields ((x1 x) (y1 y) (z1 z)) point1
      (with-fields ((x2 x) (y2 y) (z2 z)) point2
        (let ((xd (- x2 x1))
              (yd (- y2 y1))
              (zd (- z2 z1)))
          (sqrt (+ (sq xd) (sq yd) (sq zd))))))))

(defun add-to-buffer (buffer new-elem)
  (when (> (length buffer) 6) (setf buffer (butlast buffer)))
  (push new-elem buffer))

(defun line-fitting-3d (buffer)
  (let ((x-buffer nil)
        (y-buffer nil)
        (z-buffer nil))
    (mapcar (lambda (point) 
              (with-fields ((x0 x) (y0 y) (z0 z)) point
                (push x0 x-buffer)
                (push y0 y-buffer)
                (push z0 z-buffer)))
            buffer)
;    (format t "x-b ~a~%y-b ~a~%z-b ~a~%" x-buffer y-buffer z-buffer)
    (let* ((x-fitted (line-fitting-2d x-buffer))
           (y-fitted (line-fitting-2d y-buffer))
           (z-fitted (line-fitting-2d z-buffer))
           (rotation  (make-3d-vector (second x-fitted) 
                                      (second y-fitted) 
                                      (second z-fitted))))
      (list (make-3d-vector (car (last x-buffer)) (car (last y-buffer)) (car (last z-buffer)))
            (normalize-vector rotation)
            (abs (cl-transforms:v-dist (point32->3d-vector (first buffer)) 
                                       (point32->3d-vector (car (last buffer)))))
            (+ (third x-fitted) (third y-fitted) (third z-fitted))))))

(defun line-fitting-2d (values)
  (let ((numbers nil))
    (loop for i from 0.0d0 to (1- (length values))
          do (push i numbers))
    (multiple-value-bind (c0 c1 cov00 cov01 cov11 err)
        (gsll:linear-fit (grid:make-foreign-array 'double-float 
                                                  :dimensions (length values)
                                                  :initial-contents (reverse numbers))
                         (grid:make-foreign-array 'double-float 
                                                  :dimensions (length values)
                                                  :initial-contents values))
      (declare (ignore cov00 cov01 cov11))
      (list c0 c1 err))))

(defun show-direction (3d-fitting id)
  (let ((origin (first 3d-fitting))
        (direction (cl-transforms:v* (second 3d-fitting) (+ 0.1 (* 2 (third 3d-fitting))))))
    (publish-visualization-marker (cl-tf:make-pose-stamped "/shoulder_kinect_rgb_frame" 
                                                           (ros-time) 
                                                           (cl-transforms:make-identity-vector)
                                                           (cl-transforms:make-identity-rotation)) 
                                  :points (vector (cl-tf:point->msg origin)
                                                  (cl-tf:point->msg (make-3d-vector (+ (x origin)
                                                                                       (x direction))
                                                                                    (+ (y origin)
                                                                                       (y direction))
                                                                                    (+ (z origin)
                                                                                       (z direction)))))
                                  :scale-x 0.05 :scale-y 0.1
                                  :id id)))

(defun normalize-vector (vector)
  (let ((length (sqrt (+ (sq (x vector)) (sq (y vector)) (sq (z vector))))))
    (make-3d-vector (/ (x vector) length)
                    (/ (y vector) length)
                    (/ (z vector) length))))

(defun sq (value)
  (* value value))

(defun distance-to-ray (origin direction point)
  (let ((a-p (cl-transforms:v- origin point))
        (n (cl-transforms:v- origin direction)))
    (cl-transforms:v- a-p (cl-transforms:v* n (cl-transforms:dot-product a-p n)))))

(defun point32->3d-vector (msg)
  (with-fields ((x0 x) (y0 y) (z0 z)) msg
    (make-3d-vector x0 y0 z0)))
      
(defun is-behind-ray (origin direction point)
  (let ((v1 direction)
        (v2 (cl-transforms:v- point origin)))
    (> 0
       (/ (cl-transforms:dot-product v1 v2) 
          (* (cl-transforms:v-norm v1) (cl-transforms:v-norm v2))))))

