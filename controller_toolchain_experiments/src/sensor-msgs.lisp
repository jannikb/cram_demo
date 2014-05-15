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

(defmethod roslisp-interfaces:to-msg ((data hash-table) (msg-type (eql 'sensor_msgs-msg:jointstate)))
  (let ((names '()) (positions '()) (velocities '()) (efforts '()))
    (alexandria:maphash-values 
     (lambda (joint-state)
       (with-slots (joint-name joint-position joint-velocity joint-effort) joint-state
         (push joint-name names)
         (push joint-position positions)
         (push joint-velocity velocities)
         (push joint-effort efforts)))
     data)
    (make-msg "sensor_msgs/jointstate" 
              (:stamp :header) (ros-time)
              :name (coerce (reverse names) 'vector)
              :position (coerce (reverse positions) 'vector)
              :velocity (coerce (reverse velocities) 'vector)
              :effort (coerce (reverse efforts) 'vector))))
              
(defmethod roslisp-interfaces:from-msg ((msg sensor_msgs-msg:jointstate))
  (let ((joint-states (make-hash-table :test 'equal)))
    (with-fields (name position velocity effort) msg
        (loop for index to (1- (length name)) do
          (setf (gethash (aref name index) joint-states)
                (cl-robot-models:make-joint-state
                 :joint-name (aref name index)
                 :joint-position (aref position index)
                 :joint-velocity (aref velocity index)
                 :joint-effort (aref effort index)))))
    joint-states))