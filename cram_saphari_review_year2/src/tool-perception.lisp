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

(defvar *tool-perception* nil
  "ROS client triggering tool perception")
(defvar *tool-perception-service* "/detect_equipment"
  "ROS service name under which the tool detector is running.")

(defclass tool-percept ()
  ((id :initarg :id :reader id :type number)
   (pose-stamped :initarg :pose-stamped :reader pose-stamped 
                 :type cl-tf:pose-stamped))
  (:documentation "Internal representation of tool percept."))

(defun init-tool-perception ()
  "Inits connection to tool perception."
  (unless *tool-perception*
    (setf *tool-perception* 
          (roslisp:make-service-client
           *tool-perception-service*
           "saphari_msgs/PerceiveEquipment"))))

(defun trigger-tool-perception ()
  (let ((tools (when *tool-perception*
                 (with-fields (result) 
                     (roslisp:call-service *tool-perception*)
                   (mapcar (lambda (tool)
                             (with-fields (id pose) tool
                               (make-instance 
                                'tool-percept
                                :pose-stamped (cl-tf:msg->pose-stamped pose)
                                :id id))) (coerce result 'list))))))
    (separate-bowl-from-tools tools)))

(defun separate-bowl-from-tools (tools)
  (declare (type list tools))
  (let* ((bowl (find 0 tools :key #'id))
         (other-tools (remove bowl tools)))
    (values bowl other-tools)))
    