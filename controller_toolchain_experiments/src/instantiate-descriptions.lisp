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

(defun instantiate-class-description (description)
  (load-system description)
  (let ((class-instance (create-class description)))
    (initialize-class-instance class-instance description)
    class-instance))

(defun load-system (description)
  (asdf:load-system (to-keyword (read-value description :system-name t))))

(defun create-class (description)
  (make-instance 
   (to-symbol (read-value description :class-name t)
              (read-value description :system-name t))))

(defun initialize-class-instance (class-instance description)
  (labels ((get-slot-definitions (class-instance)
             (sb-mop:class-slots (class-of class-instance)))
           (get-slot-symbol (slot-definition)
             (with-slots (sb-pcl::name) slot-definition sb-pcl::name))
           (get-slot-name (slot-definition)
             (symbol-name (get-slot-symbol slot-definition))))
    (loop for slot-definition in (get-slot-definitions class-instance) do
      (multiple-value-bind (slot-init-value slot-init-value-p)
          (read-value description (to-keyword (get-slot-name slot-definition)))
        (when slot-init-value-p
          (eval `(with-slots (,(get-slot-symbol slot-definition)) ,class-instance
                   (setf ,(get-slot-symbol slot-definition) ,slot-init-value))))))))
          
(defun to-symbol (symbol-name package-name)
  (intern (string-upcase symbol-name) (string-upcase package-name)))

(defun to-keyword (symbol-name)
  (to-symbol symbol-name "keyword"))