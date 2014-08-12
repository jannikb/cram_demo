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
(defvar *lh-eq-reach-pub*)
(defvar *lh-eq-absolute-pub*)
(defun init ()
  (setf *human-fluent*
        (nth-value 1 (make-human-subscriber)))
  (setf *equipment-fluent*
        (nth-value 1 (make-equipment-subscriber)))
  (make-visualization-publisher)
  (setf *lh-eq-reach-pub* (make-equipment-publishers "reaching_distance_lh"))
  (setf *lh-eq-absolute-pub* (make-equipment-publishers "absolute_distance_lh")))
(defvar *buffer* nil)
(defvar *direction* nil)
(defvar *reaching-distances*)
(defvar *absolute-distances*)
(defun test ()
  (setf *buffer* (make-buffer-fluent *human-fluent* 5))
  (setf *direction* (fl-funcall (lambda (buffer)
                                          (when (> (length buffer) 2)
                                            (get-direction buffer :lefthand)))
                                          *buffer*))
  (setf *reaching-distances* 
        (fl-funcall (lambda (eq-fl dir) 
                      (when dir
                        (equipments-in-direction eq-fl dir)))
                    *equipment-fluent* *direction*))
  (setf *absolute-distances*
        (fl-funcall (lambda (eq-fl human-fl) 
                      (when (and eq-fl human-fl)
                        (let ((centroid (get-body-part-centroid human-fl :lefthand))
                              (head-shoulder-transform (get-head-shoulder-transform)))
                          (mapcar (lambda (eq)
                                    (cons (get-equipment-symbol (id eq))
                                          (cl-transforms:v-dist (cl-transforms:origin 
                                                                 (cl-transforms:transform 
                                                                  head-shoulder-transform
                                                                  (pose-stamped eq)))
                                                                centroid)))
                                  eq-fl))))
                    *equipment-fluent* *human-fluent*)))
(defvar *direction-vis* nil)
(defvar *reaching-distance-vis*)
(defvar *absolute-distance-vis*)
(defun test-vis ()
  (setf *direction-vis* (fl-funcall (lambda (vis-pub dir id)
                                      (when (and vis-pub dir)
                                        (show-direction vis-pub dir id)))
                                    *visualization-publisher* *direction* 1))
  (setf *reaching-distance-vis* 
        (fl-funcall #'publish-equipment-values *lh-eq-reach-pub* *reaching-distances* 5))
  (setf *absolute-distance-vis*
        (fl-funcall #'publish-equipment-values *lh-eq-absolute-pub* *absolute-distances* 5)))
                                                           

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
    (setf *equipment-subscriber* 
          (subscribe "/detect_equipment" "saphari_msgs/PerceivedEquipment" 
                     (lambda (msg) 
                       (setf (value equipment-fl) 
                             (update-equipments (from-msg msg) (value equipment-fl))))))
    (values *equipment-subscriber* equipment-fl)))

(defun make-visualization-publisher ()
  "Creates and returns an advertiser for the topic 'visualization_msgs/Marker'."
  (setf *visualization-publisher* (advertise "/visualization_marker" 
                                             "visualization_msgs/Marker")))

(defun make-equipment-publishers (name)
  "Creates an advertiser for every equipment and returns them in an alist with the 
label of the equipment.
   `body-part' specifies the body part which the distance should be relativ to."
  (let ((equipment-labels '(:bowl :clamp_big :clamp_small :scalpel :scissors)))
    (mapcar (lambda (label)
              (cons label 
                    (advertise (format nil "/equipment_distances/~a/~a" name label)
                               "std_msgs/Float64")))
            equipment-labels)))

;;rqt_plot /equipment_distances/distance_to_LEFTHAND/BOWL/data /equipment_distances/distance_to_LEFTHAND/CLAMP_BIG/data /equipment_distances/distance_to_LEFTHAND/CLAMP_SMALL/data /equipment_distances/distance_to_LEFTHAND/SCALPEL/data /equipment_distances/distance_to_LEFTHAND/SCISSORS/data
(defun publish-equipment-values (equip-pubs equip-values max-value)
  "Publishes the values for the eqipments in `equip-dists' to the corresponding topics in
`equip-pubs'. 
  `equip-pubs' is an alist with the equipment labels and the advertisers.
  `equip-values' is an alist with the equipment labels and their values.
  `max-value' is the distance that will be published if a distance is greater than 
`max-value' or nil." 
  (mapcar (lambda (equip-pub)
            (let* ((equip-value (assoc (car equip-pub) equip-values)))
              (when equip-value
                (publish-msg (cdr equip-pub)
                             :data (min (cdr equip-value) max-value)))))
          equip-pubs))

(defun cleanup ()
  "Unsubscribes from all topics"
  (when *human-subscriber* (unsubscribe *human-subscriber*))
  (setf *human-subscriber* nil)
  (when *equipment-subscriber* (unsubscribe *equipment-subscriber*))
  (setf *equipment-subscriber* nil)
  (setf *visualization-publisher* nil))


  