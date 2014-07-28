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

(defun capped (value min max)
  (if (and min (< value min))
      min
      (if (and max (> value max))
          max
          value)))

(defun publish-visualization-marker (pose-stamped &key 
                                                    (id 0) 
                                                    (type :arrow) 
                                                    min-x max-x
                                                    min-y max-y
                                                    min-z max-z
                                                    (scale-x 0.3)
                                                    (scale-y 0.15)
                                                    (scale-z 0.15)
                                                    (color-a 1)
                                                    (color-r 1)
                                                    (color-g 1)
                                                    (color-b 1)
                                                    (lifetime 50)
                                                    points)
  (let ((origin (cl-tf:origin pose-stamped))
        (orientation (cl-tf:orientation pose-stamped)))
    (publish *visualization-pub*
             (make-message "visualization_msgs/Marker"
                           (frame_id header) (cl-tf:frame-id pose-stamped)
                           (stamp header)  (ros-time)
                           ns "cram_saphari"
                           id id
                           type (symbol-code 'visualization_msgs-msg:marker 
                                             type)
                           action 0
                           (x position pose) (capped (cl-tf:x origin)
                                                     min-x max-x)
                           (y position pose) (capped (cl-tf:y origin)
                                                     min-y max-y)
                           (z position pose) (capped (cl-tf:z origin)
                                                     min-z max-z)
                           (x orientation pose) (cl-tf:x orientation)
                           (y orientation pose) (cl-tf:y orientation)
                           (z orientation pose) (cl-tf:z orientation)
                           (w orientation pose) (cl-tf:w orientation)
                           (x scale) scale-x
                           (y scale) scale-y
                           (z scale) scale-z
                           (a color) color-a
                           (r color) color-r
                           (g color) color-g
                           (b color) color-b
                           lifetime lifetime
                           points (if points
                                      points
                                      (vector))))))
