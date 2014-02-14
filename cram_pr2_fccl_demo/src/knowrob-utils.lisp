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

(in-package :pr2-fccl-demo)

(defun query-feature (feature-id)
  "Queries knowrob for the geometric feature denoted by string `feature-id' and returns an
 instance of type 'cl-feature-constraints:geometric-feature' filled with the data read 
 from Knowrob."
  (declare (type string feature-id))
  (let* ((query (concatenate 'string
                             "feature_properties("
                             feature-id
                             ", Type, _, TfFrame, Position, Direction)"))
         (bindings (cut:lazy-car (json-prolog:prolog-simple query))))
    (cut:with-vars-bound (|?Type| |?Position| |?Direction| |?TfFrame|) bindings
      (cl-feature-constraints:make-geometric-feature
       :id feature-id
       :frame-id (knowrob-symbol->string |?TfFrame|)
       :feature-type (knowrob-feature-type->symbol |?Type|)
       :origin (knowrob-point->cl-transform |?Position|)
       :orientation (knowrob-point->cl-transform |?Direction|)))))

(defun knowrob-point->cl-transform (knowrob-point)
  "Translates the list of numbers `knowrob-point' into a 3d-vector. Prints a warning and 
 returns 'nil' in case of failure."
  (declare (type list knowrob-point))
  (if (not (= (length knowrob-point) 3))
      (warn "Given Knowrob 3d-point does not have length 3: ~a"
            knowrob-point)
      (cl-transforms:make-3d-vector
       (first knowrob-point)
       (second knowrob-point)
       (third knowrob-point))))

(defun knowrob-feature-type->symbol (feature-type)
  "Translates the symbol `feature-type' which is a feature type denotion returned from
 Knowrob into the corresponding symbol in package cl-feature-constraints. It prints a
 warning and returns 'nil' in case of failure."
  (declare (type symbol feature-type))
  (cond ((eql feature-type
              (intern "'http://ias.cs.tum.edu/kb/knowrob.owl#PointFeature'")) 
         'cl-feature-constraints:point)
        ((eql feature-type
              (intern "'http://ias.cs.tum.edu/kb/knowrob.owl#LineFeature'")) 
         'cl-feature-constraints:line)
        ((eql feature-type
              (intern "'http://ias.cs.tum.edu/kb/knowrob.owl#PlaneFeature'")) 
         'cl-feature-constraints:plane)
        (t (warn "Could not translate Knowrob symbol for geometric feature: ~a"
                 feature-type)
           nil)))

(defun knowrob-symbol->string (knowrob-symbol &optional (remove-quotes t))
  "Takes a 'knowrob-symbol' as typically returned when asking knowrob through
 json-prolog-client and returns the equivalent string. If remove-quotes is not NIL, the
 first and last character of the name of the symbol will be removed."
  (declare (type symbol knowrob-symbol))
  (let ((long-symbol-name (symbol-name knowrob-symbol)))
    (unless (> (length long-symbol-name) 1)
      (error
       'simple-error
       :format-control "Tried removing quotes from string with less than 2 symbols: ~a"
       :format-arguments '(long-symbol-name)))
    (if remove-quotes
        (subseq long-symbol-name 1 (- (length long-symbol-name) 1))
        long-symbol-name)))