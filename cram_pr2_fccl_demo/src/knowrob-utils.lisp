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

(define-condition knowrob-query-error (error) 
  ((text :initarg :text :reader text)
   (args :initarg :args :initform nil :reader args)))
(define-condition knowrob-motion-query-error (knowrob-query-error) ())
(define-condition knowrob-phase-query-error (knowrob-query-error) ())
(define-condition knowrob-feature-query-error (knowrob-query-error) ())
(define-condition knowrob-constraint-query-error (knowrob-query-error) ())
(define-condition knowrob-translation-error (knowrob-query-error) ())

(defun query-motion-description (motion-type-id tool-type-id object-type-id)
  "Queries knowrob for the motion description with ID `motion-type-id', instantiated with
 tool-type `tool-type-id' and world-object-type `object-type-id', and returns the result as
 a list of instances of type 'motion-phases'. May signal an errror of any of the sub-types
 of 'knowrob-query-error' in case of failure."
  (declare (type string motion-type-id tool-type-id object-type-id))
  (let* ((query (concatenate 
                 'string 
                 "plan_subevents(" motion-type-id ", _Subs),"
                 "member(Sub, _Subs),"
                 "class_properties(Sub, knowrob:deviceUsed, Tool),"
                 "owl_subclass_of(Tool, " tool-type-id "),"
                 "class_properties(Sub, knowrob:objectActedOn, World),"
                 "owl_subclass_of(World, " object-type-id ")"))
         (bindings (cut:force-ll (json-prolog:prolog-simple query))))
    (if bindings
        (mapcar (lambda (binding)
                  (cut:with-vars-bound (|?Sub| |?Tool| |?World|) binding
                    (query-motion-phase
                     (knowrob-symbol->string |?Sub| nil)
                     (knowrob-symbol->string |?Tool| nil)
                     (knowrob-symbol->string |?World| nil))))
                bindings)
        (error 'knowrob-motion-query-error
                  :text "Knowrob query for constraint-motion came up empty."
                  :args (list motion-type-id tool-type-id object-type-id)))))

(defun query-motion-phase (phase-id tool-type-id object-type-id)
  "Queries Knowrob for the content of the motion-phase with `phase-id', instantiated with
 tool-class `tool-type-id', and world-object-class `object-type-id', and returns an instance
 of type 'motion-phase' as a result. Signals an error of any of the sub-types of
 'knowrob-query-error' in case of failure."
  (declare (type string phase-id tool-type-id object-type-id))
  (let* ((query (concatenate 'string "motion_constraint(" phase-id ", Constraint)"))
         (bindings (cut:force-ll (json-prolog:prolog-simple query))))
    (if bindings
        (cl-feature-constraints:make-motion-phase
         :id phase-id
         :constraints 
         (mapcar (lambda (binding)
                   (cut:with-vars-bound (|?Constraint|) binding
                     (query-constraint (knowrob-symbol->string |?Constraint| nil)
                                       tool-type-id object-type-id)))
                 bindings))
        (error 'knowrob-phase-query-error
                  :text "Knowrob query for motion-phase came up empty."
                  :args (list phase-id tool-type-id object-type-id)))))
  
(defun query-constraint (constraint-id tool-type-id object-type-id)
  "Queries Knowrob for the content of generic feature constraint with ID `constraint-id',
 instantiated with tool-class `tool-type-id' and object-class `object-type-id', and returns
 an instance of type 'feature-constraint'. Signals an error of any of the subtypes of
 'knowrob-query-error' in case of failure."
  (declare (type string constraint-id tool-type-id object-type-id))
  (let* ((query (concatenate 'string
                             "constraint_properties("
                             tool-type-id ", " object-type-id ", " constraint-id
                             ", Type, ToolFeature, WorldFeature, Reference, Lower, Upper)"))
         (bindings (cut:lazy-car (json-prolog:prolog-simple query))))
    (cut:with-vars-bound 
        (|?Type| |?ToolFeature| |?WorldFeature| |?Reference| |?Lower| |?Upper|) bindings
      (if bindings
          (let ((feature-relation
                  (cl-feature-constraints:make-feature-relation
                   :id (concatenate 'string "relation_" constraint-id)
                   :function-type (knowrob-constraint-type->symbol |?Type|)
                   :frame-id (knowrob-symbol->string |?Reference|)
                   :tool-feature (query-feature (knowrob-symbol->string |?ToolFeature| nil))
                   :object-feature (query-feature (knowrob-symbol->string |?WorldFeature| nil)))))
            (cl-feature-constraints:make-feature-constraint
             :id constraint-id :relation feature-relation
             :lower-boundary |?Lower| :upper-boundary |?Upper|))
          (error 'knowrob-constraint-query-error 
                  :text "Knowrob query for feature constraint came up empty."
                  :args (list constraint-id tool-type-id object-type-id))))))
      
(defun knowrob-constraint-type->symbol (constraint-type)
  "Translates the symbol `constraint-type' which is a constraint type denotion returned from
 Knowrob into the corresponding symbol in package cl-feature-constraints. Signals an error
 of type 'knowrob-translation-error in case of failure."
  (declare (type symbol constraint-type))
  (cond ((eql constraint-type
              (intern "'http://ias.cs.tum.edu/kb/motion-constraints.owl#PerpendicularityConstraint'")) 
         'cl-feature-constraints:perpendicular)
        ((eql constraint-type
              (intern "'http://ias.cs.tum.edu/kb/motion-constraints.owl#DistanceConstraint'"))
         'cl-feature-constraints:distance)
        ((eql constraint-type
              (intern "'http://ias.cs.tum.edu/kb/motion-constraints.owl#HeightConstraint'"))
         'cl-feature-constraints:above)
        ((eql constraint-type
              (intern "'http://ias.cs.tum.edu/kb/motion-constraints.owl#PointingAtConstraint'"))
         'cl-feature-constraints:pointing)
        ((eql constraint-type
              (intern "'http://ias.cs.tum.edu/kb/motion-constraints.owl#RightOfConstraint'"))
         'cl-feature-constraints:right)
        ((eql constraint-type
              (intern "'http://ias.cs.tum.edu/kb/motion-constraints.owl#InFrontOfConstraint'"))
         'cl-feature-constraints:infront)
        (t (error 'knowrob-translation-error
                   :text "Could not translate Knowrob symbol for feature constraint."
                   :args (list constraint-type)))))

(defun query-feature (feature-id)
  "Queries knowrob for the geometric feature denoted by string `feature-id' and returns an
 instance of type 'cl-feature-constraints:geometric-feature' filled with the data read 
 from Knowrob. Signals an error of type 'knowrob-feature-query-error' or of type
 'knowrob-translation-error' in case of failure."
  (declare (type string feature-id))
  (let* ((query (concatenate 'string
                             "feature_properties("
                             feature-id
                             ", Type, _, TfFrame, Position, Direction)"))
         (bindings (cut:lazy-car (json-prolog:prolog-simple query))))
    (if bindings 
        (cut:with-vars-bound (|?Type| |?Position| |?Direction| |?TfFrame|) bindings
          (cl-feature-constraints:make-geometric-feature
           :id feature-id
           :frame-id (knowrob-symbol->string |?TfFrame|)
           :feature-type (knowrob-feature-type->symbol |?Type|)
           :origin (knowrob-point->cl-transform |?Position|)
           :orientation (knowrob-point->cl-transform |?Direction|)))
        (error 'knowrob-feature-query-error
                :text "Knowrob query for geometric feature came up empty."
                :args (list feature-id)))))

(defun knowrob-point->cl-transform (knowrob-point)
  "Translates the list of numbers `knowrob-point' into a 3d-vector. Signals an error of
 type 'knowrob-translation-error' in case of failure."
  (declare (type list knowrob-point))
  (if (not (= (length knowrob-point) 3))
      (error 'knowrob-translation-error
              :text "Given Knowrob 3d-point does not have length 3."
              :args (list knowrob-point))
      (cl-transforms:make-3d-vector
       (first knowrob-point)
       (second knowrob-point)
       (third knowrob-point))))

(defun knowrob-feature-type->symbol (feature-type)
  "Translates the symbol `feature-type' which is a feature type denotion returned from
 Knowrob into the corresponding symbol in package cl-feature-constraints. Signals an error
 of type 'knowrob-translation-error' in case of failure."
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
        (t (error
            'knowrob-translation-error
            :text "Knowrob symbol for geometric feature malformed."
            :args (list feature-type)))))

(defun knowrob-symbol->string (knowrob-symbol &optional (remove-quotes t))
  "Takes a 'knowrob-symbol' as typically returned when asking knowrob through
 json-prolog-client and returns the equivalent string. If remove-quotes is not NIL, the
 first and last character of the name of the symbol will be removed. Signals an error of
 type 'knowrob-translation-error' in case of failure."
  (declare (type symbol knowrob-symbol))
  (let ((long-symbol-name (symbol-name knowrob-symbol)))
    (unless (> (length long-symbol-name) 1)
      (error 'knowrob-translation-error
              :text "Tried removing quotes from string with less than 2 symbols."
              :args (list knowrob-symbol remove-quotes)))
    (if remove-quotes
        (subseq long-symbol-name 1 (- (length long-symbol-name) 1))
        long-symbol-name)))