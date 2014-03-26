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

(cpl-impl:def-cram-function pouring ()
  (with-designators ((desig (action `((type constraints) (to pour)))))
    (destructuring-bind (motions start-controller stop-controller finished-fluent)
        (reference desig)
    (ensure-vel-controllers)
      (loop for motion in motions do
        (cram-language:pursue
          (funcall start-controller motion)
          (cram-language:whenever ((cram-language:pulsed finished-fluent))
            (when (cram-language-implementation:value finished-fluent)
              (funcall stop-controller))))))))

(cpl-impl:def-cram-function flipping ()
  (with-designators ((desig (action `((type constraints) (to flip)))))
  (destructuring-bind (motions l-start-controller l-stop-controller l-finished-fluent
                       r-start-controller r-stop-controller r-finished-fluent)
      (reference desig)
    (ensure-vel-controllers)
    (loop for motion in motions do
      (cram-language:pursue
        (funcall l-start-controller (first motion))
        (funcall r-start-controller (rest motion))
        (cram-language:whenever ((cpl-impl:fl-or 
                                  (cram-language:pulsed l-finished-fluent)
                                  (cram-language:pulsed r-finished-fluent)))
          (when (cpl-impl:fl-and (cram-language-implementation:value l-finished-fluent)
                                 (cram-language-implementation:value r-finished-fluent))
            (cram-language:par
              (funcall l-stop-controller)
              (funcall r-stop-controller)))))))))