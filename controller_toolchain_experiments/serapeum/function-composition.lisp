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

(in-package :serapeum)

(defmacro compose (&rest functions) 
  "Takes a sequence of symbols denoting functions in `functions' and
 assembles a lambda-function which corresponds to the composition of those
 functions. Two assumptions have been made:
   (1) the last function in `functions' has arity 1
   (2) all functions next to each other in the list have matching arity

 Example usage:
   CL_USER> (funcall (compose 1+ (lambda (x) (* 3 x)) 1+) 1)
   => 7

 Note: I've taken quite some inspiration from Paul Graham's 'On LISP'.
   See: http://dunsmor.com/lisp/onlisp/onlisp_19.html"
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 1)))
  `#',(recursively-build-composition functions))

(defun recursively-build-composition (functions)
  "Actual implementation of macro 'compose' in a function."
  (case (length functions)
    (0 'identity)
    (1 (car functions))
    (t (let ((g (gensym)))
         `(lambda (,g)
            ,(labels ((recursion (fns)
                        (if fns
                            `(,(car fns) ,(recursion (cdr fns)))
                            g)))
               (recursion functions)))))))