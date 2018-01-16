#lang racket

(require "project.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

; Note we have provided [only] 3 tests, but you can't run them until do some of the assignment.
; You will want more tests.

(require rackunit)

(define tests
  (test-suite
   "Project Tests"

   (check-equal? (eval-exp (add (int 2) (int 2))) (int 4) "add simple test")

   (check-exn (lambda (x) (string=? (exn-message x) "NUMEX addition applied to non-number"))
              (lambda () (eval-exp (add (int 2) (aunit))))
              "add bad argument")

   (check-equal? (numexlist->racketlist
                  (eval-exp (call (call numex-mapAddN (int 9))
                                  (racketlist->numexlist 
                                   (list (int 10) (int 9) (int 15))))))
                 (list (int 19) (int 18) (int 24))
                 "provided combined test using problems 1, 2, and 4")
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
