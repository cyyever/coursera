#lang racket

(require "hw4_extra.rkt")
(require "hw4.rkt")

(require rackunit)

;; Helper functions
(define ones (lambda () (cons 1 ones)))

(define tests
  (test-suite
   "Sample tests for Assignment 4 extra practice problems"
   
   ; problem 1
   (check-equal? (palindromic (list 1 2 4 8)) (list 9 6 6 9) "problem 1 test")

   ; problem 2
   (check-equal? (stream-for-n-steps fibonacci 7) (list 0 1 1 2 3 5 8) "problem 2 test")

   ; problem 3
   (check-equal? (stream-until (lambda (n) (if (< 10 n) #f (print n))) fibonacci) #t "problem 3 test")
   
   ; problem 4
   (check-equal? (stream-for-n-steps (stream-map add1 fibonacci) 7) (list 1 2 2 3 4 6 9) "problem 4 test")

   ; problem 5
   (check-equal? (stream-for-n-steps (stream-zip fibonacci fibonacci) 4) (list (cons 0 0) (cons 1 1) (cons 1 1) (cons 2 2)) "problem 5 test")

   ; problem 7
   (check-equal? (stream-for-n-steps (interleave (list ones fibonacci)) 10) (list 1 0 1 1 1 1 1 2 1 3) "problem 7 test")

   ; problem 8
   (check-equal? (stream-for-n-steps (pack 3 fibonacci) 3) (list (list 0 1 1) (list 2 3 5) (list 8 13 21)) "problem 8 test")

   ; problem 9
   (check-equal? (stream-for-n-steps (sqrt-stream 2) 3) (list 2 (/ 3 2) (/ 17 12)) "problem 9 test")

   ; problem 10
   (check-equal?  (perform (+ 2 2) if #t) 4 "problem 10 test")
   (check-equal?  (perform (+ 2 2) unless #f) 4 "problem 10 test") 
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
