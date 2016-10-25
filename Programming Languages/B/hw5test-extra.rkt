#lang racket
;; Programming Languages Homework 5 Extra Practice Problems Simple Test

(require "hw5-extra.rkt")
(require "hw5.rkt")

(require rackunit)

(define tests
  (test-suite
   "Sample tests for Assignment 5 Extra Practice Problems"

   (check-equal? (tree-height (btree-node 4 (btree-node 5 (btree-leaf) (btree-leaf)) (btree-leaf))) 2 "tree-height test")
   
   (check-equal? (sum-tree (btree-node 4 (btree-node 5 (btree-leaf) (btree-leaf)) (btree-leaf))) 9 "sum-tree test")
   
   (check-equal? (prune-at-v (btree-node 4 (btree-node 5 (btree-leaf) (btree-leaf)) (btree-leaf)) 5) (btree-node 4 (btree-leaf) (btree-leaf)) "prune-at-v test")

   (check-equal? (well-formed-tree? (btree-node 4 (btree-node 5 (btree-leaf) (btree-leaf)) (btree-leaf))) #t "well-formed-tree? test")

   (check-equal? (fold-tree (lambda (x y) (+ x y 1)) 7 (btree-node 4 (btree-node 5 (btree-leaf) (btree-leaf)) (btree-leaf))) 18 "fold-tree test")

   (check-equal? (((fold-tree-curried (lambda (x y) (+ x y 1))) 7) (btree-node 4 (btree-node 5 (btree-leaf) (btree-leaf)) (btree-leaf))) 18 "fold-tree-curried test")

   (check-equal? (crazy-sum (list 10 * 6 / 5 - 3)) 9 "crazy-sum test")

   (check-equal? (either-fold (lambda (x y) (+ x y 1)) 7 (btree-node 4 (btree-node 5 (btree-leaf) (btree-leaf)) (btree-leaf))) 18 "either-fold test")

   (check-equal? (either-fold + 0 (list 1 2 3 4)) 10 "either-fold test")

   (check-equal? (flatten (list 1 2 (list (list 3 4) 5 (list (list 6) 7 8)) 9 (list 10))) (list 1 2 3 4 5 6 7 8 9 10) "flatten test")

   (check-equal? (remove-lets (mlet "x" (int 5) (var "x"))) (call (fun #f "x" (var "x")) (int 5)) "remove-lets test")

   (check-equal? (eval-exp-c (remove-lets-and-pairs (fst (mlet "x" (int 1) (mlet "y" (int 2) (apair (add (var "x") (var "y")) (var "y")))))))
                 (int 3) "remove-lets-and-pairs testA")

   (check-equal? (eval-exp-c (remove-lets-and-pairs (snd (mlet "x" (int 1) (mlet "y" (int 2) (apair (add (var "x") (var "y")) (var "y")))))))
                 (int 2) "remove-lets-and-pairs testB")

   (check-equal? (eval-exp-c (call mupl-all (apair (int 1) (apair (add (int -1) (int 1)) (aunit))))) (int 0) "mupl-all test")

   (check-equal? (eval-exp-c (call (call mupl-append (apair (int 1) (apair (int 2) (aunit)))) (apair (int 3 ) (apair (int 4) (aunit)))))
                 (apair (int 1) (apair (int 2) (apair (int 3) (apair (int 4) (aunit))))) "mupl-append test")

   (check-equal? (eval-exp-c (call (call mupl-zip (apair (int 1) (apair (int 2) (aunit)))) (apair (int 3) (aunit))))
                 (apair (apair (int 1) (int 3)) (aunit)) "mupl-zip test")

   (check-equal? (eval-exp-c (call (call (call mupl-curry (fun #t "pair" (add (fst (var "pair")) (snd (var "pair"))))) (int 1)) (int 2)))
                 (int 3) "mupl-curry test")

   (check-equal? (eval-exp-c (call (call mupl-uncurry (fun #t "x" (fun #t "y" (add (var "x") (var "y"))))) (apair (int 1) (int 2))))
                 (int 3) "mupl-uncurry test")

   (check-equal? (eval-exp-c (if-greater3 (int 3) (int 2) (int 1) (int 5) (int 4))) (int 5) "if-greater3 test")

   (check-equal? (eval-exp-c (call-curried (fun #f "x" (fun #f "y" (add (var "x") (var "y")))) (list (int 1) (int 2)))) (int 3) "call-curried test")
   ))


(require rackunit/text-ui)
;; runs the test
(run-tests tests)
