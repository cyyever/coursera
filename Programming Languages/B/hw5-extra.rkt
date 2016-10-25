;; Programming Languages, Homework 5 Extra Practice Problems

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file
(require "hw5.rkt")

(struct btree-leaf () #:transparent)
(struct btree-node (value left right) #:transparent)

;; A binary tree is either (btree-leaf) or or a Racket value built from btree-node where the left and right fields are both binary trees.

;; Write a function tree-height that accepts a binary tree and evaluates to a height of this tree.
;; The height of a tree is the length of the longest path to a leaf. Thus the height of a leaf is 0.

(define (tree-height t)
  (cond [(btree-leaf? t) 0]
        [(btree-node? t) (add1 (max (tree-height (btree-node-left t)) (tree-height (btree-node-right t))))]
        [#t (error "not a tree")]))

;; Write a function sum-tree that takes binary tree and sums all the values in all the nodes.
;; (Assume the value fields all hold numbers, i.e., values that you can pass to +.

(define (sum-tree t)
  (cond [(btree-leaf? t) 0]
        [(btree-node? t) (+ (btree-node-value t) (sum-tree (btree-node-left t)) (sum-tree (btree-node-right t)))]
        [#t (error "not a tree")]))

;; Write a function prune-at-v that takes a binary tree t and a value v and produces a new binary tree
;; with structure the same as t except any node with value equal to v (use Racket's equal?) is replaced (along with all its descendants) by a leaf.

(define (prune-at-v t v)
  (cond [(btree-leaf? t) t]
        [(btree-node? t) (if (equal? v (btree-node-value t))
                             (btree-leaf)
                             (btree-node (btree-node-value t) (prune-at-v (btree-node-left t) v) (prune-at-v (btree-node-right t) v)))] 
        [#t (error "not a tree")]))

;; Write a function well-formed-tree? that takes any value and returns #t if and only if the value is legal binary tree as defined above.

(define (well-formed-tree? t)
  (cond [(btree-leaf? t) #t]
        [(btree-node? t) (and (well-formed-tree? (btree-node-left t)) (well-formed-tree? (btree-node-right t)))]
        [#t #f]))

;; Write a function fold-tree that takes a two-argument function, an initial accumulator,
;; and a binary tree and implements a fold over the tree, applying the function to all the values.
;; For example, (fold-tree (lambda (x y) (+ x y 1)) 7 (btree-node 4 (btree-node 5 (btree-leaf) (btree-leaf)) (btree-leaf))) would evaluate to 18.
;; You can traverse the tree in any order you like (though it does affect the result of a call to fold-tree if the function passed isn't associative).

(define (fold-tree f acc t)
  (cond [(btree-leaf? t) acc]
        [(btree-node? t)
         (letrec ([applied-value (f acc (btree-node-value t))]
                  [applied-left (fold-tree f applied-value (btree-node-left t))])
           (fold-tree f applied-left (btree-node-right t)))]
        [#t (error "not a tree")]))

;; Reimplement fold-tree as a curried function.
(define (fold-tree-curried f)
  (lambda (acc)
    (lambda (t)
      (cond [(btree-leaf? t) acc]
            [(btree-node? t)
             (letrec ([applied-value (f acc (btree-node-value t))]
                      [applied-left (fold-tree f applied-value (btree-node-left t))])
               (fold-tree f applied-left (btree-node-right t)))]
            [#t (error "not a tree")]))
    )
  )

;; Write a function crazy-sum that takes a list of numbers and adds them all together, starting from the left.
;; There's a twist, however. The list is allowed to contain functions in addition to numbers. Whenever an element of a list is a function,
;; you should start using it to combine all the following numbers in a list instead of +.
;; You may assume that the list is non-empty and contains only numbers and binary functions suitable for operating on two numbers.
;; Further assume the first list element is a number. For example, (crazy-sum (list 10 * 6 / 5 - 3)) evaluates to 9.
;; Note: It may superficially look like the function implements infix syntax for arithmetic expressions, but that's not really the case.

(define (crazy-sum xs)
  (define (crazy-sum-f xs f acc)
   (cond [(null? xs) acc]
         [(number? (car xs)) (crazy-sum-f (cdr xs) f (f acc (car xs)))]
         [#t (crazy-sum-f (cdr xs) (car xs) acc)]))
  (crazy-sum-f xs + 0))

;; Write a function either-fold that is like fold for lists or binary trees as defined above except that it works for both of them.
;; Give an appropriate error message if the third argument to either-fold is neither a list nor a binary tree.

(define (either-fold f acc t)
  (cond [(list? t) (foldl f acc t)]
        [(or (btree-leaf? t) (btree-node? t)) (fold-tree f acc t)]
        [#t (error "not a tree or list")]))

;; Write a function flatten that takes a list and flattens its internal structure, merging all the lists inside into a single flat list.
;; This should work for lists nested to arbitrary depth. For example, (flatten (list 1 2 (list (list 3 4) 5 (list (list 6) 7 8)) 9 (list 10)))
;; should evaluate to (list 1 2 3 4 5 6 7 8 9 10).

(define (flatten xs)
  (define (flatten-helper xs)
    (cond [(null? xs) null]
          [#t (let ([first (car xs)]
                    [rest (cdr xs)])
                (cond [(null? first) (flatten-helper rest)]
                      [(list? first) (flatten-helper (cons (car first) (cons (cdr first) rest)))]
                      [#t (cons first (flatten-helper rest))]))]))
  (cond [(list? xs) (flatten-helper xs)]
        [#t (error "not a list")]))
        
;; Notice that MUPL doesn't need mlet: Anywhere we have (mlet name e body), we can use (call (fun #f name body) e) instead and get the same result.
;; Write a Racket function remove-lets that takes a MUPL program and produces an equivalent MUPL program that does not contain mlet.

(define (remove-lets e)
  (cond [(var? e) e]
        [(int? e) e]
        [(add? e) (add
                   (remove-lets (add-e1 e))
                   (remove-lets (add-e2 e)))]        
        [(ifgreater? e) (ifgreater
                         (remove-lets (ifgreater-e1 e))
                         (remove-lets (ifgreater-e2 e))
                         (remove-lets (ifgreater-e3 e))
                         (remove-lets (ifgreater-e4 e)))]
        [(fun? e) (fun
                   (fun-nameopt e)
                   (fun-formal e)
                   (remove-lets (fun-body e)))]
        [(closure? e) (closure
                       (closure-env e)
                       (remove-lets (closure-fun e)))]
        [(call? e) (call
                    (remove-lets (call-funexp e))
                    (remove-lets (call-actual e)))]
        [(mlet? e) (call
                    (fun #f (mlet-var e) (remove-lets (mlet-body e)))
                    (remove-lets (mlet-e e)))]
        [(apair? e) (apair
                     (remove-lets (apair-e1 e))
                     (remove-lets (apair-e2 e)))]
        [(fst? e) (fst (remove-lets (fst-e e)))]
        [(snd? e) (snd (remove-lets (snd-e e)))]
        [(isaunit? e) (isaunit (remove-lets (isaunit-e e)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; [more challenging] Now we will do something even more clever: remove pairs by using closure environments as another way to "hold" two pieces of data.
;; Instead of using, (apair e1 e2), we can use (mlet "_x" e1 (mlet "_y" e2 (fun #f "_f" (call (call (var "_f") (var "_x")) (var "_y"))))))
;; (assuming "_x" isn't already used in e2 -- we will assume that). This will evaluate to a closure that has the result of evaluating e1 and e2 in its environment.
;; When the closure is called with a function, that function will be called with the result of evaluating e1 and e2 (in curried form).
;; So if we replace every apair expression as described above, then we can, rather cleverly, replace (fst e) with
;; (call e (fun #f "x" (fun #f "y" (var "x"))). Extend your remove-lets, renaming it remove-lets-and-pairs so that it removes all uses of apair, fst, and snd.
;; (We are leaving it to you to figure out how to replace (snd e).
;; Note 1: Remember you need to remove things recursively inside of apair, fst, etc., else an expression like (fst (snd (var "x"))) won't have the snd removed.
;; Note 2: The resulting program should produce the same result when evaluated if the (original) result doesn't contain any pair values.
;; If the original result does contain pair values, the result after removal will contain corresponding closures.
;; Note 3: A slightly more challenging approach is to change how apair is removed so that we do not need to assume "_y" is not used in e2.

(define (remove-lets-and-pairs e)
  (define (remove-pairs e)
    (cond [(var? e) e]
          [(int? e) e]
          [(add? e) (add
                     (remove-pairs (add-e1 e))
                     (remove-pairs (add-e2 e)))]        
          [(ifgreater? e) (ifgreater
                           (remove-pairs (ifgreater-e1 e))
                           (remove-pairs (ifgreater-e2 e))
                           (remove-pairs (ifgreater-e3 e))
                           (remove-pairs (ifgreater-e4 e)))]
          [(fun? e) (fun
                     (fun-nameopt e)
                     (fun-formal e)
                     (remove-pairs (fun-body e)))]
          [(closure? e) (closure
                         (closure-env e)
                         (remove-pairs (closure-fun e)))]
          [(call? e) (call
                      (remove-pairs (call-funexp e))
                      (remove-pairs (call-actual e)))]
          [(mlet? e) (mlet 
                      (mlet-var e)
                      (remove-pairs (mlet-e e))
                      (remove-pairs (mlet-body e)))]
          [(apair? e) (call 
                       (call
                        (fun #t "x"
                             (fun #t "y"
                                  (fun #f "f"
                                       (call (call (var "f") (var "x")) (var "y"))))) (apair-e1 e)) (apair-e2 e))]           
          [(fst? e) (call
                     (remove-pairs (fst-e e))
                     (fun #f "x" (fun #f "y" (var "x"))))]
          [(snd? e) (call
                     (remove-pairs (snd-e e))
                     (fun #f "x" (fun #f "y" (var "y"))))]
          [(isaunit? e) (isaunit (remove-pairs (isaunit-e e)))]
          [#t (error (format "bad MUPL expression: ~v" e))]))
  (remove-lets (remove-pairs e)))

;; In the first problem, we treat (int 1) as true in MUPL and (int 0) as false in MUPL.

;; Define a Racket binding mupl-all that holds a MUPL function that takes a MUPL list and evaluates to (MUPL) true
;; if all the list elements are (MUPL) true, else it evaluates to (MUPL) false.

(define mupl-all
  (fun "f" "xs" (ifaunit (var "xs")
                         (int 1)
                         (ifaunit (fst (var "xs"))
                                  (call (var "f") (snd (var "xs")))
                                  (int 0)))))

;; Define a Racket binding mupl-append that holds a MUPL function that takes two MUPL lists (in curried form) and appends them.

(define mupl-append
  (fun #t "xs"
       (mlet "append-single"
             (fun "append-single" "pair"
                  (ifaunit (fst (var "pair"))
                           (apair (snd (var "pair")) (aunit))
                           (apair (fst (fst (var "pair"))) (call (var "append-single") (apair (snd (fst (var "pair"))) (snd (var "pair")))))))
             (fun #t "ys"
                  (call (fun "append" "pair"
                             (ifaunit (snd (var "pair"))
                                      (fst (var "pair"))
                                      (call (var "append") (apair
                                                            (call (var "append-single") (apair
                                                                                         (fst (var "pair"))
                                                                                         (fst (snd (var "pair")))))
                                                            (snd (snd (var "pair")))))))
                        (apair (var "xs") (var "ys")))))))
                        

;; Define a Racket binding mupl-zip that holds a MUPL function that takes two MUPL lists (in curried form) and returns a list of pairs (much like ML's zip).
;; If the MUPL lists are different lengths, ignore a suffix of the longer list
;; (so the returned list of pairs has a length equal to the shorter of the argument lengths).

(define mupl-zip
    (fun #t "xs"
             (fun #t "ys"
                  (call (fun "zip" "pair"
                             (ifgreater (add (isaunit (fst (var "pair"))) (isaunit (snd (var "pair")))) (int 0)
                                        (aunit)
                                        (apair
                                         (apair (fst (fst (var "pair"))) (fst (snd (var "pair"))))
                                         (call (var "zip") (apair (snd (fst (var "pair"))) (snd (snd (var "pair"))))))))
                        (apair (var "xs") (var "ys"))))))

;; Define a Racket binding mupl-curry that holds a MUPL function that is like ML's fun curry f = (fn x y => f (x,y)).

(define mupl-curry
  (fun #t "f"
       (fun #t "x"
            (fun #t "y"
                 (call (var "f") (apair (var "x") (var "y")))))))

;; Define a Racket binding mupl-uncurry that holds a MUPL function that is like ML's fun uncurry f = (fn (x,y) => f x y).

(define mupl-uncurry
  (fun #t "f"
       (fun #t "pair"
            (call (call (var "f") (fst (var "pair"))) (snd (var "pair"))))))

;; Define a Racket binding if-greater3 that is a MUPL macro (a Racket function) that takes 5 MUPL expressions and produces a MUPL program that,
;; when evaluated, evaluates the 4th subpexression as the result if the 1st subexpression is greater than the 2nd and the 2nd is greater than the 3rd,
;; else it evaluates the 5th subexpression as the result. When the MUPL program is evaluated, it should always evaluate the 1st, 2nd, and 3rd subexpressions
;; exactly once each and then the 4th subexpression or 5th subexpression but not both.

(define (if-greater3 e1 e2 e3 e4 e5)
  (ifgreater
   (call (call (call
               (fun #t "e1"
                    (fun #t "e2"
                         (fun #t "e3"
                              (ifgreater (var "e1") (var "e2")
                                         (ifgreater (var "e2") (var "e3")
                                                    (int 1)
                                                    (int 0))
                                         (int 0)))))
               e1) e2) e3) (int 0)
                           e4 e5))

;; Define a Racket binding call-curried that is a MUPL macro (a Racket function) that takes a MUPL expression e1 and a Racket list of MUPL expressions e2
;; and produces a MUPL program that, when evaluated, calls the result of evaluating e1 as a curried function with all of the results of evaluating
;; the expressions in e2. For example, instead of writing (call (call e1 ea) eb), you can write (call-curried e1 (list ea eb)).

(define (call-curried e1 e2)
  (define (remain-call remain-e acc)
    (cond [(null? remain-e) acc]
          [#t (remain-call (cdr remain-e) (call acc (car remain-e)))]))
  (remain-call (cdr e2) (call e1 (car e2))))



    
      