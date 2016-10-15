#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;;Write a function palindromic that takes a list of numbers and evaluates to a list of numbers of the same length,
;;where each element is obtained as follows: the first element should be the sum of the first and the last elements of the original list,
;;the second one should be the sum of the second and second to last elements of the original list, etc.
;;Example: (palindromic (list 1 2 4 8)) evaluates to (list 9 6 6 9).
(define (palindromic xs)
  (letrec (
        [rxs (reverse xs)]
        [add-lists (lambda (xs ys)
                    (cond [(null? xs) null]
                          [#t (cons (+ (car xs) (car ys)) (add-lists (cdr xs) (cdr ys)) )]
                          )
                     )]
        )
    (add-lists xs rxs)
    )
  )

;;Define a stream fibonacci, the first element of which is 0, the second one is 1, and each successive element is the sum of two immediately preceding elements.
(define (fibonacci)
  (define (next a b)
    (cons  (+ a b) (lambda () (next b (+ a b))))
    )
  (next -1 1)
  )

;;Write a function stream-until that takes a function f and a stream s, and applies f to the values of s in succession until f evaluates to #f.

(define (stream-until f s)
  (let ([next (s)])
    (if (f (car next)) (stream-until f (cdr next)) #t))
  )

;;Write a function stream-map that takes a function f and a stream s, and returns a new stream whose values are the result of applying f to the values produced by s.

(define (stream-map f s)
  (lambda ()
   (let ([next (s)])
     (cons (f (car next)) (stream-map f (cdr next)))
     )
    )
  )

;;Write a function stream-zip that takes in two streams s1 and s2 and returns a stream that produces the pairs that result from the other two streams
;;(so the first value for the result stream will be the pair of the first value of s1 and the first value of s2).

(define (stream-zip s1 s2)
  (lambda ()
   (let ([next1 (s1)]
         [next2 (s2)])
     (cons (cons (car next1) (car next2)) (stream-zip (cdr next1) (cdr next2)))
     )
    )
  )

;;Write a function interleave that takes a list of streams and produces a new stream that takes one element from each stream in sequence.
;;So it will first produce the first value of the first stream, then the first value of the second stream and so on,
;;and it will go back to the first stream when it reaches the end of the list. Try to do this without ever adding an element to the end of a list.

(define (interleave s-list)
  (letrec (
           [s-vet (list->vector s-list)]
           [next-slot 0]
           [next (lambda ()
                 (let ([s (vector-ref s-vet next-slot)])
                   (begin
                     (vector-set! s-vet next-slot (cdr (s)))
                     (set! next-slot (remainder (add1 next-slot) (vector-length s-vet)))
                     (cons (car (s)) next)
                     )
                   )
                  )
                ])
    next
    )
  )
                  
;;Define a function pack that takes an integer n and a stream s, and returns a stream that produces the same values as s but packed in lists of n elements.
;;So the first value of the new stream will be the list consisting of the first n values of s,
;;the second value of the new stream will contain the next $\verb|n|$$ values, and so on.                 

(define (pack n s)
  (letrec
      ([real-pack
        (lambda (m s)
          (if (= 0 m)
              (cons null (lambda()(real-pack n s)))
              (let ([next (real-pack (sub1 m) (cdr (s)))])
                (cons
                 (cons (car (s)) (car next))
                 (cdr next))
                )
              )
          )
        ]
       )
    (lambda () (real-pack n s))
    )
  )

;;We'll use Newton's Method for approximating the square root of a number,
;;but by producing a stream of ever-better approximations so that clients can "decide later" how approximate a result they want:
;;Write a function sqrt-stream that takes a number n, starts with n as an initial guess in the stream,
;;and produces successive guesses applying fn(x)=12((x+nx) to the current guess.

(define (sqrt-stream n)
  (define (next x)
    (let ([next-x (/ (+ x (/ n x)) 2)])
      (cons x (lambda () (next next-x)))
    )
    )
  (lambda () (next n))
  )

;;Write a macro perform that has the following two forms: 
;; (perform e1 if e2)
;; (perform e1 unless e2)
;;e1 should be evaluated (once) depending on the result of evaluating e2 -- only if e2 evaluates to #f in the latter case,
;;and only if it doesn't in the former case. If e1 is never evaluated, the entire expression should evaluate to e2.
;;Neither e1 nor e2 should be evaluated more than once in any case

(define-syntax perform
  (syntax-rules (if unless)
    [(perform e1 if e2) (let ([res2 e2]) (if e2 e1 e2))]
    [(perform e1 unless e2) (let ([res2 e2]) (if e2 e2 e1))]
    )
  )