#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))
      )
  )

(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs)
  )

(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (null? xs)
          (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs))))
          )
      )
  )

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([pair (s)])
        (cons (car pair) (stream-for-n-steps (cdr pair) (- n 1))) 
        )
      )
  )

(define (funny-number-stream)
  (letrec
      ([f (lambda (n)
            (cons
             ((if (= 0 (remainder n 5)) - +) 0 n)
             (lambda () (f (+ n 1))))
            )])
    (f 1)
    )
  )

(define (dan-then-dog)
  (define (f n)
    (cons
     (if (= n 1) "dan.jpg" "dog.jpg")
     (lambda () (f (- 1 n)))
     )
    )
  (f 1)
  )

(define (stream-add-zero s)
  (lambda ()
    (let ([pair (s)])
      (cons
       (cons 0 (car pair))
       (stream-add-zero (cdr pair)))
      )
    )
  )

(define (cycle-lists xs ys)
  (lambda ()
    (define (f n)
      (cons
       (cons (list-nth-mod xs n) (list-nth-mod ys n))
       (lambda () (f (+ 1 n)))
       )
      )
    (f 0)
    )
  )

(define (vector-assoc v vec)
  (define (find n)
    (if (>= n (vector-length vec))
        #f
        (let ([elm (vector-ref vec n)])
          (if (and (pair? elm) (equal? (car elm) v)) elm (find (+ n 1))))))
  (find 0)   
  )

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [next-slot 0]
           )
    (lambda (v)
      (let ([cached-res (vector-assoc v cache)])
      (if cached-res
          cached-res
          (let ([res (assoc v xs)])
            (if res
                (begin
                  (vector-set! cache next-slot res)
                  (set! next-slot (remainder (+ next-slot 1) n))
                  res
                 )
                res)
            )
          )
        )
      )
    )
  )
    
(define-syntax while-less
      (syntax-rules (do)
        [(while-less e1 do e2)
         (letrec
             ([res1 e1]
              [do_once (lambda () (if (>= e2 res1) #t (do_once)))])
           (do_once))]
        )
      )