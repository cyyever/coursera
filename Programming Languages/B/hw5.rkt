;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)

(define (racketlist->mupllist xs)
  (cond [(null? xs) (aunit)]
        [#t (apair (car xs) (racketlist->mupllist (cdr xs)))]
        ))

(define (mupllist->racketlist xs)
  (cond [(aunit? xs) null]
        [#t (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))]
        ))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(int? e) e]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(ifgreater? e) 
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(fun? e)
         (let ([fun-name (fun-nameopt e)]
               [arg-name (fun-formal e)])
           (cond [(string=? arg-name "") (error "MUPL function argument name is empty string")]
                 [(and (string? fun-name) (string=? fun-name "")) (error "MUPL function name is empty string")]
                 [(and (string? fun-name) (string=? fun-name arg-name)) (error "MUPL function name is same with argument name")]
                 [#t (closure env e)]))]
        [(closure? e) e]
        [(call? e) 
         (let ([c (eval-under-env (call-funexp e) env)]
               [v (eval-under-env (call-actual e) env)])
           (if (closure? c)
               (letrec ([f (closure-fun c)]
                        [fun-name (fun-nameopt f)]
                        [arg-name (fun-formal f)]
                        [extended-env (cons (cons arg-name v) (closure-env c))])
                 (eval-under-env (fun-body f) (if fun-name (cons (cons fun-name c) extended-env) extended-env)))
               (error "MUPL calling an non-closure")))]
        [(mlet? e)
         (let ([var-name (mlet-var e)]
               [v (eval-under-env (mlet-e e) env)])
           (if (string=? var-name "")
               (error "MUPL variable name is empty string")
               (eval-under-env (mlet-body e) (cons (cons var-name v) env))))]
        [(apair? e) 
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e) 
         (let ([v (eval-under-env (fst-e e) env)])
           (cond [(apair? v) (apair-e1 v)]
                 [#t (error "MUPL fst applied to non-apair")]))]
        [(snd? e) 
         (let ([v (eval-under-env (snd-e e) env)])
           (cond [(apair? v) (apair-e2 v)]
                 [#t (error "MUPL snd applied to non-apair")]))]
        [(aunit? e) e]
        [(isaunit? e) 
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))
 
(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y") e4
                         (ifgreater (var "_y") (var "_x") e4 e3)
               ))))

;; Problem 4

(define mupl-map (fun #f "f"
                      (fun "map-remain" "lst"
                           (ifaunit (var "lst")
                                    (aunit)
                                    (apair
                                     (call (var "f") (fst (var "lst")))
                                     (call (var "map-remain") (snd (var "lst")))))))) 




(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map") (fun #f "elm" (add (var "elm") (var "i")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (define (compute-fun-body-free-vars e)
    (cond [(var? e)
           (let ([var-name (var-string e)])
             (set var-name))]
          [(int? e) (set)]
          [(add? e) (set-union
                     (compute-fun-body-free-vars (add-e1 e))
                     (compute-fun-body-free-vars (add-e2 e)))]
          [(ifgreater? e) (set-union
                           (compute-fun-body-free-vars (ifgreater-e1 e))
                           (compute-fun-body-free-vars (ifgreater-e2 e))
                           (compute-fun-body-free-vars (ifgreater-e3 e))
                           (compute-fun-body-free-vars (ifgreater-e4 e)))]
          [(fun? e)
           (letrec ([fun-name (fun-nameopt e)]
                    [arg-name (fun-formal e)]
                    [no-free-vars (set arg-name)])
             (cond [(string=? arg-name "") (error "MUPL function argument name is empty string")]
                   [(and (string? fun-name) (string=? fun-name "")) (error "MUPL function name is empty string")]
                   [(and (string? fun-name) (string=? fun-name arg-name)) (error "MUPL function name is same with argument name")]
                   [#t (let ([extended-no-free-vars (if fun-name (set-add no-free-vars fun-name) no-free-vars)])
                         (set-subtract
                          (compute-fun-body-free-vars (fun-body e))
                          extended-no-free-vars))]))]
          [(closure? e) (compute-fun-body-free-vars (closure-fun e))]
          [(call? e) (set-union
                      (compute-fun-body-free-vars (call-funexp e))
                      (compute-fun-body-free-vars (call-actual e)))] 
          [(mlet? e) (set-union
                      (compute-fun-body-free-vars (mlet-e e))
                      (set-subtract
                       (compute-fun-body-free-vars (mlet-body e))
                       (set (mlet-var e))))]
          [(apair? e) (set-union
                       (compute-fun-body-free-vars (apair-e1 e))
                       (compute-fun-body-free-vars (apair-e2 e)))]
          [(fst? e) (compute-fun-body-free-vars (fst-e e))]
          [(snd? e) (compute-fun-body-free-vars (snd-e e))]
          [(aunit? e) (set)]
          [(isaunit? e) (compute-fun-body-free-vars (isaunit-e e))]
          [#t (error (format "bad MUPL expression: ~v" e))])
    )
  (cond [(add? e) (add
                   (compute-free-vars (add-e1 e))
                   (compute-free-vars (add-e2 e)))]
        [(ifgreater? e) (ifgreater
                         (compute-free-vars (ifgreater-e1 e))
                         (compute-free-vars (ifgreater-e2 e))
                         (compute-free-vars (ifgreater-e3 e))
                         (compute-free-vars (ifgreater-e4 e)))]
        [(fun? e) (fun-challenge
                   (fun-nameopt e)
                   (fun-formal e)
                   (compute-free-vars (fun-body e))
                   (compute-fun-body-free-vars e))]
        [(closure? e) (closure
                       (closure-env e)
                       (compute-free-vars (closure-fun e)))]
        [(call? e) (call
                    (compute-free-vars (call-funexp e))
                    (compute-free-vars (call-actual e)))]
        [(mlet? e) (mlet
                    (mlet-var e)
                    (compute-free-vars (mlet-e e))
                    (compute-free-vars (mlet-body e)))]
        [(apair? e) (apair
                     (compute-free-vars (apair-e1 e))
                     (compute-free-vars (apair-e2 e)))]
        [(fst? e) (fst (compute-free-vars (fst-e e)))]
        [(snd? e) (snd (compute-free-vars (snd-e e)))]
        [(isaunit? e) (isaunit (compute-free-vars (isaunit-e e)))]
        [#t e]))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
    (cond [(var? e)
           (envlookup env (var-string e))]
        [(int? e) e]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(ifgreater? e) 
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(fun-challenge? e)
         (letrec ([fun-name (fun-challenge-nameopt e)]
                  [arg-name (fun-challenge-formal e)]
                  [free-var-env (lambda (free-vars)
                                  (cond [(set-empty? free-vars) null]
                                        [#t (let ([first-free-var (set-first free-vars)]
                                                  [remain-free-vars (set-rest free-vars)])
                                              (cond [(or (string=? arg-name first-free-var)
                                                         (and (string? fun-name) (string=? fun-name first-free-var))) (free-var-env remain-free-vars)]
                                                    [#t (cons (cons first-free-var (envlookup env first-free-var)) (free-var-env remain-free-vars))]))]))])
           (closure (free-var-env (fun-challenge-freevars e)) e))]
        [(closure? e) e]
        [(call? e)         
         (let ([c (eval-under-env-c (call-funexp e) env)]
               [v (eval-under-env-c (call-actual e) env)])
           (if (closure? c)
               (letrec ([f (closure-fun c)]
                        [fun-name (fun-challenge-nameopt f)]
                        [arg-name (fun-challenge-formal f)]
                        [extended-env (cons (cons arg-name v) (closure-env c))])
                 (eval-under-env-c (fun-challenge-body f) (if fun-name (cons (cons fun-name c) extended-env) extended-env)))
               (error "MUPL calling an non-closure")))]
        [(mlet? e)
         (let ([var-name (mlet-var e)]
               [v (eval-under-env-c (mlet-e e) env)])
           (if (string=? var-name "")
               (error "MUPL variable name is empty string")
               (eval-under-env-c (mlet-body e) (cons (cons var-name v) env))))]
        [(apair? e) 
         (let ([v1 (eval-under-env-c (apair-e1 e) env)]
               [v2 (eval-under-env-c (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e) 
         (let ([v (eval-under-env-c (fst-e e) env)])
           (cond [(apair? v) (apair-e1 v)]
                 [#t (error "MUPL fst applied to non-apair")]))]
        [(snd? e) 
         (let ([v (eval-under-env-c (snd-e e) env)])
           (cond [(apair? v) (apair-e2 v)]
                 [#t (error "MUPL snd applied to non-apair")]))]
        [(aunit? e) e]
        [(isaunit? e) 
         (let ([v (eval-under-env-c (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
