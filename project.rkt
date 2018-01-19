;; PL Project - Fall 2017
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; Add the missing ones

(struct var  (string)    #:transparent)
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct munit   ()      #:transparent) ;; unit value -- good for ending a list

(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct mult (e1 e2)  #:transparent)  ;; multiply two expressions
(struct neg  (num)    #:transparent)

(struct islthan (e1 e2)    #:transparent)
(struct ifzero (e1 e2 e3)    #:transparent)
(struct ifgthan (e1 e2 e3 e4)    #:transparent)
(struct ismunit (e)     #:transparent) ;; if e1 is unit then 1 else 0

(struct mlet (s e1 e2)    #:transparent)

(struct apair (e1 e2) #:transparent)
(struct first (e1) #:transparent)
(struct second (e1) #:transparent)

(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct closure (env fun) #:transparent) ;; a closure is not in "source" programs; it is what functions evaluate to


;; Problem 1

(define (racketlist->numexlist xs)
  (cond [(null? xs) (munit)]
        [(list? xs) (apair (car xs) (racketlist->numexlist (cdr xs)))]
        [#t (error ("it's not a racket list"))]
  )
)
(define (numexlist->racketlist xs)
  (cond [(munit? xs) null]
        [(apair? xs) (cons (apair-e1 xs) (numexlist->racketlist (apair-e2 xs)))]
        [#t (error ("it's not a numex list"))]
  )
)

;; Problem 2

;; lookup a variable in an environment
;; Complete this function

(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)])
  (cond [(equal? str (car (car env))) (cdr (car env))]
        [else (envlookup (cdr env) str)]
  )
)

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) (envlookup env (var-string e))]
        [(int? e)
         (cond [(integer? (int-num e)) e]
               [else (error "NUMEX int applied to non racket integer")])]
        [(munit? e) e]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "NUMEX addition applied to non-number")))]
        [(mult? e)
         (let ([v1 (eval-under-env (mult-e1 e) env)]
               [v2 (eval-under-env (mult-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (* (int-num v1) 
                       (int-num v2)))
               (error "NUMEX multiplication applied to non-number")))]
        [(neg? e)
         (let ([v (eval-under-env (neg-num e) env)])
           (if (int? v)
               (int (- 0 (int-num v)))
               (error "NUMEX negation applied to non-number")))]
        [(islthan? e)
         (let ([v1 (eval-under-env (islthan-e1 e) env)]
               [v2 (eval-under-env (islthan-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (< (int-num v1) (int-num v2)) (int 1) (int 0))
               (error "NUMEX islthan applied to non-number")))]
        [(ifzero? e)
         (let ([v1 (eval-under-env (ifzero-e1 e) env)])
           (if (int? v1)
               (if (equal? (int-num v1) 0)
                   (eval-under-env (ifzero-e2 e) env)
                   (eval-under-env (ifzero-e3 e) env))
               (error "NUMEX iszero applied to non-number")))]
        [(ifgthan? e)
         (let ([v1 (eval-under-env (ifgthan-e1 e) env)]
               [v2 (eval-under-env (ifgthan-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgthan-e3 e) env)
                   (eval-under-env (ifgthan-e4 e) env))
               (error "NUMEX ifgthan applied to non-number")))]
        [(mlet? e)
         (let ([v1 (eval-under-env (mlet-e1 e) env)])
           (if (string? (mlet-s e))
               (eval-under-env (mlet-e2 e) (cons (cons (mlet-s e) v1) env))
               (error "NUMEX mlet applied to non-number or the name of the variable is not a string")))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
                (apair v1 v2)
               )]
        [(first? e)
         (let ([v (eval-under-env (first-e1 e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "NUMEX first applied to non-apair" e)
               ))]
        [(second? e)
         (let ([v (eval-under-env (second-e1 e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "NUMEX second applied to non-apair")
               ))]
        [(ismunit? e)
         (let ([v (eval-under-env (ismunit-e e) env)])
           (if (munit? v) (int 1) (int 0)))]

        [(fun? e)
         (if (and (or (string? (fun-nameopt e)) (null? (fun-nameopt e))) (string? (fun-formal e)))
             (closure env e)
             (error "NUMEX function name and parameter name must be string"))]
        [(call? e)
         (let ([v (eval-under-env (call-actual e) env)]
               [clsr (eval-under-env (call-funexp e) env)])
           (if (closure? clsr)
               (let ([clsrFun (closure-fun clsr)])
                 (if (null? (fun-nameopt clsrFun))
                     (eval-under-env (fun-body clsrFun) (cons (cons (fun-formal clsrFun) v) (closure-env clsr)))
                     (eval-under-env (fun-body clsrFun) (cons (cons (fun-nameopt clsrFun) clsr) (cons (cons (fun-formal clsrFun) v) (closure-env clsr))))))
               (error "NUMEX call applied to non-function" e)))]
         [(closure? e) e]
         
        ;; CHANGE add more cases here
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
      
;; Problem 3 ; NUMEX Macros

(define (ifmunit e1 e2 e3)
  (ifgthan (ismunit e1) (int 0) e2 e3))

(define (mlet* bs e2)
  (if (equal? bs null)
      e2
      (mlet (car (car bs)) (cdr (car bs)) (mlet* (cdr bs) e2))))
  

(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1 (mlet "_y" e2
                      (ifgthan (var "_x") (var "_y") e4
                               (ifgthan (var "_y") (var "_x") e4 e3)))))

;; Problem 4

(define numex-map
  (fun null "_map-func"
  (fun "_self" "_nList"
       (ifzero (ismunit (var "_nList"))
                        (apair (call (var "_map-func") (first (var "_nList")))
                               (call (var "_self") (second (var "_nList"))))
                        (munit)))))

(define numex-mapAddN
  (mlet "map" numex-map
        (fun null "i"
             (call (var "map") (fun null "xL" (add (var "xL") (var "i")))))))



;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
;;