#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Fall 2022
;;
;; Lab #7
;;
;; Yi Wang
;; W01553561
;; The purpose of this lab is to bulid an interpreter for Scheme.
;; To evaluate a precedure by using our own function.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide lookup 
         evaluate
         special-form?
         evaluate-special-form
         
         )


(define (lookup sb ev)
  (cond
    [(not (symbol? sb)) (error "Not symbol")] ;if it's not a symbol,error
    [(equal? sb (car(car ev))) (cadr(car ev))] ;if the symbol is the first element of environment
    [(equal? sb (caar ev)) (cadar ev)] ;if the symbol is the first element of environment
    [(empty? ev) (error "Not in Environment")] ;if the list is empty,so means the symbol is not in the list
    [else (lookup sb (cdr ev)) ];check if it's in the rest of the list(environment)
   )
)

(define (evaluate exp ev)
  (cond
    [(number? exp) exp] ;if the expression is number,return itself
    [(symbol? exp) (lookup exp ev)] ;if the expression is a symbol, call the lookup function
    [(not (list? exp)) (error "Not a List")] ;if the expression is not a list, error
    [(special-form? exp) (evaluate-special-form exp ev)] ;if the expression is a special-form, call the evaluate-special-form function to solve it
    [else (apply-function (evaluate (car exp) ev) (map (lambda (num) (evaluate num ev)) (cdr exp)))] ;evaluate the precedure by using lookup, distributes(map) over itself or sublist using map
  )
)

;(apply + '(1 2 3 4))
;10


(define (special-form? exp) ;check if the expression is "if" or "cond"
  (cond
    [(not(list? exp)) #f] ;return false when expression is not a list
    [(equal?(car exp) 'if) #t] ;return true when the first element of expression is 'if
    [(equal?(car exp) 'cond) #t] ;return true when the first element of expression is 'cond
    [(equal?(car exp) 'let) #t] ;return true when the first element of expression is 'let
    [(equal?(car exp) 'lambda) #t] ;return true when the first element of expression is 'lambda
    [(equal?(car exp) 'letrec) #t] ;return true when the first element of expression is 'letrec
    [else #f] ;else return false
  )
)

(define (evaluate-special-form exp ev)
  (cond
    [(equal? (car exp) 'if) ;if the first element of expression is 'if
    (cond
      [(evaluate (list-ref exp 1) ev) (evaluate (list-ref exp 2) ev)] ;if the first element inside 'if statement is true, return the second element
      [else (evaluate (list-ref exp 3) ev)] ;else evalute the third element of the 'if statemnet
    )]
    [(equal? (car exp) 'cond) (helper (cdr exp) ev)] ;if the first element of expression is 'cond, use the helper function
    [(equal? (car exp) 'let) (evaluate (caddr exp)(let-helper (cadr exp) ev))] ;if the first element of expression is 'let, evaluate the final expression inside 'let
    [(equal? (car exp) 'lambda) (closure (cadr exp) (caddr exp) ev)] ;if the first element of expression is 'let, create a closure then evaluate it
    [(equal? (car exp) 'letrec) (evaluate-letrec (cdr exp) ev)]
    [else (error "Unknown expression")] ; error when the expression isn't an If or Cond statement
  )
)

(define (helper exp ev) ;'cond, helper function of when the expression is 'cond
  (cond
    [(evaluate (caar exp) ev) (evaluate (car(cdar exp)) ev)] ;evaluate the first element of first element inside 
    [(evaluate (caar exp) ev) (evaluate (cadar exp) ev)] ;evaluate the first element of first element inside the cond and evaluate the second element of first element inside the cond 
    [else (helper (cdr exp) ev)] ;if the first conditional statement is false, we check the second conditional statement in the cond. 
  )
)

(define (let-helper exp ev) ;helper function of when the expression is 'let
  (append (map
      (lambda (exp)
        (list (car exp) (evaluate (cadr exp) ev))) exp) ev) ;append the list of symbol and expression that already evaluated to the old environment, which created a new environment
)


;from lab7 description
(define closure
(lambda (vars body env)
(mcons 'closure (mcons env (mcons vars body)))))

(define closure?
(lambda (clos) (and (mpair? clos) (eq? (mcar clos) 'closure))))

(define closure-env
(lambda (clos) (mcar (mcdr clos))))

(define closure-vars
(lambda (clos) (mcar (mcdr (mcdr clos)))))

(define closure-body
(lambda (clos) (mcdr (mcdr (mcdr clos)))))

(define set-closure-env!
(lambda (clos new-env) (set-mcar! (mcdr clos) new-env)))
;from lab7 description


(define (apply-function proc args) 
  (cond
    ;[(procedure? proc) (apply proc args)]
    [(closure? proc) (apply-closure proc args)];when the expression is a closure, call apply-closure function
    [else (apply proc args)] ;when the expression is a procedure, use Racket apply function
   )
)

(define (apply-closure clos ls) ;call it when expression is a closure, and append the stuff to the environment, then evaluate the expression.
  (evaluate (closure-body clos) (append (map list (closure-vars clos)ls) (closure-env clos))))

(define (evaluate-letrec exp env) ;evaluate letrec ecpression by using let*, and call modify-env function
  (let* [                         ;let* create the mini environment, and the new environment
         (mini-env (map
      (lambda (exp)
        (list (car exp) (evaluate (cadr exp) env))) (car exp))) 
  
         (new-env (append mini-env env))]
    
    (modify-env mini-env new-env)
    (evaluate (second exp) new-env)
   )
)

(define (modify-env mini-env new-env)
  (cond
    [(empty? mini-env) mini-env] ;if it's empty, do nothing
    [(closure? (cadar mini-env)) (set-closure-env! (cadar mini-env) new-env) (modify-env (cdr mini-env) new-env)] ;if the second thing in mini-env is closure
    [else (modify-env (cdr mini-env) new-env)] ;recursion call, check others in the mini-env
    
   )
)