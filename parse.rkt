#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Fall 2022
;;
;; Lab #8
;;
;; Yi Wang
;; W01553561
;; 
;; This file has a parser for our interpreter.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide parse)
(require racket/trace) ;debugging

(define parse
  (lambda (str)
    (first (L (string->list str)))
  )
)

;L (expression list)
;L -> _L|EL|e(null)
(define L
  (lambda (input)
    (cond
      [(null? input) (cons '() '())]
      [(char-whitespace? (first input)) (L (rest input))]
      [(E? (first input)) (let* ((e-rest (E input))
                                 (l-rest (L (rest e-rest))))
                            (cons (cons (first e-rest) (first l-rest))
                                (rest l-rest)))]
      ;[(eq? (first input) #\)) (cons null (rest input))]
      [else (cons null input)]
    )
  )
)

;(trace L)

;E (expression)
;E -> DN|AS|(L)
(define E
  (lambda (input)
    (cond
      [(null? input) (cons '() null)]
      [(char-whitespace? (first input)) (L (rest input))]
      [(D? (first input)) (let* ((d-rest (D input))
                                 (n-rest (N (rest d-rest)(first d-rest))))
                            n-rest)]
      [(A? (first input)) (let* ((a-rest (A input))
                                 (s-rest (S (rest a-rest) (first a-rest))))
                            s-rest)]
      
      [(equal? (first input) #\() (let ((l-rest (L (rest input))))
                               (cons (first l-rest)
                                     (rest (rest l-rest))))]
    )
   )
)
;(trace E)

;S (symbol)
;S -> AS|e
(define S
  (lambda (input sys)
    (cond
      [(or (null? input)
           (not (A? (first input)))) (cons (string->symbol (list->string sys)) input)]
      [else (let* ((a-rest (A input))
                   (s-rest (S (rest a-rest)
                              (append sys (list (car input)))
                              )))
                   s-rest)]    
    )
  )
)
;(trace S)

;N (number)
;N -> DN|e
(define N
  (lambda (input num)
    (cond
     [(or (null? input)
          (not (D? (first input)))) (cons num input)]

     [else (let* ((d-rest (D input))
                  (n-rest (N (rest d-rest)
                             (+ (* num 10)
                                (first d-rest))
                             )))
             n-rest)]
     )
    )
  )
;(trace N)

;D (digits: 1234)
(define D
  (lambda (input)
    (cond
     [(null? input) (error (string-append "Not a digit:"
                                          (list->string input)))]
     
     [(D? (first input)) (cons (char->number (first input))
                               (rest input))]
           
      [else (error (string-append "Not a digit:"
                                  (list->string input)))]
      )
    )
  )
;(trace D)

;A (symbolic: abc)
(define A
  (lambda (input)
    (cond
      [(null? input) (error (string-append "Not a Symbol:"
                                           (list->string input)))]
      
      [(A? (first input)) (cons (cons (first input) null) (rest input))]
      
      [else (error (string-append "Not a Symbol:"
                                  (list->string input)))]
    )
   )
 )
;(trace A)

(define char-symbolic?
  (lambda (char) (and (not (char-whitespace? char))
                      (not (eq? char #\())
                      (not (eq? char #\))))))

(define D? char-numeric?)
(define A? char-symbolic?)


;use in L, check is it's D?, A?, or #\(
(define E?
  (lambda (input)
  (or
    (D? input)
    (A? input)
    (equal? #\( input))
  )
)

(define char->number
  (lambda (char)
    (- (char->integer char)
       (char->integer #\0))))


;(parse "(1 2 3)")