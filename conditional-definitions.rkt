#lang scheme

(require "helper-definitions.rkt")
(require "error-checkers.rkt")
(require racket/trace)

;;;;
;;;; Boolean/ Conditional Definitions
;;;;


(define falso ;false
  #f)

   (provide falso)

(define cierto ;true
  #t)

   (provide cierto)


;;Procedures for determining equality

(define ¿ig? ;calls eq?, which checks if two values represent the same object in memory 
  (lambda args ;all of our parameteres, should be two:an atom or a list, and an atom or a list 
   (if (arity-check (count-args args) 2 "¿ig?") 
       (eq? (car args) (cadr args))
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿ig?)

(define ¿igv? ;calls eqv?, works the same as eq? but always returns true for (equal) primitive values 
  (lambda args ;all of our parameters, should be two: an atom or a list, and an atom or a list
   (if (arity-check (count-args args) 2 "¿igv?")
       (eqv? (car args) (cadr args))
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿igv?)

(define ¿igual? ;calls equal?, which checks equality (by eqv? function) between two atoms or between two lists by calling eqv? on all elements in the list 
  (lambda args ;all of our parameters, should be two: an atom or a list, and an atom or a list
   (if (arity-check (count-args args) 2 "¿igual?")
       (equal? (car args) (cadr args))
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿igual?)


;;Conditional procedures

(define si ;if
  (lambda args 
    (if (arity-check (count-args args) 3 "si")
    (cond   
     ((eq? (car args) 'cierto) (cadr args)) ;if condition true, do the first operation (cadr) (else do the second operation, the caddr)
     (else (caddr args)) )
    (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación"))))

  (provide si) 

(define sino ;else
  (lambda args
    (if (arity-check (count-args args) 1 "sino")
      (car args)
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

   (provide sino) 

(define y ;and 
  (lambda args ;all parameters, should be two: an atom or a list and an atom or a list
    (if (arity-check (count-args args) -1 "y")
     (cond 
      ((null? (car args)) 'cierto) ; no #f element(s)
      ((eq? (car args) #f) 'falso) ; if any element is `falso`, which evaluates to `#f`, return `falso.
      ((and (eq? (count-args args) 1) (eq? (car args) #t) 'cierto))
      ((eq? (count-args args) 1) (car args)) ;only one thing left in the list and its non #t #f, like a number, for example
      (else (y (cadr args))) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación"))))

   (provide y)

(define o ;and 
  (lambda args ;all parameters, should be two: an atom or a list and an atom or a list
    (if (arity-check (count-args args) -1 "o")
     (cond 
      ((null? (car args)) 'falso) ; no #t element(s)
      ((eq? (car args) #t) 'cierto) ; if any element is `cierto`, which evaluates to `#t`, return `cierto.
      ((and (eq? (count-args args) 1) (eq? (car args) #t) 'cierto))
      ((and (eq? (count-args args) 1) (false? (car args))) 'falso)
      ((eq? (count-args args) 1) (car args)) ;only one thing left in the list and its non #t #f, like a number, for example
      (else (o (cadr args))) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación"))))

   (provide o)

(define no ;not
  (lambda args ;all parameters, should be one: an atom or a list (or a condition, which is technically an atom)
   (if (arity-check (count-args args) 1 "no") 
     ;(not (car args) )
       (if (¿falso? (car args)) 'cierto 'falso)
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

   (provide no)

(define ¿falso? ;false?
  (lambda args
    (if (arity-check (count-args args) 1 "¿falso?")
    (cond
      ((false? (car args)) 'cierto)
       (else 'falso) )
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿falso?)


(define ¿cierto? ; functions like true? if scheme included such a procedure
  (lambda args
    (if (arity-check (count-args args) 1 "¿cierto?")
    (cond
      ((eq? #t (car args)) 'cierto)
       (else 'falso) )
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿cierto?)

(define ¿nulo? ;null?
  (lambda args ; all of our parameters, should be one:either an atom or a list
   (if (arity-check (count-args args) 1 "¿nulo?") 
    (cond
      ((null? (car args)) 'cierto) 
       (else 'falso))
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿nulo?)

(define-syntax condición
  (syntax-rules ()
    ((condición (condition result) ...)
     ;(cond ((and (¿cierto? condition) (not(¿falso? condition))) result)
     (cond ((eq? condition 'cierto) result)
           ...))))

(provide condición)

