#lang scheme

(require "helper-definitions.rkt")
(require "err-chks.rkt")

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

(define ¿ig? ;eq?
  (lambda args 
   (if (arity-check (count-args args) 2 "¿ig?")
      (cond
      ((eq? (car args) (cadr args)) 'cierto)
      ('falso) )
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿ig?)

(define ¿igv? ;eqv?
  (lambda args 
   (if (arity-check (count-args args) 2 "¿igv?")
      (cond
      ((eqv? (car args) (cadr args)) 'cierto)
      ('falso) )
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿igv?)

(define ¿igual? ;equal?
  (lambda args 
   (if (arity-check (count-args args) 2 "¿igual?")
      (cond
      ((equal? (car args) (cadr args)) 'cierto)
      ('falso) )
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

(define y ;and 
  (lambda args 
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
  (lambda args
    (if (arity-check (count-args args) -1 "o")
     (cond 
      ((null? (car args)) 'falso) ; at least 1 #t element not found 
      ((eq? (car args) #t) 'cierto) ; if any element is `cierto`, which evaluates to `#t`, return `cierto.
      ((eq? (car args) 'cierto) 'cierto)
      ((and (eq? (count-args args) 1) (false? (car args))) 'falso)
      ((and (eq? (count-args args) 1) (eq? (car args) 'falso)) 'falso)
      ((number? (car args)) (car args)) ;only one thing left in the list and its non #t #f, like a number, for example
      (else (o-helper (cdr args))) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación"))))

   (provide o)

(define no ;not
  (lambda args 
   (if (arity-check (count-args args) 1 "no") 
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
      ((eq? 'cierto (car args)) 'cierto)
       (else 'falso) )
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿cierto?)

(define ¿nulo? ;null?
  (lambda args 
   (if (arity-check (count-args args) 1 "¿nulo?") 
    (cond
      ((null? (car args)) 'cierto) 
       (else 'falso))
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿nulo?)


