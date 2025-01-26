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

(define-syntax ¿ig?
  (lambda (stx)
    (syntax-case stx ()
      [(¿ig? . args)
       #'(inner-¿ig? . args)] 
      [¿ig?
       #'(displayln "#<procedimiento:¿ig?>")]
      [else
       (error "\n¿ig?: sintaxis no válida")])))

(define inner-¿ig? ;eq?
  (lambda args 
   (if (arity-check (count-args args) 2 "¿ig?")
      (cond
      ((eq? (car args) (cadr args)) 'cierto)
      ('falso) )
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿ig?)

(define-syntax ¿igv?
  (lambda (stx)
    (syntax-case stx ()
      [(¿igv? . args)
       #'(inner-¿igv? . args)] 
      [¿igv?
       #'(displayln "#<procedimiento:¿igv?>")]
      [else
       (error "\n¿igv?: sintaxis no válida")])))

(define inner-¿igv? ;eqv?
  (lambda args 
   (if (arity-check (count-args args) 2 "¿igv?")
      (cond
      ((eqv? (car args) (cadr args)) 'cierto)
      ('falso) )
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿igv?)

(define-syntax ¿igual?
  (lambda (stx)
    (syntax-case stx ()
      [(¿igual? . args)
       #'(inner-¿igual? . args)] 
      [¿igual?
       #'(displayln "#<procedimiento:¿igual?>")]
      [else
       (error "\n¿igual?: sintaxis no válida")])))

(define inner-¿igual? ;equal?
  (lambda args 
   (if (arity-check (count-args args) 2 "¿igual?")
      (cond
      ((equal? (car args) (cadr args)) 'cierto)
      ('falso) )
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿igual?)


;;Conditional procedures

(define-syntax si
  (lambda (stx)
    (syntax-case stx ()
      [(si . args)
       #'(inner-si . args)] 
      [si
       #'(displayln "#<procedimiento:si>")]
      [else
       (error "\nsi: sintaxis no válida")])))

(define inner-si ;if
  (lambda args 
    (if (arity-check (count-args args) 3 "si")
    (cond   
     ((eq? (car args) 'cierto) (cadr args)) ;if condition true, do the first operation (cadr) (else do the second operation, the caddr)
     (else (caddr args)) )
    (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación"))))

  (provide si) 

(define-syntax y
  (lambda (stx)
    (syntax-case stx ()
      [(y . args)
       #'(inner-y . args)] 
      [y
       #'(displayln "#<procedimiento:y>")]
      [else
       (error "\ny: sintaxis no válida")])))

(define inner-y ;and 
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

(define-syntax o
  (lambda (stx)
    (syntax-case stx ()
      [(o . args)
       #'(inner-o . args)] 
      [o
       #'(displayln "#<procedimiento:o>")]
      [else
       (error "\no: sintaxis no válida")])))

(define inner-o ;and 
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

(define-syntax no
  (lambda (stx)
    (syntax-case stx ()
      [(no . args)
       #'(inner-no . args)] 
      [no
       #'(displayln "#<procedimiento:no>")]
      [else
       (error "\nno: sintaxis no válida")])))

(define inner-no ;not
  (lambda args 
   (if (arity-check (count-args args) 1 "no") 
       (if (¿falso? (car args)) 'cierto 'falso)
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

   (provide no)

(define-syntax ¿falso?
  (lambda (stx)
    (syntax-case stx ()
      [(¿falso? . args)
       #'(inner-¿falso? . args)] 
      [¿falso?
       #'(displayln "#<procedimiento:¿falso?>")]
      [else
       (error "\n¿falso?: sintaxis no válida")])))

(define inner-¿falso? ;false?
  (lambda args
    (if (arity-check (count-args args) 1 "¿falso?")
    (cond
      ((or (false? (car args)) (eq? 'falso (car args))) 'cierto)
       (else 'falso) )
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿falso?)

(define-syntax ¿cierto?
  (lambda (stx)
    (syntax-case stx ()
      [(¿cierto? . args)
       #'(inner-¿cierto? . args)] 
      [¿cierto?
       #'(displayln "#<procedimiento:¿cierto?>")]
      [else
       (error "\n¿cierto?: sintaxis no válida")])))

(define inner-¿cierto? ; functions like true? if scheme included such a procedure
  (lambda args
    (if (arity-check (count-args args) 1 "¿cierto?")
    (cond
      ((eq? (¿falso? (car args) ) 'cierto) 'falso)
       (else 'cierto) )
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿cierto?)




