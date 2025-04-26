#lang scheme

(require "helper-definitions.rkt")
(require "err-chks.rkt")
(require racket/trace)

;;;;
;;;; List Definitions
;;;;

(define-syntax combinar
  (lambda (stx)
    (syntax-case stx ()
      [(combinar . args)
       #'(inner-combinar . args)] 
      [combinar
       #'(displayln "#<procedimiento:combinar>")]
      [else
       (error "\ncombinar: sintaxis no válida")])))

(define inner-combinar ;cons
  (lambda args 
    (if (arity-check (count-args args) 2 "combinar") 
      (cons (car args) (cadr args) )
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide combinar)

(define-syntax lista
  (lambda (stx)
    (syntax-case stx ()
      [(lista . args)
       #'(inner-lista . args)] 
      [lista
       #'(displayln "#<procedimiento:lista>")]
      [else
       (error "\nlista: sintaxis no válida")])))

 (define inner-lista ;list
   (lambda args
     (cond
       ((= 0 (count-args args)) '())
       ((null? (cdr args)) (list (car args)))
       (else (cons (car args) (apply inner-lista (cdr args) )) ) )))

  (provide lista)

(define-syntax ¿nulo?
  (lambda (stx)
    (syntax-case stx ()
      [(¿nulo? . args)
       #'(inner-¿nulo? . args)] 
      [¿nulo?
       #'(displayln "#<procedimiento:¿nulo?>")]
      [else
       (error "\n¿nulo?: sintaxis no válida")])))

(define inner-¿nulo? ;null?
  (lambda args 
   (if (arity-check (count-args args) 1 "¿nulo?") 
    (cond
      ((null? (car args)) 'cierto) 
       (else 'falso))
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿nulo?)

(define-syntax juntar
  (lambda (stx)
    (syntax-case stx ()
      [(juntar . args)
       #'(inner-juntar . args)] 
      [juntar
       #'(displayln "#<procedimiento:juntar>")]
      [else
       (error "\njuntar: sintaxis no válida")])))

(define inner-juntar ;append
  (lambda args 
    (if (contract-viol-check args "lista lista... cualquiera" "juntar")
    (cond
      ((null? args) '())
      ((and (atom? (car args)) (= 1(count-args args))) (car args))
      (else (append (append-helper (all-but-last args)) (last-elem args))))
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))
   
  (provide juntar)

(define-syntax longitud
  (lambda (stx)
    (syntax-case stx ()
      [(longitud . args)
       #'(inner-longitud . args)] 
      [longitud
       #'(displayln "#<procedimiento:longitud>")]
      [else
       (error "\nlongitud: sintaxis no válida")])))

(define inner-longitud ;length
  (lambda args
    (if (and (arity-check (count-args args) 1 "longitud") (contract-viol-check args "(cualquier tipo)" "longitud"))
     (length (car args))
     (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))
   
  (provide longitud)

(define-syntax pri
  (lambda (stx)
    (syntax-case stx ()
      [(pri . args)
       #'(inner-pri . args)] 
      [pri
       #'(displayln "#<procedimiento:pri>")]
      [else
       (error "\npri: sintaxis no válida")])))

(define inner-pri ;car
  (lambda args
    (if (and (arity-check (count-args args) 1 "pri") (contract-viol-check args "(cualquiera)" "pri")) 
      (caar args)  
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

(provide pri)

(define-syntax prri
  (lambda (stx)
    (syntax-case stx ()
      [(prri . args)
       #'(inner-prri . args)] 
      [prri
       #'(displayln "#<procedimiento:prri>")]
      [else
       (error "\nprri: sintaxis no válida")])))

(define inner-prri ;caar
  (lambda args
    (if (and (arity-check (count-args args) 1 "prri") (contract-viol-check args "((cualquiera) cualquiera)" "prri"))
      (caaar args) 
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide prri)

(define-syntax prrri
  (lambda (stx)
    (syntax-case stx ()
      [(prrri . args)
       #'(inner-prrri . args)] 
      [prrri
       #'(displayln "#<procedimiento:prrri>")]
      [else
       (error "\nprrri: sintaxis no válida")])))

(define inner-prrri ;caaar
  (lambda args
    (if (and (arity-check (count-args args) 1 "prrri") (contract-viol-check args "(((cualquiera)) cualquiera)" "prrri"))
    (cond
      ((caaaar args) ) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) )) ;;must be caaaar because our arguments are encased in a list, so (car args) = list, and we want caaar of the list

  (provide prrri)

(define-syntax prrrri
  (lambda (stx)
    (syntax-case stx ()
      [(prrrri . args)
       #'(inner-prrrri . args)] 
      [prrrri
       #'(displayln "#<procedimiento:prrrri>")]
      [else
       (error "\nprrrri: sintaxis no válida")])))

(define inner-prrrri ;caaaar
  (lambda args
    (if (and (arity-check (count-args args) 1 "prrrri") (contract-viol-check args "((((cualquiera))) cualquiera)" "prrrri"))
        (car(caaaar args)) 
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) )) 

  (provide prrrri)

(define-syntax res
  (lambda (stx)
    (syntax-case stx ()
      [(res . args)
       #'(inner-res . args)] 
      [res
       #'(displayln "#<procedimiento:res>")]
      [else
       (error "\nres: sintaxis no válida")])))

(define inner-res ;cdr
  (lambda args
   (if (and (arity-check (count-args args) 1 "res") (contract-viol-check args "(cualquiera, al menos 1)" "res"))
      (cdar args)
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide res)

(define-syntax ress
  (lambda (stx)
    (syntax-case stx ()
      [(ress . args)
       #'(inner-ress . args)] 
      [ress
       #'(displayln "#<procedimiento:ress>")]
      [else
       (error "\nress: sintaxis no válida")])))

(define inner-ress ;cddr
  (lambda args
   (if (and (arity-check (count-args args) 1 "ress") (contract-viol-check args "(cualquiera, al menos 2)" "ress"))
     (cddar args) 
      (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ress)

(define-syntax resss
  (lambda (stx)
    (syntax-case stx ()
      [(resss . args)
       #'(inner-resss . args)] 
      [resss
       #'(displayln "#<procedimiento:resss>")]
      [else
       (error "\nresss: sintaxis no válida")])))

(define inner-resss ;cdddr
  (lambda args
   (if (and (arity-check (count-args args) 1 "resss") (contract-viol-check args "(cualquiera, al menos 3)" "resss"))
     (cdddar args)
      (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide resss)

(define-syntax ressss
  (lambda (stx)
    (syntax-case stx ()
      [(ressss . args)
       #'(inner-ressss . args)] 
      [ressss
       #'(displayln "#<procedimiento:ressss>")]
      [else
       (error "\nressss: sintaxis no válida")])))

(define inner-ressss ;cddddr
  (lambda args
    (if (and (arity-check (count-args args) 1 "ressss") (contract-viol-check args "(cualquiera, al menos 4)" "ressss"))
      (cdr(cdddar args))
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ressss)

(define-syntax pres
  (lambda (stx)
    (syntax-case stx ()
      [(pres . args)
       #'(inner-pres . args)] 
      [pres
       #'(displayln "#<procedimiento:pres>")]
      [else
       (error "\npres: sintaxis no válida")])))

(define inner-pres ; cadr
  (lambda args
    (if (and (arity-check (count-args args) 1 "pres") (contract-viol-check args "(al menos dos elementos)" "pres"))
     (cadar args) 
      (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide pres)

(define-syntax resp
  (lambda (stx)
    (syntax-case stx ()
      [(resp . args)
       #'(inner-resp . args)] 
      [resp
       #'(displayln "#<procedimiento:resp>")]
      [else
       (error "\nresp: sintaxis no válida")])))

(define inner-resp ;cdar
  (lambda args
    (if (and (arity-check (count-args args) 1 "resp")(contract-viol-check args "(lista cualquiera cualquiera ...)" "resp"))
       (cdar (car args)) 
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide resp)
