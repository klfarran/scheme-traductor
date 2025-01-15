#lang scheme

(require "helper-definitions.rkt")
(require "err-chks.rkt")


;;;;
;;;; List Definitions
;;;;

(define combinar ;cons
  (lambda args 
    (if (arity-check (count-args args) 2 "combinar") 
      (cons (car args) (cadr args) )
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide combinar)

 (define lista ;list
   (lambda args
     (cond
       ((= 0 (count-args args)) '())
       ((null? (cdr args)) (list (car args)))
       ((cons (car args) (lista (cadr args) )) ) )))

  (provide lista)

(define ¿nulo? ;null?
  (lambda args 
   (if (arity-check (count-args args) 1 "¿nulo?") 
    (cond
      ((null? (car args)) 'cierto) 
       (else 'falso))
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿nulo?)

(define concatenar ;append
  (lambda args 
    (if (contract-viol-check args "lista lista... cualquiera" "concatenar")
    (cond
      ((null? args) '())
      ((and (atom? (car args)) (= 1(count-args args))) (car args))
      (else (append (append-helper (all-but-last args)) (last-elem args))))
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))
   
  (provide concatenar)

(define longitud ;length
  (lambda args
    (if (and (arity-check (count-args args) 1 "longitud") (contract-viol-check args "(cualquiera)" "longitud"))
     (length (car args))
     (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

  (provide longitud)

(define pri ;car
  (lambda args
    (if (and (arity-check (count-args args) 1 "pri") (contract-viol-check args "(cualquiera)" "pri")) 
      (caar args)  
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

;(define-syntax pri ;pri (supongo)
 ; (lambda (stx)
  ;  (syntax-case stx ()
   ;   [(_ args)
    ;      (if ;(and
     ;          (macro-arity-check (count-args (syntax->datum #'args)) 1 "pri" )
      ;     ;(contract-viol-check (syntax->datum #'args) "(cualquiera)" "pri"))
       ;    (caar (syntax->datum #'args)) (error "error"))
        ;  ]
   ;   [_ 
    ;   (error "\n pri: syntax no valido")])))

 (define-syntax pritest ;pri (supongo)
  (lambda (stx)
    (syntax-case stx ()
     [(_ args)
      (begin
          #'(if ;(and
               (arity-check (count-args args) 1 "pri" )
           ;(contract-viol-check (syntax->datum #'args) "(cualquiera)" "pri"))
           (caar (syntax->datum #'args)) (error "error")) )
          ]
      [(_ arg1 arg2)
       (error "boo")]
      [_ 
       (error "\n pri: syntaxis no valido")])))


  (provide pri)

(define prri ;caar
  (lambda args
    (if (and (arity-check (count-args args) 1 "prri") (contract-viol-check args "((cualquiera) cualquiera)" "prri"))
      (caaar args) 
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide prri)
  
(define prrri ;caaar
  (lambda args
    (if (and (arity-check (count-args args) 1 "prrri") (contract-viol-check args "(((cualquiera)) cualquiera)" "prrri"))
    (cond
      ((caaaar args) ) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) )) ;;must be caaaar because our arguments are encased in a list, so (car args) = list, and we want caaar of the list

  (provide prrri)
  
(define prrrri ;caaaar
  (lambda args
    (if (and (arity-check (count-args args) 1 "prrrri") (contract-viol-check args "((((cualquiera))) cualquiera)" "prrrri"))
        (car(caaaar args)) 
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) )) 

  (provide prrrri)
  
(define res ;cdr
  (lambda args
   (if (and (arity-check (count-args args) 1 "res") (contract-viol-check args "(cualquiera, al menos 1)" "res"))
      (cdar args)
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide res)

(define ress ;cddr
  (lambda args
   (if (and (arity-check (count-args args) 1 "ress") (contract-viol-check args "(cualquiera, al menos 2)" "ress"))
     (cddar args) 
      (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ress)

(define resss ;cdddr
  (lambda args
   (if (and (arity-check (count-args args) 1 "resss") (contract-viol-check args "(cualquiera, al menos 3)" "resss"))
     (cdddar args)
      (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide resss)

(define ressss ;cddddr
  (lambda args
    (if (and (arity-check (count-args args) 1 "ressss") (contract-viol-check args "(cualquiera, al menos 4)" "ressss"))
      (cdr(cdddar args))
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ressss)

(define pres ; cadr
  (lambda args
    (if (and (arity-check (count-args args) 1 "pres") (contract-viol-check args "(al menos dos elementos)" "pres"))
     (cadar args) 
      (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide pres)


(define resp ;cdar
  (lambda args
    (if (and (arity-check (count-args args) 1 "resp")(contract-viol-check args "(lista cualquiera cualquiera ...)" "resp"))
       (cdar (car args)) 
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide resp)
