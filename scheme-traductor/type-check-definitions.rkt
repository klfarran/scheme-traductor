#lang scheme

(require "helper-definitions.rkt")
(require "err-chks.rkt")

;;;;
;;;; Type-checking Definitions
;;;;

(define-syntax ¿átomo?
  (lambda (stx)
    (syntax-case stx ()
      [(¿átomo? . args)
       #'(inner-¿átomo? . args)] 
      [¿átomo?
       #'(displayln "#<procedimiento:¿átomo?>")]
      [else
       (error "\n¿átomo?: sintaxis no válida")])))

(define inner-¿átomo? ;atom?
  (lambda args 
    (if (arity-check (count-args args) 1 "¿átomo?")
       (cond
        ((atom? (car args)) 'cierto)
        (else 'falso) )
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿átomo?)

(define-syntax ¿número?
  (lambda (stx)
    (syntax-case stx ()
      [(¿número? . args)
       #'(inner-¿número? . args)] 
      [¿número?
       #'(displayln "#<procedimiento:¿número?>")]
      [else
       (error "\n¿número?: sintaxis no válida")])))

(define inner-¿número? ;number?
  (lambda args
     (if (arity-check (count-args args) 1 "¿número?")
    (cond
      ((number? (car args)) 'cierto)
      (else 'falso) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿número?)

(define-syntax ¿lista?
  (lambda (stx)
    (syntax-case stx ()
      [(¿lista? . args)
       #'(inner-¿lista? . args)] 
      [¿lista?
       #'(displayln "#<procedimiento:¿lista?>")]
      [else
       (error "\n¿lista?: sintaxis no válida")])))

(define inner-¿lista? ;list?
  (lambda args
    (if (arity-check (count-args args) 1 "¿lista?")
    (cond
      ((list? (car args)) 'cierto)
      (else 'falso) )
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿lista?)

(define-syntax ¿par?
  (lambda (stx)
    (syntax-case stx ()
      [(¿par? . args)
       #'(inner-¿par? . args)] 
      [¿par?
       #'(displayln "#<procedimiento:¿par?>")]
      [else
       (error "\n¿par?: sintaxis no válida")])))

(define inner-¿par? ;pair?
  (lambda args
    (if (arity-check (count-args args) 1 "¿par?")
    (cond
      ((pair? (car args)) 'cierto)
      (else 'falso) )
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿par?)

(define-syntax ¿símbolo?
  (lambda (stx)
    (syntax-case stx ()
      [(¿símbolo? . args)
       #'(inner-¿símbolo? . args)] 
      [¿símbolo?
       #'(displayln "#<procedimiento:¿símbolo?>")]
      [else
       (error "\n¿símbolo?: sintaxis no válida")])))

(define inner-¿símbolo? ;symbol?
  (lambda args
    (if (arity-check (count-args args) 1 "¿símbolo?")
     (cond
      ((symbol? (car args)) 'cierto)
      (else 'falso) )
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

 (provide ¿símbolo?)

(define-syntax ¿carácter?
  (lambda (stx)
    (syntax-case stx ()
      [(¿carácter? . args)
       #'(inner-¿carácter? . args)] 
      [¿carácter?
       #'(displayln "#<procedimiento:¿carácter?>")]
      [else
       (error "\n¿carácter?: sintaxis no válida")])))

(define inner-¿carácter? ;char?
  (lambda args
    (if (arity-check (count-args args) 1 "¿carácter?")
     (cond
      ((char? (car args)) 'cierto)
      (else 'falso) )
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

 (provide ¿carácter?)

(define-syntax ¿booleano?
  (lambda (stx)
    (syntax-case stx ()
      [(¿booleano? . args)
       #'(inner-¿booleano? . args)] 
      [¿booleano?
       #'(displayln "#<procedimiento:¿booleano?>")]
      [else
       (error "\n¿booleano?: sintaxis no válida")])))

(define inner-¿booleano? ;boolean?
  (lambda args
    (if (arity-check (count-args args) 1 "¿booleano?")
    (cond
      ((boolean? (car args)) 'cierto)
      (else 'falso) )
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿booleano?)

(define-syntax ¿inmutable?
  (lambda (stx)
    (syntax-case stx ()
      [(¿inmutable? . args)
       #'(inner-¿inmutable? . args)] 
      [¿inmutable?
       #'(displayln "#<procedimiento:¿inmutable?>")]
      [else
       (error "\n¿inmutable?: sintaxis no válida")])))

(define inner-¿inmutable? ; immutable?
  (lambda args
    (if (arity-check (count-args args) 1 "¿inmutable?")
    (cond
      ((immutable? (car args)) 'cierto)
      (else 'falso) )
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿inmutable?)
