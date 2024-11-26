#lang scheme

(require "helper-definitions.rkt")
(require "error-checkers.rkt")
(require racket/trace)

;;;;
;;;; Type-checking Definitions
;;;;

(define ¿átomo? ;atom?
  (lambda args ;all parameters, should be one: an atom or a list
    (if (arity-check (count-args args) 1 "¿átomo?")
       (cond
        ((atom? (car args)) 'cierto)
        (else 'falso) )
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿átomo?)

(define ¿número? ;number?
  (lambda args
     (if (arity-check (count-args args) 1 "¿número?")
    (cond
      ((number? (car args)) 'cierto)
      (else 'falso) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿número?)

(define ¿lista? ;list?
  (lambda args
    (if (arity-check (count-args args) 1 "¿lista?")
    (cond
      ((list? (car args)) 'cierto)
      (else 'falso) )
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿lista?)

(define ¿par? ;pair?
  (lambda args
    (if (arity-check (count-args args) 1 "¿par?")
    (cond
      ((pair? (car args)) 'cierto)
      (else 'falso) )
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿par?)

(define ¿símbolo? ;symbol?
  (lambda args
    (if (arity-check (count-args args) 1 "¿símbolo?")
     (cond
      ((symbol? (car args)) 'cierto)
      (else 'falso) )
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

 (provide ¿símbolo?)

(define ¿carácter? ;char?
  (lambda args
    (if (arity-check (count-args args) 1 "¿carácter?")
     (cond
      ((char? (car args)) 'cierto)
      (else 'falso) )
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

 (provide ¿carácter?)

(define ¿booleano? ;boolean?
  (lambda args
    (if (arity-check (count-args args) 1 "¿booleano?")
    (cond
      ((boolean? (car args)) 'cierto)
      (else 'falso) )
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿booleano?)

(define ¿inmutable?; immutable?
  (lambda args
    (if (arity-check (count-args args) 1 "¿inmutable?")
    (cond
      ((immutable? (car args)) 'cierto)
      (else 'falso) )
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿inmutable?)
