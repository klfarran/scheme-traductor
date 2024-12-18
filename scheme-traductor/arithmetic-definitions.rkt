#lang scheme

(require "helper-definitions.rkt")
(require "err-chks.rkt")
(require "conditional-definitions.rkt")

;;;;
;;;; Arithmetic Definitions
;;;;

(define más ; +
  (lambda args
    (if (and (arity-check (count-args args) -1 "más") (contract-viol-check args "número número ..." "más"))
    (cond
      ((= 0 (count-args args)) 0)
      ((and (= 1 (count-args args)) (not (list? (car args))) (car args))) ;if given 3, we return 3, but if given (3), its an error
      (else (más-helper args) ) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide más)

(define menos ; -
  (lambda args
    (if (and (arity-check (count-args args) -1 "menos") (contract-viol-check args "número número ..." "menos"))
    (cond
      ((= 1 (count-args args)) (- 0 (car args))) 
      (else (menos-helper args) ) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide menos)

(define mult ; *
  (lambda args
    (if (and (arity-check (count-args args) -1 "mult") (contract-viol-check args "número número ..." "mult"))
    (cond
      ((= 1 (count-args args)) (car args))
      (else (mult-helper args) ) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

  (provide mult)

(define div ; /
  (lambda args
    (if (and (arity-check (count-args args) -1 "div") (contract-viol-check args "número número ..." "div") (div-by-zero-check args "div"))
    (cond
      ((= 1 (count-args args)) (/ 1 (car args))) 
      (else (div-helper args)) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide div)

(define ¿número-entero? ;integer?
  (lambda args
    (if (arity-check (count-args args) 1 "¿número-entero?")
    (cond
     ((integer? (car args)) 'cierto)
      ('falso) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿número-entero?)

(define ¿cero? ;zero?
  (lambda args
    (if (and (arity-check (count-args args) 1 "¿cero?") (contract-viol-check args "número" "¿cero?"))
    (cond
      ((zero? (car args)) 'cierto)
      ('falso) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿cero?)

(define ¿positivo?  ;positive?
  (lambda args
    (if (and (arity-check (count-args args) 1 "¿positivo?") (contract-viol-check args "número" "¿positivo?"))
    (cond
      ((positive? (car args)) 'cierto)
      ('falso) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿positivo?)

(define ¿negativo? ;negative?
  (lambda args
    (if (and (arity-check (count-args args) 1 "¿negativo?") (contract-viol-check args "número" "¿negativo?"))
    (cond
      ((negative? (car args)) 'cierto)
      ('falso) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿negativo?) 

(define ¿número-par? ;even?
  (lambda args
    (if (and (arity-check (count-args args) 1 "¿número-par?") (contract-viol-check args "número" "¿número-par?"))
    (cond
      ((even? (car args)) 'cierto)
      ('falso) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿número-par?)

(define ¿número-impar? ;odd?
  (lambda args
    (if (and (arity-check (count-args args) 1 "¿número-impar?") (contract-viol-check args "número" "¿número-impar?") )
    (cond
      ((odd? (car args)) 'cierto)
      (else 'falso) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿número-impar?)

(define menos-de ; <
  (lambda args
    (if (and (arity-check (count-args args) -1 "menos-de") (contract-viol-check args "número número ..." "menos-de"))
     (menos-de-helper args)
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide menos-de)

(define más-de ; >
  (lambda args
    (if (and (arity-check (count-args args) -1 "más-de") (contract-viol-check args "número número ..." "más-de"))
      (más-de-helper args) 
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide más-de)

(define raíz-cuadrada ;sqrt
  (lambda args
    (if (and (arity-check (count-args args) 1 "raíz-cuadrada") (contract-viol-check args "número" "raíz-cuadrada") )
    (cond
      ((sqrt (car args))) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide raíz-cuadrada)
  

(define expo ;expt
  (lambda args
    (if (and (arity-check (count-args args) 2 "expo")(contract-viol-check args "número número" "expo"))
      (expt (car args) (cadr args)) 
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide expo)


(define máx ;max
  (lambda args
    (if (and (arity-check (count-args args) -1 "máx")(contract-viol-check args "número número ..." "máx"))
          (máx-helper args)
           (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

(provide máx)

(define mín ;min
  (lambda args
    (if (and (arity-check (count-args args) -1 "mín")(contract-viol-check args "número número ..." "mín"))
        (mín-helper args)
           (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

(provide mín)

(define val-abso ;abs
  (lambda args
    (if (and (arity-check (count-args args) 1 "val-abso")(contract-viol-check args "número" "val-abso"))
        (abs (car args))
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

(provide val-abso)

(define cociente ;quotient
  (lambda args
    (if (and (arity-check (count-args args) 2 "cociente")(contract-viol-check args "número número" "cociente"))
        (quotient (car args) (cadr args))
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

(provide cociente)

(define residuo ;remainder
  (lambda args
    (if (and (arity-check (count-args args) 2 "residuo")(contract-viol-check args "número número" "residuo"))
        (remainder (car args) (cadr args))
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

(provide residuo)

(define cociente/residuo ;quotient/remainder
  (lambda args
    (if (and (arity-check (count-args args) 2 "cociente/residuo")(contract-viol-check args "número número" "cociente/residuo"))
        (quotient/remainder (car args) (cadr args))
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

(provide cociente/residuo)

(define módulo ;modulo
  (lambda args
    (if (and (arity-check (count-args args) 2 "módulo")(contract-viol-check args "número número" "módulo"))
        (modulo (car args) (cadr args))
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

(provide módulo)


(define redondear ;round
  (lambda args
    (if (and (arity-check (count-args args) 1 "redondear")(contract-viol-check args "número" "redondear"))
        (round (car args))
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

(provide redondear)


(define truncar ;truncate
  (lambda args
    (if (and (arity-check (count-args args) 1 "truncar")(contract-viol-check args "número" "truncar"))
        (truncate (car args))
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

(provide truncar)

(define signo ;sgn
  (lambda args
    (if (and (arity-check (count-args args) 1 "signo")(contract-viol-check args "número" "signo"))
        (sgn (car args))
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

(provide signo)