#lang scheme

(require "helper-definitions.rkt")
(require "err-chks.rkt")
;(require "conditional-definitions.rkt")

;;;;
;;;; Arithmetic Definitions
;;;;

(define-syntax más
  (lambda (stx)
    (syntax-case stx ()
      [(más . args)
       #'(inner-más . args)] 
      [más
       #'(displayln "#<procedimiento:más>")]
      [else
       (error "\nmás: sintaxis no válida")])))

(define inner-más ; +
  (lambda args
    (if (contract-viol-check args "número número ..." "más")
    (cond
      ((= 0 (count-args args)) 0)
      ((and (= 1 (count-args args)) (not (list? (car args))) (car args))) ;if given 3, we return 3, but if given (3), its an error
      (else (más-helper args) ) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide más)

(define-syntax menos
  (lambda (stx)
    (syntax-case stx ()
      [(menos . args)
       #'(inner-menos . args)] 
      [menos
       #'(displayln "#<procedimiento:menos>")]
      [else
       (error "\nmenos: sintaxis no válida")])))

(define inner-menos ; -
  (lambda args
    (if (and (arity-check (count-args args) -1 "menos") (contract-viol-check args "número número ..." "menos"))
    (cond
      ((= 1 (count-args args)) (- 0 (car args))) 
      (else (menos-helper args) ) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide menos)

(define-syntax mult
  (lambda (stx)
    (syntax-case stx ()
      [(mult . args)
       #'(inner-mult . args)] 
      [mult
       #'(displayln "#<procedimiento:mult>")]
      [else
       (error "\nmult: sintaxis no válida")])))

(define inner-mult ; *
  (lambda args
    (if (contract-viol-check args "número número ..." "mult")
    (cond
      ((= 0 (count-args args)) 1)
      ((= 1 (count-args args)) (car args))
      (else (mult-helper args) ) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

  (provide mult)

(define-syntax div
  (lambda (stx)
    (syntax-case stx ()
      [(div . args)
       #'(inner-div . args)] 
      [div
       #'(displayln "#<procedimiento:div>")]
      [else
       (error "\ndiv: sintaxis no válida")])))

(define inner-div ; /
  (lambda args
    (if (and (arity-check (count-args args) -1 "div") (contract-viol-check args "número número ..." "div") (div-by-zero-check args "div"))
    (cond
      ((= 1 (count-args args)) (/ 1 (car args))) 
      (else (div-helper args)) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide div)

(define-syntax ¿número-entero?
  (lambda (stx)
    (syntax-case stx ()
      [(¿número-entero? . args)
       #'(inner-¿número-entero? . args)] 
      [¿número-entero?
       #'(displayln "#<procedimiento:¿número-entero?>")]
      [else
       (error "\n¿número-entero?: sintaxis no válida")])))

(define inner-¿número-entero? ;integer?
  (lambda args
    (if (arity-check (count-args args) 1 "¿número-entero?")
    (cond
     ((integer? (car args)) 'cierto)
      ('falso) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿número-entero?)

(define-syntax ¿cero?
  (lambda (stx)
    (syntax-case stx ()
      [(¿cero? . args)
       #'(inner-¿cero? . args)] 
      [¿cero?
       #'(displayln "#<procedimiento:¿cero?>")]
      [else
       (error "\n¿cero?: sintaxis no válida")])))

(define inner-¿cero? ;zero?
  (lambda args
    (if (and (arity-check (count-args args) 1 "¿cero?") (contract-viol-check args "número" "¿cero?"))
    (cond
      ((zero? (car args)) 'cierto)
      ('falso) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿cero?)

(define-syntax ¿positivo?
  (lambda (stx)
    (syntax-case stx ()
      [(¿positivo? . args)
       #'(inner-¿positivo? . args)] 
      [¿positivo?
       #'(displayln "#<procedimiento:¿positivo?>")]
      [else
       (error "\n¿positivo?: sintaxis no válida")])))

(define inner-¿positivo?  ;positive?
  (lambda args
    (if (and (arity-check (count-args args) 1 "¿positivo?") (contract-viol-check args "número" "¿positivo?"))
    (cond
      ((positive? (car args)) 'cierto)
      ('falso) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿positivo?)

(define-syntax ¿negativo?
  (lambda (stx)
    (syntax-case stx ()
      [(¿negativo? . args)
       #'(inner-¿negativo? . args)] 
      [¿negativo?
       #'(displayln "#<procedimiento:¿negativo?>")]
      [else
       (error "\n¿negativo?: sintaxis no válida")])))

(define inner-¿negativo? ;negative?
  (lambda args
    (if (and (arity-check (count-args args) 1 "¿negativo?") (contract-viol-check args "número" "¿negativo?"))
    (cond
      ((negative? (car args)) 'cierto)
      ('falso) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿negativo?)

(define-syntax ¿número-par?
  (lambda (stx)
    (syntax-case stx ()
      [(¿número-par? . args)
       #'(inner-¿número-par? . args)] 
      [¿número-par?
       #'(displayln "#<procedimiento:¿número-par?>")]
      [else
       (error "\n¿número-par?: sintaxis no válida")])))


(define inner-¿número-par? ;even?
  (lambda args
    (if (and (arity-check (count-args args) 1 "¿número-par?") (contract-viol-check args "número" "¿número-par?"))
    (cond
      ((even? (car args)) 'cierto)
      ('falso) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿número-par?)

(define-syntax ¿número-impar?
  (lambda (stx)
    (syntax-case stx ()
      [(¿número-impar? . args)
       #'(inner-¿número-impar? . args)] 
      [¿número-impar?
       #'(displayln "#<procedimiento:¿número-impar?>")]
      [else
       (error "\n¿número-impar?: sintaxis no válida")])))

(define inner-¿número-impar? ;odd?
  (lambda args
    (if (and (arity-check (count-args args) 1 "¿número-impar?") (contract-viol-check args "número" "¿número-impar?") )
    (cond
      ((odd? (car args)) 'cierto)
      (else 'falso) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿número-impar?)

(define-syntax menos-de
  (lambda (stx)
    (syntax-case stx ()
      [(menos-de . args)
       #'(inner-menos-de . args)] 
      [menos-de
       #'(displayln "#<procedimiento:menos-de>")]
      [else
       (error "\nmenos-de: sintaxis no válida")])))

(define inner-menos-de ; <
  (lambda args
    (if (and (arity-check (count-args args) -1 "menos-de") (contract-viol-check args "número número ..." "menos-de"))
     (menos-de-helper args)
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide menos-de)

(define-syntax más-de
  (lambda (stx)
    (syntax-case stx ()
      [(más-de . args)
       #'(inner-más-de . args)] 
      [más-de
       #'(displayln "#<procedimiento:más-de>")]
      [else
       (error "\nmás-de: sintaxis no válida")])))

(define inner-más-de ; >
  (lambda args
    (if (and (arity-check (count-args args) -1 "más-de") (contract-viol-check args "número número ..." "más-de"))
      (más-de-helper args) 
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide más-de)

(define-syntax raíz-cuadrada
  (lambda (stx)
    (syntax-case stx ()
      [(raíz-cuadrada . args)
       #'(inner-raíz-cuadrada . args)] 
      [raíz-cuadrada
       #'(displayln "#<procedimiento:raíz-cuadrada>")]
      [else
       (error "\nraíz-cuadrada: sintaxis no válida")])))

(define inner-raíz-cuadrada ;sqrt
  (lambda args
    (if (and (arity-check (count-args args) 1 "raíz-cuadrada") (contract-viol-check args "número" "raíz-cuadrada") )
    (cond
      ((sqrt (car args))) )
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide raíz-cuadrada)

(define-syntax expo
  (lambda (stx)
    (syntax-case stx ()
      [(expo . args)
       #'(inner-expo . args)] 
      [expo
       #'(displayln "#<procedimiento:expo>")]
      [else
       (error "\nexpo: sintaxis no válida")])))

(define inner-expo ;expt
  (lambda args
    (if (and (arity-check (count-args args) 2 "expo")(contract-viol-check args "número número" "expo"))
      (expt (car args) (cadr args)) 
       (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide expo)

(define-syntax máx
  (lambda (stx)
    (syntax-case stx ()
      [(máx . args)
       #'(inner-máx . args)] 
      [máx
       #'(displayln "#<procedimiento:máx>")]
      [else
       (error "\nmáx: sintaxis no válida")])))

(define inner-máx ;max
  (lambda args
    (if (and (arity-check (count-args args) -1 "máx")(contract-viol-check args "número número ..." "máx"))
          (máx-helper args)
           (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

(provide máx)

(define-syntax mín
  (lambda (stx)
    (syntax-case stx ()
      [(mín . args)
       #'(inner-máx . args)] 
      [mín
       #'(displayln "#<procedimiento:mín>")]
      [else
       (error "\nmín: sintaxis no válida")])))

(define inner-mín ;min
  (lambda args
    (if (and (arity-check (count-args args) -1 "mín")(contract-viol-check args "número número ..." "mín"))
        (mín-helper args)
           (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

(provide mín)

(define-syntax val-abso
  (lambda (stx)
    (syntax-case stx ()
      [(val-abso . args)
       #'(inner-val-abso . args)] 
      [val-abso
       #'(displayln "#<procedimiento:val-abso>")]
      [else
       (error "\nval-abso: sintaxis no válida")])))

(define inner-val-abso ;abs
  (lambda args
    (if (and (arity-check (count-args args) 1 "val-abso")(contract-viol-check args "número" "val-abso"))
        (abs (car args))
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

(provide val-abso)

(define-syntax cociente
  (lambda (stx)
    (syntax-case stx ()
      [(cociente . args)
       #'(inner-cociente . args)] 
      [cociente
       #'(displayln "#<procedimiento:cociente>")]
      [else
       (error "\ncociente: sintaxis no válida")])))

(define inner-cociente ;quotient
  (lambda args
    (if (and (arity-check (count-args args) 2 "cociente")(contract-viol-check args "número número" "cociente"))
        (quotient (car args) (cadr args))
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

(provide cociente)

(define-syntax residuo
  (lambda (stx)
    (syntax-case stx ()
      [(residuo . args)
       #'(inner-residuo . args)] 
      [residuo
       #'(displayln "#<procedimiento:residuo>")]
      [else
       (error "\nresiduo: sintaxis no válida")])))

(define inner-residuo ;remainder
  (lambda args
    (if (and (arity-check (count-args args) 2 "residuo")(contract-viol-check args "número número" "residuo"))
        (remainder (car args) (cadr args))
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

(provide residuo)

(define-syntax cociente/residuo
  (lambda (stx)
    (syntax-case stx ()
      [(cociente/residuo . args)
       #'(inner-cociente/residuo . args)] 
      [cociente/residuo
       #'(displayln "#<procedimiento:cociente/residuo>")]
      [else
       (error "\ncociente/residuo: sintaxis no válida")])))

(define inner-cociente/residuo ;quotient/remainder
  (lambda args
    (if (and (arity-check (count-args args) 2 "cociente/residuo")(contract-viol-check args "número número" "cociente/residuo"))
        (quotient/remainder (car args) (cadr args))
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

(provide cociente/residuo)

(define-syntax módulo
  (lambda (stx)
    (syntax-case stx ()
      [(módulo . args)
       #'(inner-módulo . args)] 
      [módulo
       #'(displayln "#<procedimiento:módulo>")]
      [else
       (error "\nmódulo: sintaxis no válida")])))

(define inner-módulo ;modulo
  (lambda args
    (if (and (arity-check (count-args args) 2 "módulo")(contract-viol-check args "número número" "módulo"))
        (modulo (car args) (cadr args))
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

(provide módulo)

(define-syntax redondear
  (lambda (stx)
    (syntax-case stx ()
      [(redondear . args)
       #'(inner-redondear . args)] 
      [redondear
       #'(displayln "#<procedimiento:redondear>")]
      [else
       (error "\nredondear: sintaxis no válida")])))

(define inner-redondear ;round
  (lambda args
    (if (and (arity-check (count-args args) 1 "redondear")(contract-viol-check args "número" "redondear"))
        (round (car args))
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

(provide redondear)

(define-syntax truncar
  (lambda (stx)
    (syntax-case stx ()
      [(truncar . args)
       #'(inner-truncar . args)] 
      [truncar
       #'(displayln "#<procedimiento:truncar>")]
      [else
       (error "\ntruncar: sintaxis no válida")])))

(define inner-truncar ;truncate
  (lambda args
    (if (and (arity-check (count-args args) 1 "truncar")(contract-viol-check args "número" "truncar"))
        (truncate (car args))
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

(provide truncar)

(define-syntax signo
  (lambda (stx)
    (syntax-case stx ()
      [(signo . args)
       #'(inner-signo . args)] 
      [signo
       #'(displayln "#<procedimiento:signo>")]
      [else
       (error "\nsigno: sintaxis no válida")])))

(define inner-signo ;sgn
  (lambda args
    (if (and (arity-check (count-args args) 1 "signo")(contract-viol-check args "número" "signo"))
        (sgn (car args))
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

(provide signo)