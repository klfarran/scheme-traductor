#lang scheme

(require "helper-definitions.rkt")
(require "err-chks.rkt")

;;;;
;;;; String definitions
;;;;

(define ¿cadena? ;string?
  (lambda args
    (if (arity-check (count-args args) 1 "¿cadena?")
    (cond
      ((string? (car args)) 'cierto)
      ('falso) )
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿cadena?)

(define longitud-de-cadena ;string-length
  (lambda args
    (if (and (arity-check (count-args args) 1 "longitud-de-cadena") (contract-viol-check args "cadena" "longitud-de-cadena")) 
        (string-length (car args))
          (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

 (provide longitud-de-cadena)

(define subcadena ;substring
  (lambda args
    (if (and (arity-check (count-args args) 3 "subcadena") (contract-viol-check args "cadena número número" "subcadena"))
        (substring (car args) (cadr args) (caddr args))
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

(provide subcadena)

(define concatenar-cadena ;string-append
  (lambda args
    (if (and (arity-check (count-args args) -1 "concatenar-cadena") (contract-viol-check args "cadena cadena, ..." "concatenar-cadena"))
        (concatenar-cadena-helper args)
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") ) ))

(provide concatenar-cadena)



