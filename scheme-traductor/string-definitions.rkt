#lang scheme

(require "helper-definitions.rkt")
(require "err-chks.rkt")

;;;;
;;;; String definitions
;;;;

(define-syntax ¿cadena?
  (lambda (stx)
    (syntax-case stx ()
      [(¿cadena? . args)
       #'(inner-¿cadena? . args)] 
      [¿cadena?
       #'(displayln "#<procedimiento:¿cadena?>")]
      [else
       (error "\n¿cadena?: sintaxis no válida")])))

(define inner-¿cadena? ;string?
  (lambda args
    (if (arity-check (count-args args) 1 "¿cadena?")
    (cond
      ((string? (car args)) 'cierto)
      ('falso) )
        (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación")) ))

  (provide ¿cadena?)

(define-syntax longitud-de-cadena
  (lambda (stx)
    (syntax-case stx ()
      [(longitud-de-cadena . args)
       #'(inner-longitud-de-cadena . args)] 
      [longitud-de-cadena
       #'(displayln "#<procedimiento:longitud-de-cadena>")]
      [else
       (error "\nlongitud-de-cadena: sintaxis no válida")])))

(define inner-longitud-de-cadena ;string-length
  (lambda args
    (if (and (arity-check (count-args args) 1 "longitud-de-cadena") (contract-viol-check args "cadena" "longitud-de-cadena")) 
        (string-length (car args))
          (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

 (provide longitud-de-cadena)

(define-syntax subcadena
  (lambda (stx)
    (syntax-case stx ()
      [(subcadena . args)
       #'(inner-subcadena . args)] 
      [subcadena
       #'(displayln "#<procedimiento:subcadena>")]
      [else
       (error "\nsubcadena: sintaxis no válida")])))

(define inner-subcadena ;substring
  (lambda args
    (if (and (arity-check (count-args args) "2 or 3" "subcadena") (contract-viol-check args "cadena número número" "subcadena"))
        (apply substring args)
        ;(if (= 2 (count-args args))
         ;   (substring (car args) (cadr args))
          ;  (substring (car args) (cadr args) (caddr args)))
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") )))

(provide subcadena)

(define-syntax concatenar-cadena
  (lambda (stx)
    (syntax-case stx ()
      [(concatenar-cadena . args)
       #'(inner-concatenar-cadena . args)] 
      [concatenar-cadena
       #'(displayln "#<procedimiento:concatenar-cadena>")]
      [else
       (error "\nconcatenar-cadena: sintaxis no válida")])))

(define inner-concatenar-cadena ;string-append
  (lambda args
    (if (contract-viol-check args "cadena cadena, ..." "concatenar-cadena")
        (concatenar-cadena-helper args)
         (error "error al comprobar si hay errores en la entrada\npor favor consulte la documentación") ) ))

(provide concatenar-cadena)



