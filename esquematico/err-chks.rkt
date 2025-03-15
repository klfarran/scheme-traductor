#lang scheme

(require "helper-definitions.rkt")

;;;;
;;;; Error-checking Definitions
;;;;

  ;my-args is the number of arguments that were passed to the function
  ;desired-args is the number of desired arguments for the function
  ;proc-id is the identifier bound to the procedure that we are checking the arity of 
  (define arity-check
   (lambda (my-args desired-args proc-id)
     (if (eq? desired-args "2 or 3")
         (if (or (= my-args 2) (= my-args 3))
             #t
             (error "error"))
      (if (= -1 desired-args)
        (if (< 0 my-args)
           #t
           (error 
            (string-append "\n  "
             proc-id 
             ": desajuste de aridad;\n el número esperado de argumentos no coincide con el número dado\n esperado: al menos 1\n dado: " 
             (number->string my-args)))) ;passing -1 means "the desired args are any number of args > 0
      (if (= my-args desired-args)
        #t
        (error 
          (string-append "\n  "
            proc-id 
            ": desajuste de aridad;\n el número esperado de argumentos no coincide con el número dado\n esperado: " 
            (number->string desired-args) 
            "\n dado: " 
            (number->string my-args)))))  )))

(provide arity-check)

  ;my-args are the actual arguements passed to the function (in a list format)
  ;desired-args is the desired contract of the function 
  ;proc-id is the identifier bound to the procedure of which we are checking for a contract violation
  (define contract-viol-check
    (lambda (my-args desired-args proc-id)
      (if (eq? (match-contract my-args proc-id) #t) ;have to check explicitly is equals #t because everything that is not explicity #f is 'true' in scheme
          #t
          (error
           (string-append "\n  "
            proc-id
            ": violación de contrato\n esperado: " desired-args "\ndado: " (list-to-string my-args) "\nerror en: " (match-contract my-args proc-id)) ))))

(provide contract-viol-check)

  ;my-args are the actual arguements passed to the function (in a list format)
  ;proc-id is the identifier bound to the procedure of which we are checking for a divide by zero error
  (define div-by-zero-check
    (lambda (my-args proc-id)
      (cond
        ((or (and (= 1 (count-args my-args)) (= 0 (car my-args)))  (contains-zero (cdr my-args)))(error (string-append "\n  " proc-id ": división por cero") ))) ))
    
  (provide div-by-zero-check)
