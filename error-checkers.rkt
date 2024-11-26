#lang scheme

(require "helper-definitions.rkt")
(require racket/trace)

;;;;
;;;; Error-checking Definitions
;;;;

  ;my-args is the number of arguments that were passed to the function
  ;desired-args is the number of desired arguments for the function
  ;functname is the name of the function that we are checking the arity of 
  (define arity-check
   (lambda (my-args desired-args functname)
      (if (= -1 desired-args)
        (if (< 0 my-args)
           #t
           (error 
            (string-append "\n  "
             functname 
             ": desajuste de aridad;\n el número esperado de argumentos no coincide con el número dado\n esperado: al menos 1\n dado: " 
             (number->string my-args)))) ;passing -1 means "the desired args are any number of args > 0
      (if (= my-args desired-args)
        #t
        (error 
          (string-append "\n  "
            functname 
            ": desajuste de aridad;\n el número esperado de argumentos no coincide con el número dado\n esperado: " 
            (number->string desired-args) 
            "\n dado: " 
            (number->string my-args)))))))

(provide arity-check)

  ;my-args are the actual arguements passed to the function (in a list format)
  ;desired-args is the desired contract of the function 
  ;functname is the name of the function where we are checking for a contract violation
  (define contract-viol-check
    (lambda (my-args desired-args functname)
      (if (eq? (match-contract my-args functname) #t) ;have to check explicitly is equals #t because everything that is not explicity #f is 'true' in scheme
          #t
          (error
           (string-append "\n  "
            functname
            ": violación de contrato\n esperado: " desired-args "\ndado: " (list-to-string my-args) "\nerror at: " (match-contract my-args functname)) ))))

(provide contract-viol-check)

  ;my-args are the actual arguements passed to the function (in a list format)
  ;functname is the name of the function where we are checking for a divide by zero error
  (define div-by-zero-check
    (lambda (my-args functname)
      (cond
        ((or (and (= 1 (count-args my-args)) (= 0 (car my-args)))  (contains-zero (cdr my-args)))(error (string-append "\n  " functname ": división por cero") ))) ))
    
  (provide div-by-zero-check)
