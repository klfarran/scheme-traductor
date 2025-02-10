#lang scheme

;;;;
;;;; Helper Definitions
;;;;


(define count-args ;function to count number of arguments in a list 
  (lambda (args) ; a list, which consists of a variable number of parameters
   (if (null? args)
       0
       (+ 1 (count-args (cdr args) ))) ))

(provide count-args)

(define atom?
    (lambda (x)
      (and (not (pair? x)) (not (null? x)))))

  (provide atom?)

;helper function for más: adds together all elements in a list
;should only ever be passed a list, no need to check input
(define más-helper
  (lambda (lis)
    (cond
    ((null? lis) 0)
    ((eq? (count-args lis) 1) (car lis))
    (else (+ (car lis) (más-helper (cdr lis) )) ) )))

(provide más-helper)

;helper function for menos: subtracts all elements in a list (left to right)
;should only ever be passed a list, no need to check input
;should only ever be passed a list of size > 1 (the menos function has a case
  ;for when the list has a size of 0, 1)
(define menos-helper
  (lambda (lis)
    (cond
    ((null? lis) 0)
    ;((eq? (count-args lis) 1) (- 0 (car lis)))
    (else (- (car lis) (menos-helper (cdr lis) )) ) )))

(provide menos-helper)

;helper function for mult: multiplies all elements in a list together
;should only ever be passed a list, no need to check input
(define mult-helper
  (lambda (lis)
    (cond
    ((null? lis) 1)
    (else (* (car lis) (mult-helper (cdr lis) )) ) )))

(provide mult-helper)

;helper function for div: divides all elements in a list (left ro right)
;should only ever be passed a list, no need to check input
(define div-helper
  (lambda (lis)
    (cond
    ((null? lis) 1)
    (else (/ (car lis) (mult-helper (cdr lis) )) ) )))

(provide div-helper)

;helper function to throw errors for functions which should not accept lists anywhere in their input parameters
;takes a list 'lis' and returns true if any of its elements are lists, false otherwise
(define contains-list
  (lambda (lis)
    (cond
      ((null? lis) #f)
      ((list? (car lis)) #t)
      (else (contains-list (cdr lis)) ))))

(provide contains-list)

;helper function which returns true if there is a zero present in the list, and false otherwise
;used to check for divide by zero errors in the 'div' division function
;should only ever be passed a list of non-list elements, so we don't need to check for the case
  ;in which we are passed a list with nested list(s) inside
  (define contains-zero
    (lambda (lis)
      (cond
        ((null? lis) #f)
        ((= 0 (car lis)) #t)
        (else (contains-zero (cdr lis))) )))

(provide contains-zero)

  ;helper function which returns true if all the arguments in the list lis are lists, and false otherwise
  (define all-lists?
   (lambda (lis)
    (cond
      ((null? lis) #t)           
      ((not (list? (car lis))) #f)  
      (else (all-lists? (cdr lis))))) )

(provide all-lists?)

;helper function which returns true if all the arguments in the list lis are strings, and false otherwise
  (define all-strings?
   (lambda (lis)
    (cond
      ((null? lis) #t)           
      ((not (string? (car lis))) #f)  
      (else (all-strings? (cdr lis))))) )

(provide all-strings?)

  ;helper function which returns true if all the arguments in the list 'lis' are numeric values, and false otherwise
  (define all-numbers?
    (lambda (lis)
      (cond
        ((null? lis) #t)
        ((not (number? (car lis))) #f)  
        (else (all-numbers? (cdr lis))) )))

(provide all-numbers?)

  ;helper function which returns the list lis without the last element of lis
  (define all-but-last
    (lambda (lis)
      (cond
        ((or (null? (cdr lis)) (null? lis)) '())
        (else (cons (car lis) (all-but-last (cdr lis)) )) )))

(provide all-but-last)

  ;helper function which takes a list lis and appends together every element in the list
  ;should only be passed 'legal' parameters from the append function
  (define append-helper
    (lambda (lis)
      (cond
        ((null? lis) '())
        ((append (car lis)(append-helper (cdr lis)))) )))

(provide append-helper)

  ;helper function to return the last element of a list
  ;is never passed an empty list
  (define last-elem
    (lambda (lis)
      (cond
        ((null? (cdr lis)) (car lis))
        (else (last-elem (cdr lis)))) ))

(provide last-elem)

 ;helper function to return the item in the given list 'lis' that is not itself a list
 ;function will only be called when there is in fact a non-list item in 'lis'
  (define non-list-item
    (lambda (lis)
      (cond
      ((null? (car lis)) (error "expected a list, given null"))
      ((not (list? (car lis))) (car lis))
      (else (non-list-item (cdr lis))))))

(provide non-list-item)

;helper function to return the item in the given list 'lis' that is not a string
 ;function will only be called when there is in fact a non-string item in 'lis'
  (define non-string-item
    (lambda (lis)
      (cond
      ((null? (car lis)) (error "expected a list, given null"))
      ((not (string? (car lis))) (car lis))
      (else (non-string-item (cdr lis))))))

(provide non-string-item)

  ;helper function to return the item in the given list "lis" that is not a number (non list)
  ;function will only be called when there is in fact a non-numeric item in 'lis'
  (define non-numeric-item
    (lambda (lis)
      (cond
        ((null? (car lis)) (error "expected a list, given null"))
        ((not (number? (car lis))) (car lis))
        (else (non-numeric-item (cdr lis))) )))

(provide non-numeric-item)

;helper function for menos-de: determines if all elements in the list 'lis'
;are less than the first element
;should only ever be passed a list with at least one element, no need to check input
(define menos-de-helper
  (lambda (lis)
    (cond
      ((= (count-args lis) 1) 'cierto)
      ((< (car lis) (cadr lis)) (menos-de-helper (cdr lis)))
      (else 'falso) )))

(provide menos-de-helper)

;helper function for más-de: determines if all elements in the list 'lis'
;are greater than the first element
;should only ever be passed a list with at least one element, no need to check input
(define más-de-helper
  (lambda (lis)
    (cond
      ((= (count-args lis) 1) 'cierto)
      ((> (car lis) (cadr lis)) (más-de-helper (cdr lis)))
      (else 'falso) )))

(provide más-de-helper)

;helper function for máx: determines the maximum value out of the list 'lis'
;precondition- we know that this is a list 1 or more numerical values only
(define máx-helper
  (lambda (lis)
    (cond
      ((= 1 (count-args lis)) (car lis))
       (else (max (car lis) (máx-helper (cdr lis)))) )))

(provide máx-helper)

;helper function for mín: determines the minimum value out of the list 'lis'
;precondition- we know that this is a list 1 or more numerical values only
(define mín-helper
  (lambda (lis)
    (cond
      ((= 1 (count-args lis)) (car lis))
       (else (min (car lis) (máx-helper (cdr lis)))) )))

(provide mín-helper)

(define true?
  (lambda (x)
    (if (eq? x #t) #t #f)))

(provide true?)

;helper function which takes a list and returns a string which is all of the elments of the list
  (define list-to-string
    (lambda (lis)
      (cond
        ((null? lis) "")
        (else (string-append (format "~a " (car lis)) (list-to-string (cdr lis)))) )))

(provide list-to-string)

;helper function to concatenate together all of the strings in the list 'lis'
;precondition- we know that this list 'lis' only contains strings 
(define concatenar-cadena-helper
  (lambda (lis)
   (cond
     ((null? lis) "")
     (else (string-append (car lis) (concatenar-cadena-helper (cdr lis))))) ))

(provide concatenar-cadena-helper)

;helper function for o
;if we see a #t element, return 'cierto, otherwise, if no #t elements present, return 'falso
(define o-helper
  (lambda args
    (cond
      ((null? (car args)) 'falso)
      ((eq? (caar args) #t) ' cierto)
      ((eq? (car args) 'cierto) 'cierto)
      ((and (eq? (count-args args) 1) (false? (car args))) 'falso)
      ((and (eq? (count-args args) 1) (eq? (car args) 'falso)) 'falso)
      (else (o-helper (cdar args))))))

(provide o-helper)


;helper function to determine if the given args 'my-args' to the procedure 'proc-id' match the
  ;desired conteact that is required for that function
  ;issue: nowhere is the desired contract passed, the questions asked to determine if the
  ;args match the relevant contract for the function are 'hard coded', in the sense that i took
  ;them straight from where they used to be in each individual function
  ;this is because the contract names are obscure and difficult to decifer what each one really means 
 (define match-contract
    (lambda (my-args proc-id)
      (cond
        ((and (equal? (substring (get-contract proc-id) 0 1) "_") (null? (car my-args))) ", se espera un átomo o una lista")
        ((and (equal? (get-contract proc-id) "string") (not (string? (car my-args))) ) (string-append (format "~a" (car my-args)) ", se espera una cadena"))
        ((and (equal? (get-contract proc-id) "string, string, ...") (not (all-strings? my-args)) ) (string-append (format "~a" (non-string-item my-args)) ", se espera una cadena"))
        ((and (equal? (get-contract proc-id) "string, number, [number]") (not (string? (car my-args))) ) (string-append (format "~a" (car my-args)) ", se espera una cadena"))
        ((and (equal? (get-contract proc-id) "string, number, [number]") (not (number? (cadr my-args))) ) (string-append (format "~a" (cadr my-args)) ", se espera un número"))
        ((and (equal? (get-contract proc-id) "(1+)") (null? (car my-args))) "(), se espera una lista no vacía")
        ((and (equal? (get-contract proc-id) "(1+)") (not (pair? (car my-args)))) (string-append (format "~a" (car my-args)) ", se espera una lista"))
        ((and (equal? (get-contract proc-id) "(2+)") (not (pair? (car my-args)))) (string-append (format "~a" (car my-args)) ", se espera una lista"))
        ((and (equal? (get-contract proc-id) "(2+)") (> 2 (length (car my-args)))) (string-append (format "~a" (car my-args)) ", se espera una lista con al menos dos elementos"))
        ((and (equal? (get-contract proc-id) "(3+)") (not (pair? (car my-args)))) (string-append (format "~a" (car my-args)) ", se espera una lista"))
        ((and (equal? (get-contract proc-id) "(3+)") (> 3 (length (car my-args)))) (string-append (format "~a" (car my-args)) ", se espera una lista con al menos tres elementos"))
        ((and (equal? (get-contract proc-id) "(4+)") (not (pair? (car my-args)))) (string-append (format "~a" (car my-args)) ", se espera una lista"))
        ((and (equal? (get-contract proc-id) "(4+)") (> 4 (length (car my-args)))) (string-append (format "~a" (car my-args)) ", se espera una lista con al menos cuatro elementos"))
        ((and (equal? (get-contract proc-id) "(2+ of any)")  (not (pair? (car my-args)))) (string-append (format "~a" (car my-args)) ", se espera una lista"))
        ((and (equal? (get-contract proc-id) "(2+ of any)")  (> 2 (length (car my-args)))) (string-append (format "~a" (car my-args)) ", se espera una lista con al menos dos elementos"))
        ((and (equal? (get-contract proc-id) "((), 0+ of any)")  (not (pair? (car my-args)))) (string-append (format "~a" (car my-args)) ", se espera una lista"))
        ((and (equal? (get-contract proc-id) "number")(not (number? (car my-args)))) (string-append (format "~a" (car my-args)) ", se espera un número"))
        ((and (equal? (get-contract proc-id) "1+ numbers") (not (all-numbers? my-args))) (string-append (format "~a" (non-numeric-item my-args)) ", se espera un número" ))
        ((and (equal? (get-contract proc-id) "((), (), ...any)") (not (all-lists? (all-but-last my-args)))) (string-append (format "~a" (non-list-item (all-but-last my-args))) ", se espera una lista"))    
        ((and (equal? (get-contract proc-id) "list") (not (list? (car my-args))))  (string-append (format "~a" (car my-args)) ", se espera una lista")) 
        ((and (equal? (get-contract proc-id) "pair") (not (pair? (car my-args))))  (string-append (format "~a" (car my-args)) ", se espera un par")) 
        ((and (equal? (get-contract proc-id) "(())") (not (pair? (caar my-args)))) (string-append (format "~a" (caar my-args)) ", se espera una lista"))
        ((and (equal? (get-contract proc-id) "((()))") (not (pair? (caaar my-args)))) (string-append (format "~a" (caaar my-args)) ", se espera una lista"))
        ((and (equal? (get-contract proc-id) "(((())))") (not (pair? (caaaar my-args)))) (string-append (format "~a" (caaaar my-args)) ", se espera una lista"))
        ((equal? (get-contract proc-id) "string, number, [number]")
            (if (= 3 (length my-args))
                (if (not (number? (caddr my-args)))
                   (string-append (format "~a" (caddr my-args)) ", se espera un número")
                    #t)
                 #t))
            (else #t)))) ;passes all checks

(provide match-contract)


;returns a contract for the procedure bound to the identifier 'proc-id'
(define get-contract
  (lambda (proc-id)
    (cond
      ((equal? proc-id "pri") "(1+)")
      ((equal? proc-id "longitud") "list")
      ((equal? proc-id "prri") "(())")
      ((equal? proc-id "prrri") "((()))")
      ((equal? proc-id "prrrri") "(((())))")
      ((equal? proc-id "res") "(1+)")
      ((equal? proc-id "ress") "(2+)")
      ((equal? proc-id "resss") "(3+)")
      ((equal? proc-id "ressss") "(4+)")
      ((equal? proc-id "pres") "(2+ of any)")
      ((equal? proc-id "resp") "((), 0+ of any)")
      ((equal? proc-id "longitud-de-cadena") "string")
      ((equal? proc-id "subcadena") "string, number, [number]")
      ((equal? proc-id "concatenar") "((), (), ...any)")
      ((equal? proc-id "concatenar-cadena") "string, string, ...")
      ((or (equal? proc-id "más")(equal? proc-id "menos")(equal? proc-id "div")(equal? proc-id "mult")(equal? proc-id "menos-de")
           (equal? proc-id "más-de") (equal? proc-id "expo")(equal? proc-id "cociente")(equal? proc-id "residuo")
           (equal? proc-id "máx") (equal? proc-id "mín")(equal? proc-id "cociente/residuo")(equal? proc-id "módulo")
           (equal? proc-id "redondear")(equal? proc-id "truncar")(equal? proc-id "signo")) "1+ numbers")
      ((or (equal? proc-id "¿cero?")(equal? proc-id "¿positivo?")(equal? proc-id "¿negativo?")(equal? proc-id "¿número-par?")
           (equal? proc-id "¿número-impar?")(equal? proc-id "raíz-cuadrada") (equal? proc-id "val-abso"))"number")
      (else "n/a"))))

(provide get-contract)


  