#lang scheme

(require "helper-definitions.rkt")
(require "err-chks.rkt")
(require racket/trace)

;;;;
;;;; Keyword Definitions
;;;;
  

(define-syntax definir
  (lambda (stx)
    (syntax-case stx ()
      ;CASE 1-  (define (id args) body ... +)
       [(_ (sym . params) expr)
       (if (not (identifier? #'sym)) ; ensure sym si a valid identifier
           (error (string-append "\n definir: el primer argumento debe ser un identificador\noccurido en: " (format "~a" (syntax->datum #'sym))))     
       #'(define sym
               (lambda args                 
                   ; add arity-check as the first line in any procedure defined using definir 
                   (arity-check (count-args args) (count-args (syntax->datum #'params)) (symbol->string 'sym)) 
                    ;'execute' procedure with given args 'args' as a lambda expression 
                   (apply (lambda params expr) args) )))]  
      ;CASE 2- (define id expr)
      [(_ sym expr)
       (if (not (identifier? #'sym)) ; ensure sym is a valid identifier
           (error (string-append "\n definir: el primer argumento debe ser un identificador\noccurido en: " (format "~a" (syntax->datum #'sym))))
       (if (identifier? #'expr) ; expr can't be an identifier- can't bind an identifier to an identifier
           (error (string-append "\n  definir: no se puede enlazar un identificador a un identificador\n el segundo argumento debe ser un valor\noccurido en: " (format "~a" (syntax->datum #'expr))))     
           ;CASE 2a- check if expr is a lambda expression
              (if (and (pair? (syntax->datum #'expr)) (eq? (car (syntax->datum #'expr)) 'lambda))
             #'(define sym
                 (lambda args                 
                   (let* ((lambda-body (syntax->datum #'expr))   ; get body of lambda expr by converting the syntax object expr to a datum
                          (params (car (cdr lambda-body))) )      ; get parameters (as datum now) from lambda expression (desired args)
                   ; add arity-check as the first line in any procedure defined using definir 
                   (arity-check (count-args args) (count-args params) (symbol->string 'sym)) 
                   ;actually bind the expr to sym
                     (define proc expr)
                     (apply proc args)  )))
             ;CASE 2b- expr is not lambda expression- bind normally
                   #'(define sym expr)))) ]
      ;CASE 3- given any number of parameters that is not what was expected
      [(_ sym) ;given 1 param only
       (error "\n definir: se esperaba dos argumentos: un símbolo y una expresión")]
      [(_ sym ...) ;given 1+ params (and 2 params was already 'caught' above)
       (error "\n definir: se esperaba dos argumentos: un símbolo y una expresión")]
      ;CASE 4- given any other (incorrect) form
      [_  
       (error "\n  definir: sintaxis no válida")])))


(provide definir)

(define-syntax sea ;let
  (lambda (stx)
    (syntax-case stx ()
      ;CASE 1- (let ([id val-expr] ...) body ...+)
      [(_ ((var val) ...) body ...)
         ; check that each `var` is a valid identifier
         (for-each
          (lambda (binding)
              (unless (identifier? (car (syntax->list binding))) ; get just 'var' out of the pair and make sure its a valid identifier
                (error (string-append "\n  sea: par de enlaces no válido\n el primer argumento debe ser un identificador\nen: " (format "~a" (syntax->datum binding)))))
              ;(unless (not (identifier? (cadr (syntax->list binding)))) ; get just 'val' out of the pair and make sure its not a valid identifier
               ; (error (string-append "\n  sea: par de enlaces no válido\n el segundo argumento debe ser un valor\nen: " (format "~a" (syntax->datum binding)))))
            ) 
          (syntax->list #'((var val) ...))) ; convert the list of bindings to syntax objects
         #'(let ((var val) ...) body ...)] ; expand to `let`
      ;CASE2 - (let proc-id ([id init-expr] ...) body ...+)
      [(_ proc-id ((var val) ...) body ...)
       (begin ;needed because we're doing the unless and the for-each
       (unless (identifier? #'proc-id) ;proc-id must be a valid identifier
           (error (string-append "\n  sea: identificador no válido\n se esperaba un identificador\nen: " (format "~a" (syntax->datum #'proc-id)))))
      (for-each
        (lambda (binding)
          (unless (identifier? (car (syntax->list binding))) ; get just 'var' out of the pair and make sure its a valid identifier
            (error (string-append "\n  sea: par de enlaces no válido\n el primer argumento debe ser un identificador\nen: " (format "~a" (syntax->datum binding)))))
          (unless (not (identifier? (cadr (syntax->list binding)))) ; get just 'val' out of the pair and make sure its not a valid identifier
                (error (string-append "\n  sea: par de enlaces no válido\n el segundo argumento debe ser un valor\nen: " (format "~a" (syntax->datum binding))))) )
        (syntax->list #'((var val) ...)))
       ;#'(let proc-id ((var val) ...) body ...))]
        #'(letrec ([proc-id (lambda (var ...) body ...)])
             (proc-id val ...)))]
      [_
       (error "\n  sea: sintaxis no válida\n  se esperaba una lista de enlaces y una o más expresiones de cuerpo\n un enlace válido debe tener la forma: [identificador valor]")])))

 (provide sea)

(define-syntax sea* ;let*
  (lambda (stx)
    (syntax-case stx ()
      ;CASE 1- (let* ([id val-expr] ...) body ...+)
      [(_ ((var val) ...) body ...)
         ; check that each `var` is a valid identifier
         (for-each
          (lambda (binding)
              (unless (identifier? (car (syntax->list binding))) ; get just 'var' out of the pair and make sure its a valid identifier
                (error (string-append "\n  sea*: par de enlaces no válido\n el primer argumento debe ser un identificador\nen: " (format "~a" (syntax->datum binding)))))
              (unless (not (identifier? (cadr (syntax->list binding)))) ; get just 'val' out of the pair and make sure its not a valid identifier
                (error (string-append "\n  sea*: par de enlaces no válido\n el segundo argumento debe ser un valor\nen: " (format "~a" (syntax->datum binding)))))  ) 
          (syntax->list #'((var val) ...))) ; convert the list of bindings to syntax objects
         #'(let* ((var val) ...) body ...)] ; expand to `let*`
      [_
       (error "\n  sea*: se esperaba una lista de enlaces y una o más expresiones de cuerpo\n un enlace válido debe tener la forma: [identificador valor]")])))

 (provide sea*)

(define-syntax ¡establezca! ;set!
  (lambda (stx)
    (syntax-case stx ()
     [(_ var val)
       (if (identifier? #'var) ;check that var is an identifier
           (if (identifier-binding #'var) ;check that var has already been bound to/defined as a value (we can't set! a var that hasn't been bound already)
               #`(set! var val) ;do the set! 
               (error (string-append "\n  ¡establezca!: identificador aún no está enlazado a un valor\n para ¡establezca! un identificador, el identificador ya debe estar definido\noccurido en: " (format "~a" (syntax->datum #'var))))) ;var hasn't been bound to any value yet
           (error (string-append "\n ¡establezca!: el primer argumento debe ser un identificador\noccurido en: " (format "~a" (syntax->datum #'var)))))]
      [_ 
       (error "\n ¡establezca!: se esperaba 2 argumentos: un identificador ya enlazado y un valor")] ))) 

(provide ¡establezca!)

(define-syntax cita ;quote
  (lambda (stx)
   (syntax-case stx ()
     [(_ arg) ;arg can be a symbol, expression, atom, list,...
      #' (quote arg)]
     [_
      (error "\n cita: se esperaba 1 argumento")]) ))

(provide cita)

(define-syntax rastrear ;trace
  (lambda (stx)
    (syntax-case stx ()
      [(_ id)
       #'(begin
       (trace id))]
      [_
       (error "\n rastrear: se esperaba un identificador cuyo valor es un procedimiento")] )))

(provide rastrear)

(define-syntax rastrear-definir ;trace-define
  (lambda (stx)
    (syntax-case stx ()
      [(_ sym expr)
       #`(begin   ;begin is used to group multiple expressions so they execute in sequence as a single unit (define and trace)
           (definir sym expr)  
           (trace sym))]
      [_
      (error "\n rastrear-definir: se esperaba dos argumentos: un símbolo y una expresión")])))    

(provide rastrear-definir)

(define-syntax condición
  (lambda (stx)
    (syntax-case stx (sino) ;need to specify sino (else) as a keyword to be recognized in condicion statement (otherwise its an unbound id)
      [(_ (condition result) ... (sino expr))
       #'(cond ((eq? condition 'cierto) result) ... (else expr))]
      [(_ (condition result) ...)
       #'(cond ((eq? condition 'cierto) result) ...)]
      [_ 
       (error 'condición "sintaxis no válida: se esperaba una lista de condiciones y resultados")])))

(provide condición)

(define-syntax requerir ; require- maybe not necessary however
  (syntax-rules ()
    [(_ args) (require args)] ))

(provide requerir)

;could potentially add into 'definir' a place where we write a macro for the name/ proc id that's being defined to handle
;sytnax errors with those user-defined functions? god that's a lot...
;(define-syntax pri
 ; (lambda (stx)
  ;  (syntax-case stx ()
   ;   [(pri) 
    ;   (error 'pri "sintaxis no válida: se esperaba una lista")]  ; Return an error for (pri)
     ; [(pri . args)
      ; (begin
       ;  (displayln (syntax->datum #'args))
;       (if (mac-arity-check (mac-count-args (syntax->datum #'args)) 1 "pri") ;cdr 'gets rid of' the extra quote that's placed in front of the args 
 ;          #'(caadar (syntax->datum #'args))  ; Expand to (car args) if arity is correct
  ;         (error 'pri "sintaxis no válida: número incorrecto de argumentos")))]
   ;   [pri 
    ;   #'(begin 
     ;       (display "#<procedimiento:pri>")
      ;      (newline))]
      ;[else
       ;(error 'pri "sintaxis no válida") ])))

