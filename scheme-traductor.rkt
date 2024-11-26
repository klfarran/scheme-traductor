#lang scheme

;; Require all groupings of definitions

(require scheme-traductor/keyword-definitions)
(require scheme-traductor/arithmetic-definitions)
(require scheme-traductor/string-definitions)
(require scheme-traductor/list-definitions)
(require scheme-traductor/conditional-definitions)
(require scheme-traductor/type-check-definitions)

    ;;provide all definitions from each group     

(provide (all-from-out scheme-traductor/keyword-definitions)
         (all-from-out scheme-traductor/arithmetic-definitions)
         (all-from-out scheme-traductor/string-definitions)
         (all-from-out scheme-traductor/list-definitions)
         (all-from-out scheme-traductor/conditional-definitions)
         (all-from-out scheme-traductor/type-check-definitions))


