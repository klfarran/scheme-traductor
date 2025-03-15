#lang scheme

;; require all groupings of definitions

(require esquematico/keyword-definitions)
(require esquematico/arithmetic-definitions)
(require esquematico/string-definitions)
(require esquematico/list-definitions)
(require esquematico/conditional-definitions)
(require esquematico/type-check-definitions)

    ;;provide all definitions from each group     

(provide (all-from-out esquematico/keyword-definitions)
         (all-from-out esquematico/arithmetic-definitions)
         (all-from-out esquematico/string-definitions)
         (all-from-out esquematico/list-definitions)
         (all-from-out esquematico/conditional-definitions)
         (all-from-out esquematico/type-check-definitions))


