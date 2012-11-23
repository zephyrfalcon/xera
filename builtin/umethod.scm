;; builtin/umethod.scm

(define-record umethod
  args     ;; list of symbols
  block    ;; block to be executed
  )

;; UserMethod new [symbols] block
(define (x-umethod-new obj args env ip)
  (let* ((u-args (map keyword->symbol (map object-hidden (butlast args))))
         (u-block (object-hidden (last args))))
    (new-object (make-umethod u-args u-block) (lookup-proto 'UserMethod ip))))
  
(define (x-umethod-call obj args env ip)
  (let* ((target (car args))
         (args (cdr args))
         (symbols (umethod-args (object-hidden obj)))
         (arg-value-pairs (zip symbols args)))
    (eval-block-contents (umethod-block (object-hidden obj))
                         target arg-value-pairs env ip)))

(define *umethod-methods*
  `((call       ,x-umethod-call)
    (new        ,x-umethod-new)
    ))

