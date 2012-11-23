;; tools.scm

(define (keyword->symbol k)
  (string->symbol (keyword->string k)))
