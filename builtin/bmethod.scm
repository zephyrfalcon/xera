;; builtin/bmethod.scm

(define (make-bmethod-proto . args) ...)

(define (new-bmethod-from f ip) ...)

(define (x-bmethod-call obj args env ip)
  (let* ((f (object-hidden obj))) ; the actual Scheme function
    (f (car args) (cdr args) env ip)))

(define (x-bmethod-repr obj args env ip)
  (new-string-from (sprintf "BuiltinMethod_0x~x"(object-id obj)) ip))

(define *bmethod-methods*
  `((call ,x-bmethod-call)
    (repr ,x-bmethod-repr)
    ))
