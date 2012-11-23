;; builtin/null.scm

(define (null ip)
  (lookup-proto 'Null ip))

(define (x-null-clone obj args env ip) obj)
(define (x-null-repr obj args env ip)
  (new-string-from "Null" ip))

(define *null-methods*
  `((clone    ,x-null-clone)
    (repr     ,x-null-repr)
    ))


