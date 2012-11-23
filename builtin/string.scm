;; builtin/string.scm

(define (make-string-proto . args) ...)

(define (new-string-from s ip)
  (let ((num-proto (lookup-proto 'String ip)))
    (new-object s num-proto)))

(define (x-string-repr obj args env ip)
  (new-string-from (sprintf "~s" (object-hidden obj)) ip))

(define (x-string-as-string obj args env ip) obj)

(define *string-methods*
  `((as-string ,x-string-as-string)
    (repr ,x-string-repr)))
