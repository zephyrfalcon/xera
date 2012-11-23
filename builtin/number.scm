;; builtin/number.scm

(define (make-number-proto . args) ...)

(define (new-number-from n ip)
  (let ((num-proto (lookup-proto 'Number ip)))
    (new-object n num-proto)))

(define (x-number-plus obj args env ip)
  (assert (number? (object-hidden (car args))) "not a number")
  (let ((result (+ (object-hidden obj)
                   (object-hidden (car args)))))
    (new-number-from result ip)))
  
(define (x-number-minus obj args env ip) ...)

(define (x-number-repr obj args env ip)
  (new-string-from (sprintf "~s" (object-hidden obj)) ip))

(define *number-methods*
  `((minus ,x-number-minus)
    (plus  ,x-number-plus)
    (repr  ,x-number-repr)
    ))
