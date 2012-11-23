;; builtin/object.scm

(define (make-object-proto . args) ...)

(define (x-object-clone obj args env ip) ...)

(define (x-object-get obj args env ip)
  (let* ((id (keyword->symbol (object-hidden (car args))))
         (result (object-get obj id)))
    (or result
        (error "slot does not exist:" obj id))))

(define (x-object-set obj args env ip)
  (let ((id (keyword->symbol (object-hidden (car args)))))
    (object-set! obj id (cadr args))
    obj))

(define (x-object-id obj args env ip)
  (new-number-from (object-id obj) ip))

(define (x-object-as-string obj args env ip) ...)
(define (x-object-repr obj args env ip)
  (new-string-from (sprintf "Object_0x~x" (object-id obj)) ip))
(define x-object-as-string x-object-repr)

(define *object-methods*
  `((as-string ,x-object-as-string)
    (clone     ,x-object-clone)
    (get       ,x-object-get)
    (id        ,x-object-id)
    (repr      ,x-object-repr)
    (set       ,x-object-set)
    ))
