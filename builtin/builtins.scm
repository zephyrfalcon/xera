;; builtin/builtins.scm

(include "builtin/object.scm")
(include "builtin/number.scm")
(include "builtin/bmethod.scm")
(include "builtin/string.scm")
(include "builtin/symbol.scm")
(include "builtin/block.scm")
(include "builtin/null.scm")
(include "builtin/umethod.scm")

(define *methods*
  `((Object        ,*object-methods*)
    (Number        ,*number-methods*)
    (BuiltinMethod ,*bmethod-methods*)
    (String        ,*string-methods*)
    (Symbol        ,*symbol-methods*)
    (Block         ,*block-methods*)
    (Null          ,*null-methods*)
    (UserMethod    ,*umethod-methods*)
    ))

(define (setup-objects ip)
  (print "Creating empty protos...")
  (let ((builtins (interpreter-builtins ip))
        (toplevel (interpreter-toplevel ip))
        (object-proto (new-object #f)))
    (object-set! builtins 'Object object-proto)
    (object-protos-set! builtins (list object-proto)) ; !
    (object-set! builtins 'Number (new-object 0 object-proto))
    (object-set! builtins 'BuiltinMethod (new-object #f object-proto))
    (object-set! builtins 'String (new-object "" object-proto))
    (object-set! builtins 'Symbol (new-object #:x object-proto))
    (object-set! builtins 'Block (new-object (make-xblock '() builtins)
                                             object-proto))
    (object-set! builtins 'Null (new-object #f object-proto))
    (object-set! builtins 'UserMethod (new-object #f object-proto))
    ;; use make-number-proto etc here instead?
    ;; or, use multiple-step mechanism; create "empty" protos first,
    ;; then fill in methods?
    ;; aliases
    (object-set! builtins 'Method (object-get builtins 'UserMethod))
    ;; namespaces
    (object-set! toplevel '^ toplevel)
    (object-set! toplevel '* toplevel)
    ))

(define (setup-methods ip)
  (for-each
   (lambda (method-info)
     (let ((id (car method-info))
           (method-list (cadr method-info)))
       (printf "Setting up methods for ~s...~n" id)
       (setup-methods-for ip id method-list)))
   *methods*))

(define (setup-methods-for ip id methods)
  (let* ((builtins (interpreter-builtins ip))
         (obj-proto (object-get builtins id))
         (bmethod-proto (object-get builtins 'BuiltinMethod)))
    (for-each
     (lambda (method)
       (let* ((method-name (car method))
              (method-f (cadr method))
              (bmeth (new-object method-f bmethod-proto)))
         (object-set! obj-proto method-name bmeth)))
     methods)))

(define (lookup-proto name ip)
  (let ((builtins (interpreter-builtins ip)))
    (object-get builtins name)))

;; calling objects

(define (object-callable? obj)
  (or (procedure? (object-hidden obj))
      (object-get obj 'call)))

(define (object-call callable target args env ip)
  (let ((f (object-hidden callable)))
    (if (procedure? f)
        (f target args env ip)
        (let ((call-meth (object-get callable 'call)))
          (if call-meth
              (let ((f (object-hidden call-meth)))
                (f callable (cons target args) env ip))
              (error "object is uncallable:" callable))))))
;; XXX maybe always use call? builtin methods can have this too...
