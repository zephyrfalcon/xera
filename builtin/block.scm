;; builtin/block.scm

(define-record xblock
  statements     ;; list of AST exprs
  env            ;; captured environment (a Xera object)
  )

(define (new-block-from xblk ip)
  (let ((blk-proto (lookup-proto 'Block ip)))
    (new-object xblk blk-proto)))

(define (x-block-env obj args env ip)
  (xblock-env (object-hidden obj)))

;; execute a Block as-is, with @ set to Null.
(define (x-block-execute obj args env ip)
  (eval-block-contents (object-hidden obj) (null ip) '() env ip))

(define *block-methods*
  `((env     ,x-block-env)
    (execute ,x-block-execute)
    ))



