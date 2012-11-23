;; ast.scm
;; Xera AST evaluator in Chicken Scheme.

(define (ast-atom? x)
  (or (symbol? x)
      (keyword? x)
      (number? x)
      (string? x)))

(define (ast-mcc? x)
  (and (list? x)
       (equal? (car x) 'call)))

(define (ast-block? x)
  (and (list? x)
       (equal? (car x) 'block)))

(define (eval-ast ast env ip)
  (cond ((ast-atom? ast) (eval-atom ast env ip))
        ((ast-mcc? ast) (eval-mcc ast env ip))
        ((ast-block? ast) (eval-block ast env ip))
        (else (error "not supported" ast))))

(define (eval-atom ast env ip)
  (cond ((number? ast) (new-number-from ast ip)) 
        ((string? ast) (new-string-from ast ip))
        ((keyword? ast) (new-symbol-from ast ip))
        ((symbol? ast) (object-get env ast))  ; look up
        (else (error "not supported" ast))))

(define (eval-mcc ast env ip)
  (let ((head (eval-ast (mcc-head ast) env ip)))
    (let loop ((result head) (tail (mcc-tail ast)))
      (if (null? tail)
          result
          (let ((id (mcp-id (car tail)))
                (args (mcp-args (car tail))))
            (let ((meth (object-get result id)))
              (if (object-callable? meth)
                  (let* ((evaled-args
                          (map (lambda (x) (eval-ast x env ip)) args))
                         (new-result (object-call meth result evaled-args env ip)))
                    (loop new-result (cdr tail)))
                  ;; just look up attr and return it
                  meth)))))))

(define (eval-block ast env ip)
  (new-block-from (make-xblock (block-stmts ast) env) ip))
;; note that this CREATES a block, rather than executing it

(define (eval-string s env ip)
  (let* ((tokens (tokenize s))
         (ast (match-program tokens)))
    (pretty-print ast)
    (map (lambda (stmt) (eval-ast stmt env ip))
         ast)))
;; NOTE: DON'T use repr'ing here; EVAL-STRING should be a general function
;; that returns a list of results. Let the interactive interpreter
;; handle how it is printed.

;; evaluate the contents of an xblock, given a target and (optional)
;; parameters of the form ((a 1) (b 2) ...).
(define (eval-block-contents xblk target params env ip)

  (define (add-params-to-env params env)
    (for-each (lambda (pair)
                (object-set! env (car pair) (cadr pair)))
              params))
  
  (let* ((new-env (new-object #f (xblock-env xblk))))
    (object-set! new-env '@ target)
    (object-set! new-env '^ env)
    (object-set! new-env '* new-env)
    (add-params-to-env params new-env)
    (let ((result (null ip))) 
      (for-each (lambda (stmt)
                  (set! result (eval-ast stmt new-env ip)))
                (xblock-statements xblk))
      result)))
