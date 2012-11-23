;; parser.scm

(use srfi-1)
(keyword-style #:prefix)

;; simple API

(define (make-mcc head tail) (cons* 'call head tail))
(define mcc-head cadr)
(define mcc-tail cddr)

(define (make-mcp id args) (cons id args))
(define mcp-id car)
(define mcp-args cdr)

(define (make-block stmts) (cons 'block stmts))
(define block-stmts cdr)

;; matchers

(define *atoms* '(number string identifier symbol))

(define (match-atom tokens)
  (let ((v (token-value (car tokens))))
    (let ((atom (case (token-name (car tokens))
                 ((number) (string->number v))
                 ((string) (substring v 1 (- (string-length v) 1)))
                 ((identifier) (string->symbol v))
                 ((symbol) (string->keyword (substring v 1)))
                 (else #f))))
      (values atom (cdr tokens)))))

(define (match-expr tokens)
  (receive (atom rest)
      (match-atom tokens)
    (if atom
        (values atom rest)
        (receive (block rest)
            (match-block tokens)
          (if block
              (values block rest)
              (receive (mccip rest)
                  (match-mcc-inside-parens tokens)
                (if mccip
                    (values mccip rest)
                    (values #f #f))))))))

;; match a token of the given type (e.g. dot, lparen, etc) which will
;; be thrown away (otherwise, use MATCH-ATOM).
(define (match-token tokens type)
  (if (eq? (token-name (car tokens)) type)
      (values (car tokens) (cdr tokens))
      (values #f #f)))

(define (match-mcc tokens)
  (receive (head rest)
      (match-expr tokens)
    (if head
        (receive (tail rest2)
            (match-mct rest)
          (if tail
              (values (make-mcc head tail) rest2)
              ;;(values (make-mcc head '()) rest)))
              (values head rest))) ;; just the head
        (values #f #f))))

(define (match-mcc-inside-parens tokens)
  (receive (lparen? rest)
      (match-token tokens 'lparen)
    (if lparen?
        (receive (mcc rest2)
            (match-mcc rest)
          (if mcc
              (receive (rparen? rest3)
                  (match-token rest2 'rparen)
                (if rparen?
                    (values mcc rest3)
                    (error "missing closing parenthesis" tokens)))
              (error "method call chain expected" tokens)))
        (values #f #f))))

(define (match-mct tokens)
  (receive (part rest)
      (match-mcp tokens)
    (if part
        (let loop ((rest rest) (parts (list part)))
          (receive (comma? rest2)
              (match-token rest 'comma)
            (if comma?
                (receive (part rest3)
                    (match-mcp rest2)
                  (if part
                      (loop rest3 (cons part parts))
                      (error "syntax error" rest2))) ;; comma but no part
                (values (reverse parts) rest))))
        (values #f #f))))

;; match a statement, i.e. a method call chain followed by a dot.
(define (match-statement tokens)
  (receive (mcc rest)
      (match-mcc tokens)
    (if mcc
        (receive (dot? rest2)
            (match-token rest 'dot)
          (if dot?
              (values mcc rest2)
              (error "unterminated statement" tokens)))
        (values #f #f))))

(define (match-mcp tokens)
  (receive (id rest1)
      (match-token tokens 'identifier)
    (if id
        (let loop ((rest2 rest1) (args '()))
          (receive (arg rest3)
              (match-expr rest2)
            (if arg
                (loop rest3 (cons arg args))
                (values (make-mcp (string->symbol (cadr id)) (reverse args))
                        rest2))))
        (values #f #f))))

(define (match-block tokens)
  (receive (lbrace? rest1)
      (match-token tokens 'lbrace)
    (if lbrace?
        (let loop ((rest2 rest1) (stmts '()))
          (receive (stmt rest3)
              (match-statement rest2)
            (if stmt
                (loop rest3 (cons stmt stmts))
                ;; done with statements; match "}"
                (receive (rbrace? rest4)
                    (match-token rest2 'rbrace)
                  (if rbrace?
                      (values (make-block (reverse stmts)) rest4)
                      (error "unterminated block" tokens))))))
        (values #f #f))))

;; return a list of statements
(define (match-program tokens)
  (let loop ((rest tokens) (stmts '()))
    (if (null? rest)
        (reverse stmts)
        (receive (stmt rest2)
            (match-statement rest)
          (if stmt
              (loop rest2 (cons stmt stmts))
              (error "syntax error" rest))))))
