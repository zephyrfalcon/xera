;; tokenizer.scm

(use regex)
(use srfi-13)
(use data-structures)

(define *token-regexen*
  (let ((s-identifier "[a-zA-Z_~@^*][a-zA-Z0-9_-]*")
        (r (lambda (s) (regexp (conc "^" s)))))
    `((number     ,(r "-?[0-9]+"))
      (identifier ,(r s-identifier))
      (symbol     ,(r (conc ":" s-identifier)))
      (string     ,(r "\\\"[^\\\"]*\\\""))
      (lbrace     ,(r "\\{"))
      (rbrace     ,(r "\\}"))
      (lparen     ,(r "\\("))
      (rparen     ,(r "\\)"))
      (comma      ,(r "\\,"))
      (equal      ,(r "="))
      (dot        ,(r "\\."))
      )))

;; simple API
(define (make-token name value) (list name value))
(define (token-name token) (car token))
(define (token-value token) (cadr token))

;; take a string (that doesn't start with whitespace) and return the
;; token that matches from the beginning of the string (if any,
;; otherwise raise an error).
(define (get-next-token s)
  (let loop ((regexen *token-regexen*))
    (if (null? regexen)
        (error "invalid token" s)
        (let* ((blah (car regexen))
               (token-name (car blah))
               (token-regex (cadr blah))
               (match (match-regex token-regex s)))
          (if match
              (list token-name match)
              (loop (cdr regexen)))))))

(define (match-regex regex data)
  (and-let* ((m (string-search regex data)))
    (car m)))

(define (tokenize data)
  (let loop ((data (string-trim data)) (tokens '())) 
    (if (= (string-length data) 0)
        (reverse tokens)
        (let* ((token (get-next-token data))
               (rest-str (substring data (string-length (token-value token)))))
          (loop (string-trim rest-str) (cons token tokens))))))
  
      