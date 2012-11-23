;; xera.scm

(use extras trace)
(include "tokenizer")
(include "parser")
(include "ast")
(include "interpreter")
(include "object")
(include "builtin/builtins")
(include "tools")

;; use an object's repr to display its value
(define (print-result x ip)
  (let* ((repr (object-get x 'repr))
         (repr-f (object-hidden repr))
         (result (repr-f x '() (interpreter-toplevel ip) ip))) ;; a String
    (print (object-hidden result))
    ))

(define (mainloop)
  (define ip (new-interpreter))
  (setup-interpreter ip)

  (define (display-prompt)
    (printf "> ")
    (flush-output))

  (define (normalize-input s)
    (let ((s (string-trim-both s)))
      (if (not (string-suffix? "." s))
          (conc s ".")
          s)))
  
  (let loop ()
    (display-prompt)
    (let ((s (read-line)))
      (when (not (eq? s #!eof))
        (let ((toplevel (interpreter-toplevel ip)))
          (let ((results (eval-string (normalize-input s) toplevel ip)))
            (for-each (lambda (x) (print-result x ip))
                      results))
          (loop))))))

(define (main args)
  (let ((args (command-line-arguments)))
    (mainloop)))
