;; interpreter.scm

(define-record interpreter
  toplevel      ;; toplevel namespace (an object)
  builtins      ;; namespace for builtins
  )

(define (new-interpreter)
  (let* ((builtins (new-object #f))
         (toplevel (new-object #f builtins)))
    (make-interpreter toplevel builtins)))

(define (setup-interpreter ip)
  (print "Setting up interpreter...")
  (setup-objects ip)
  (setup-methods ip))

