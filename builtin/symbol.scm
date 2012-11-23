;; builtin/symbol.scm

;; TODO: symbol cache

(define (new-symbol-from sym ip)
  (let ((symbol-proto (lookup-proto 'Symbol ip)))
    (new-object sym symbol-proto)))

(define (x-symbol-repr obj args env ip)
  (let ((kw (object-hidden obj)))
    (new-string-from (sprintf "~s" kw) ip)))

(define *symbol-methods*
  `((repr ,x-symbol-repr)))
