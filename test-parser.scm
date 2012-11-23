;; test-parser.scm

(use test)
(use trace)
(load "tokenizer")
(load "parser")

(define (id x) x) ;; helper function

(define (test-atom token atom)
  (receive (atom rest)
      (match-atom (list token))
    (test atom (id atom))
    (test rest (list))))

(test-begin "atoms")
(test-atom '(number "1") 1)
(test-atom '(string "\"hello\"") "hello")
(test-atom '(symbol ":foo") #:foo)
(test-atom '(identifier "bar") 'bar)
(test-end)

(test-begin "expressions")

(receive (expr rest)
    (match-expr '((number "3")))
  (test expr (id 3))
  (test rest (list)))

(receive (expr rest)
    (match-expr '((lbrace "{") (number "4") (dot ".") (rbrace "}")))
  (test expr '(block 4))
  (test rest '()))

;; todo: method call chain inside parens

(test-end)

(test-begin "match-mcp")

(receive (mcp rest)
    (match-mcp '((identifier "plus") (number "3") (comma ",")))
  (test mcp '(plus 3))
  (test rest '((comma ","))))
(test-end)

(test-begin "match-mct")

(receive (mct rest)
    (match-mct '((identifier "plus") (number "4") (comma ",")
                 (identifier "minus") (number "5") (dot ".")))
  (test mct '((plus 4) (minus 5)))
  (test rest '((dot "."))))

(test-end)

(test-begin "match-mcc")

(receive (mcc rest)
    (match-mcc '((number "100") (identifier "plus") (number "3") (dot ".")))
  ;; if we omit the dot (or another terminator), match-expr will be
  ;; called with an empty list. this syntax isn't valid anyway.
  (test mcc '(call 100 (plus 3)))
  (test rest '((dot "."))))

(receive (mcc rest)
    (match-mcc '((number "6")
                 (identifier "plus") (number "7") (comma ",")
                 (identifier "minus") (number "8") (dot ".")))
  (test mcc '(call 6 (plus 7) (minus 8)))
  (test rest '((dot "."))))

(test-end)

(test-begin "match-mcc-inside-parens")
(receive (mcc rest)
    (match-mcc-inside-parens '((lparen "(")
                               (number "12") (identifier "times") (number "2")
                               (rparen ")")))
  (test mcc '(call 12 (times 2)))
  (test rest '()))
(test-end)

(test-begin "match-statement")

(receive (stmt rest)
    (match-statement '((number "9") (dot ".") (number "10")))
  (test stmt (id 9))
  (test rest '((number "10"))))

(receive (stmt rest)
    (match-statement '((number "1") (identifier "plus") (number "2")
                       (dot ".") (number "3")))
  (test stmt '(call 1 (plus 2)))
  (test rest '((number "3"))))

(test-end)

(test-begin "match-block")

(receive (blk rest)
    (match-block '((lbrace "{") (number "4") (dot ".") (rbrace "}")))
  (test blk '(block 4))
  (test rest '()))

;; empty block
(receive (blk rest)
    (match-block '((lbrace "{") (rbrace "}")))
  (test blk '(block))
  (test rest '()))

(test-end)

(test-begin "high-level parsing")
(define (t&p s)
  (match-program (tokenize s)))

(test '(1) (t&p "1."))
(test '(1 2) (t&p "1. 2."))
(test '((call z (if-true (block)))) (t&p "z if-true {}."))
(test '((call 1 (plus 2) (println))) (t&p "1 plus 2, println."))
(test '((call 1 (plus 2 3 4) (minus 5))
        (call 6 (println)))
      (t&p "1 plus 2 3 4, minus 5. 6 println."))
(test '((call f (set (call 3 (plus 4))))) (t&p "f set (3 plus 4)."))

(test-end)
