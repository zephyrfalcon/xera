;; test-tokenizer.scm

(use test)
(load "tokenizer")

(test-begin "tokenizer")

(test '(number "3") (get-next-token "3 4 5"))
(test '(identifier "plus") (get-next-token "plus 4 5"))
(test '(string "\"hello world\"") (get-next-token "\"hello world\" ja"))

(test '((number "3") (number "4") (number "5"))
      (tokenize "3 4 5"))
(test '((lparen "(") (symbol ":p") (rparen ")"))
      (tokenize "(:p)"))

(test-end)