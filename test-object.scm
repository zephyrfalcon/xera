;; test-object.scm

(use test)
(load "object")

;;test object without protos
(let ((obj (new-object #f)))
  (object-set! obj 'a 42)
  (object-set! obj 'b 100)
  (test 42 (object-get obj 'a))
  (receive (val x)
      (object-get obj 'b)
    (test val 100)
    (test obj x))
  )

;; test object with one proto
(let* ((parent (new-object #f))
       (obj (new-object #f parent)))
  (object-set! parent 'a 42)
  (object-set! parent 'b 100)
  (object-set! obj 'a 99)
  (object-set! obj 'c 12)
  (test 12 (object-get obj 'c))
  (test 100 (object-get obj 'b))
  (test 99 (object-get obj 'a))
  (test 42 (object-get parent 'a))
  (receive (val x)
      (look-up-in-protos (list parent) 'b)
    (test val 100)
    (test x parent))
  (receive (val x)
      (object-get obj 'b)
    (test val 100)
    (test x parent))
  )

;; test object with multiple protos
;; a --> b --> c
;;       d -->
(let* ((a (new-object #f))
       (b (new-object #f a))
       (c (new-object #f b))
       (d (new-object #f)))
  (object-protos-set! c (list b d))
  (object-set! a 'e 1)
  (object-set! b 'f 2)
  (object-set! c 'g 3)
  (object-set! d 'h 4)
  (test 3 (object-get c 'g))
  (test 4 (object-get c 'h))
  (test 2 (object-get c 'f))
  (test 1 (object-get c 'e))
  (object-set! b 'i 10)
  (object-set! d 'i 20)
  (test 10 (object-get c 'i))
  (object-set! a 'j 30)
  (object-set! d 'j 40)
  (test 30 (object-get c 'j))
  (object-update! c 'j 50)
  (test 50 (object-get c 'j))
  )