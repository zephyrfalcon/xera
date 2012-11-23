;; object.scm

(use srfi-69) ; hash tables 

(define-record object
  hidden     ;; hidden value; default #f
  protos     ;; list of protos
  slots      ;; hash table of slots
  id         ;; unique id
  )

;; look up NAME in the object's slots. return #f if not found.
(define (object-get-local obj name)
  (hash-table-ref/default (object-slots obj) name #f))

(define (look-up-in-protos protos name)
  (let loop ((protos protos))
    (if (null? protos)
        (values #f #f)
        (let ((value (object-get (car protos) name)))
          (if value
              (values value (car protos))
              (loop (cdr protos)))))))
               
;; look up NAME in the object's slots; if not found, check its protos.
;; return two values (value obj).
(define (object-get obj name)
  (let ((value (object-get-local obj name)))
    (if value
        (values value obj)
        (look-up-in-protos (object-protos obj) name))))

(define (object-set! obj name value)
  (hash-table-set! (object-slots obj) name value))

(define (object-update! obj name value)
  (receive (old-value xobj)
      (object-get obj name)
    (object-set! xobj name value)))

;; figure out what exactly this does first :-3
(define (object-delete! obj name)
  ...)

(define (object-slot-names obj)
  (hash-table-keys (object-slots obj)))

(define (new-object hidden . protos)
  (make-object hidden protos (make-hash-table) (new-id)))

;; IDs
(define *max-id* 0)
(define (new-id)
  (set! *max-id* (+ *max-id* 1))
  *max-id*)
