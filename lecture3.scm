;; More On Vectors

(define make-vector cons)
(define xcor car)
(define ycor cdr)

; adding vectors
(define (+vect v1 v2)
  (make-vector
    (+ (xcor v1) (xcor v2))
    (+ (ycor v1) (ycor v2))))


; scaling a vector
(define (scale s v)
  (make-vector
    (* s (xcor v))
    (* s (ycor v))))


; line segment
(define make-segment cons)
(define seg-start car)
(define seg-end cdr)

; ex
(make-segment (make-vector 2 3)
              (make-vector 5 1))

; lists

(list 1 2 3 4)

(define 1-to-4 (list 1 2 3 4))

(car (1-to-4)

(car (cdr (cdr (1-to-4))))


;; mapping over a list

;(scale-list 10 1-to-4) -> (10, 20, 30, 40)
(define (scale-list l s)
  (if (null? l)
    ()
    (cons (* (car l) s)
          (scale-list s (cdr l)))))


; Generalized map procedure
(define (map p l)
  (if (null? l)
    ()
    (cons (p (car l))
          (map p (cdr l)))))


(define (scale-list s l)
  (map (lambda (item) (* item s))
       l))


(define (for-each proc list)
  (cond ((null? list) "done")
        (else (proc (car list))
              (for-each proc
                    (cdr list)))))
