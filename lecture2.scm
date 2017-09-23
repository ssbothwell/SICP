(define (sum-int a b)
  (if (> a b)
      0
      (+ a
         (sum-int (1+ a) b))))

(sum-int 2 5)
(+ 2 (sum-int 3 5))
(+ 2 (+ 3 (sum-int 4 5)))
(+ 2 (+ 3 (+ 4 (sum-int 5 5))))
(+ 2 (+ 3 (+ 4 (+ 5 0))))


(define (sum-square a b)
  (if (> a b)
      0
      (+ (square a)
         (sum-square (1+ a) b))))


(define (sum-pi a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (sum-pi (+ a 4) b))))


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term
              (next a)
              next
              b))))


(define (sum-int a b)
  (define (identity a) a)
  (sum identity a 1+ b))


(define (sum-square a b)
  (sum square a 1+ b))


(define (pi-sum a b)
  (sum (lambda (i) (/ 1.0 (* i (+ i 2))))
       a
       (lambda (i) (+ i 4))
       b))


(define (sqrt x)
  (fixed-point
    (lambda (y) (average (/ x y ) y))
    1))

(define (fixed-point f start)
  (define (close-enough? u v)
    (< (abs (- u v)) 0.0001))
  (define (iter old new)
    (if (close-enough? old new)
        new
        (iter new (f new))))
  (iter start (f start)))

(define (average x y)
  (/ (+ x y) 2.0))


(define (sqrt x)
  (define average-damp 
    (lambda (f)
            (lambda (x) (average (f x) x))))
  (fixed-point
    (average-damp (lambda (y) (/ x y)))
    1))


(define (sqrt x)
  (define (newton f guess)
    (define deriv
      (lambda (f)
        (lambda (x)
          (/ (- (f (+ x 0.00001))
                (f x))
             0.00001))))
    (define df (deriv f))
    (fixed-point
      (lambda (x) (- x (/ (f x) (df x))))
      guess))
  (newton (lambda (y) (- x (square y)))
          1))

;; Adding rational numbers
; make-rat takes two integers and turns them into a
; coproduct representing numerator and denominator.
; numer and denom are fst and snd returning the numerator
; and denominator respectively.
;
; +rat and *rat use these procedures to find sum and
; product of two quotients. 
; 
; reducing the quotients lowest common denominators
; is shown using two different methods. Either in make-rat
; or in numer/denom



;; this version reduces the quotient when it is calculated

; int int -> (int . int)
(define (make-rat n d)
  (let ((g ( gcd n d)))
    (cons (/ n g)
          (/ d g))))

; (int . int) -> fst
(define (numer x) (car x))
; (int . int) -> snd
(define (denom x) (cdr x))


; This version will reduce the quotient when you call
; numer or denom on the final quotient to view the answer

; int int -> (int . int)
(define (make-rat n d) (cons n d))

; (int . int) -> fst
(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))

; (int . int) -> snd
(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))


; (int . int) (int . int) -> (int . int)
(define (+rat x y)
  (make-rat
    (+ (* (numer x) (denom y))
       (* (numer y) (denom x)))
    (* (denom x) (denom y))))

; (int . int) (int . int) -> (int . int)
(define (*rat x y)
  (make-rat
    (* (numer x) (numer y))
    (* (denom x) (denom y))))


;; Working with vectors in the plane
; int int -> ( int . int)
(define (make-vector x y) (cons x y))
(define (xcor p) (car p))
(define (ycor p) (cdr p))

; representing line segments
(define (make-seg p q) (cons p q))
(define (seg-start s) (car s))
(define (seg-end s) (cdr s))

; calculate midpoint of segment
(define (midpoint s)
  (let ((a (seg-start s))
       (b (seg-end s)))
     (make-vector
       (average (xcor a) (xcor b))
       (average (ycor a) (ycor b)))))

; calculate length of segment
(define (length s)
  (let
    ((dx (- (xcor (seg-end s))
            (xcor (seg-start s))))
     (dy (- (ycor (seg-end s))
            (ycor (seg-start s)))))
    (sqrt (+ (square dx)
             (square dy)))))


;; Implementation of cons, car, cdr
(define (cons a b)
  (lambda (pick)
    (cond ((= pick 1) a)
          ((= pick 2) b))))
(define (car x) (x 1))
(define (cdr x) (x 2))

