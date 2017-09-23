(define (abs x)
  (cond ((< x 0) (- x))
    (else x)))


(define (double x) (* 2 x))

; EX 1.3
(define (sumOfThree x y z)
  (if (> x y)
    (if (> y z)
    (+ (* x x) (* y y))
    (+ (* x x) (* z z)))
    (if (> x z)
    (+ (* y y) (* x x))
    (+ (* y y) (* z z)))
  )
)

; Just for fun
(define (fac n)
  (if (= n 1)
  1
  (* n (fac (- n 1)))  
  )
)


(define (aplus a b)
  ((if (> b 0) + -) a b))

; EX 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
    0
    y))


; EX 1.6
(define (square x)
   (* x x))

(define (average x y)
   (/ (+ x y)
      2))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
      x)))

(define (improve guess x)
  (average guess 
           (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 
     0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; EX 1.7
(define (square x)
   (* x x))

(define (average x y)
   (/ (+ x y)
      2))

(define (sqrt-iter guess prev-guess x)
  (if (good-enough? guess prev-guess x)
    guess
    (sqrt-iter (improve guess x)
      guess x)))

(define (improve guess x)
  (average guess 
           (/ x guess)))

(define (good-enough? guess prev-guess x)
  (< (abs (/ (- prev-guess guess) guess)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 0 x))


; EX 1.8
; Newton's method for cube root approximations
; ((x / y^2) + 2y)/ 3
(define (cube-iter guess prev-guess x)
  (if (good-enough? guess prev-guess x)
    guess
    (cube-iter (cubeimprove guess x)
      guess x)))

(define (cubeimprove guess x)
  (/ (+ (/ x 
           (square guess)) 
        (* 2 guess)) 
     3))
  
(define (cbrt x)
  (cube-iter 1.0 0 x))


; EX 1.9
(define (+ a b)
  (if (= a 0)
    b
    (inc (+ (dec a) b))))
#
(+ 4 5) evaluates to:
(inc (+ (dec 4) 5))
(inc (+ 3 5))
(inc (inc (+ (dec 3) 5)))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ dec 2) 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ dec 1) 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5)
(inc (inc (inc 6)
(inc (inc 7))
(inc 8)
9
#
(define (+ a b)
  (if (= a 0)
    b
    (+ (dec a) (inc b))))
#
(+ 4 5) evaluates to:
(+ (dec 4) (inc 5))
(+ 3 6)
(+ (dec 3) (inc 6))
(+ 2 7)
(+ (dec 2) (inc 7))
(+ 1 8)
(+ (dec 1) (inc 8))
(+ 0 9)
9
#


; EX 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

# 
(A 1 10)
(A 0 (A 1 9))
(A 0 (A 0 (A 1 8)))
(A 0 (A 0 (A 0 (A 1 7))))
(A 0 (A 0 (A 0 (A 0 (A 1 6)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
(A 0 (A 0 (A 0 (A 0 (A 0 32)))))
(A 0 (A 0 (A 0 (A 0 64))))
(A 0 (A 0 (A 0 128)))
(A 0 (A 0 256))
(A 0 512)
1024
#


; 1.2.2 Tree Recursion

; Fib(n) with tree recursion
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                  (fib (- n 2))))))

; Fib(n) iteratively
(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (cond ((= count 0) b)
        (else (fib-iter (+ a b) a (- count 1)))))


; Counting Change
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                    (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))


; EX 1.11
(define (f n)
  (if (< n 3)
    n
    (+ (* 1 (f (- n 1)))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3)))
    )))

(define (f n)
  (if (< n 3)
    n
    (f-iter 2 1 0 n)))

(define (f-iter a b c count)
  (if (< count 3)
    a
    (f-iter (+ a (* 2 b) (* 3 c))
            a
            b
            (- count 1))))


; EX 1.12
(define (pascal row col)
  (cond ((< row col) 0)
        ((or (= col 0) (= row col)) 1)
        (else (+ (pascal (- row 1) col)
                 (pascal (- row 1) (- col 1))))))


; EX 1.13
; prove that Fib(n) is the closest integer to φn/√5, where:
;         φ = (1 + √5)/2
;         ψ = (1 - √5)/2

; First prove by induction that Fib(n) = (φn - ψn) / √5
; By showing that the left and right and sides are equal
; where n = 0, 1, 2:

; Left hand Side:
; Fib(0) = 0
; Fib(1) = 1
; Fib(2) = Fib(0) + Fib(1) = 1 + 0 = 1

; Right Hand Side:
;   for n = 0
;               (φ^n - ψ^n) / √5 = (φ^0 - ψ^0) / √5
;                              = (1 - 1) / √5
;                              = 0 / √5
;                              = 0
;
; for n = 1
;             (φ^n - ψ^n) / √5 = (φ^1 - ψ^1) / √5
;                              = (((1 + √5)/2) - (1 - √5)/2) / √5
;                              = (1/2 + √5/2) - (1/2 - √5/2) / √5
;                              = (1/2 + √5/2 - 1/2 + √5/2) / √5
;                              = (√5/2 + √5/2) / √5
;                              = (2 * √5/2) / √5
;                              = √5 / √5
;                              = 1
;
; for n = 2
;             (φ^n - ψ^n) / √5 = (φ^2 - ψ^2) / √5
;                              = ((φ + 1) - (ψ + 1)) / √5
;                              = (((1 + √5)/2 + 1) - (1 - √5)/2 + 1) / √5
;                              = ((1/2 + √5/2 + 1) - (1/2 - √5/2 + 1)) / √5
;                              = (1/2 + √5/2 + 1 - 1/2 + √5/2 - 1) / √5
;                              = (√5/2 + √5/2) / √5
;                              = (2 * √5/2) / √5
;                              = √5 / √5
;                              = 1


(define (phi)
  (/ (+ 1 (sqrt 5)) 2))

(define (psi)
  (/ (- 1 (sqrt 5)) 2))
  
(define (^ base exponent)
  (define (*^ exponent acc)
    (if (= exponent 0)
      acc
      (*^ (- exponent 1) (* acc base))))
  (*^ exponent 1))

(define (fib n)
  (/ (- (^ (phi) n) (^ (psi) n)) (sqrt 5)))


; EX 1.14
; Draw a tree illustrating count-change for 15cents

; drawing in notepad..
; Space: The space required grows linearly with the size of the input making it O(n).

; Time: 
; cc(n, 1) = O(n)
; For cc(n, 2) there will be n/5 cc(n, 2) calls. This is
; because we subtract 5 from n in each recursive call until
; the base case. 
; Each cc(n, 2) call will also produce a cc(n, 1) call.
; Therefore T(n, 2) = (n/5) * 2n + 1 = O(n^2)
;
; For cc(n, 3) there will be n/10 cc(n, 3) calls each with
; n/5 cc(n, 2) calls. Therefore O(n^3).

; Thus the final answer for cc(n, 5) is O(n^5)

; EX 1.15
(sine 12.15)
(p (sine (/ 12.15 3.0)))
(p (sine 4.05))
(p (p (sine (/ 4.05 / 3))))
(p (p (sine 1.35)))
(p (p (p (sine (/ 1.35 / 3)))))
(p (p (p (sine .45))))
(p (p (p (p (sine (/ .45 3))))))
(p (p (p (p (sine .15)))))
(p (p (p (p (p (sine (/ .15 3)))))))
(p (p (p (p (p (sine 0.005))))))

; Space: The amount of space increases linearly for each tripling of the input
; Time: We also must triple the input to add another step.
; Therefore it is O(log(n)) in both space and time.

