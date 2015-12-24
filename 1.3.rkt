#lang racket


(define (maxadd x y z)
  (cond ((and (< x y) (< x z)) (+ y z))
        ((> y z) (+ x y))
        (else (+ x z))
  )
)


(define (add-abs a b)
  ((if (> b a) + - ) a b))

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))


;;1.6
(define (square x)
  (* x x))

(define (abs x)
  (if (> x 0)
      x
      (- x)))

(define (average x y)
  (/ (+ x y) 2))


(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))


(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (new-sqrt-iter (improve guess x)
                         x)))