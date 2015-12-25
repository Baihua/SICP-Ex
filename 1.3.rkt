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

(define (new-enough? per-guess guess)
  (< (abs (- per-guess guess)) 0.001))

(define (other-sqrt-iter per-guess guess x)
  (if (new-enough? per-guess guess)
      guess
      (other-sqrt-iter guess (improve guess x) x)))

(define (improve3 guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
    3))

(define (cube x)
  (* x x x))

(define (good-enough3? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cube-root guess x)
  (if (good-enough3? guess x)
      guess
      (cube-root (improve3 guess x) x)))


;; 1.11  f(n) = f(n-1) + 2f(n-2) + 3f(n-3)
(define (f-1 n)
  (cond ((< n 3) n)
        (else (+ (f-1 (- n 1))
                 (* 2 (f-1 (- n 2)))
                 (* 3 (f-1 (- n 3)))))))

(define (f-2 n)
  (f-2-iter 2 1 0 n))

(define (f-2-iter a b c n)
  (if (= n 0)
      c
      (f-2-iter (+ a (* 2 b) (* 3 c))
                a
                b
                (- n 1))))


;; 1.12
(define (pask n m)
  (cond ((= m 1) 1)
        ((= m n) 1)
        (else (+ (pask (- n 1) m)
                 (pask (- n 1) (- m 1))))))

;;1.16
(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (fast-expt-iter 1 b n))

(define (fast-expt-iter result b n)
  (cond ((= n 0) result)
        ((even? n) (fast-expt-iter
                    result
                    (square b)
                    (/ n 2)))
        (else (fast-expt-iter (* result b) b (- n 1)))))


;;1.17
