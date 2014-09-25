#lang scheme
(define (square x)
  (* x x))

(define (abs x)
  ((if (> x 0) + -) 0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.00000001))

(define (sqrt x)
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.00000001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (average x y)
    (/ (+ x y ) 2))
  (sqrt-iter 1.000))


(define (cube x)
  (* x x x))
(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1 (* a (+ a 2))) 
         (pi-sum (+ a 4) b))))

(define (factorial x)
  (if (> x 1) (* x (factorial (- x 1))) 1))

(define (factorial-iter x)
  (define (fac-i a b)
    (if (> b 1) (fac-i (* a b) (- b 1)) a))
  (fac-i 1 x))
(define (inc x)
  (+ x 1))
(define (dec x)
  (- x 1))
;1.41
(define (double go)
  (lambda (x) (go (go x))))

;1.42
(define (compose fun1 fun2)
  (lambda (x) (fun1 (fun2 x))))