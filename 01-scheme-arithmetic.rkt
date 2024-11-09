#lang racket

;2
(define (not x) (if x #f #t))
(define (and x y) (if x y #f))
(define (or x y) (if x #t y))
(define (xor x y) (if (or (and  x (not y)) (and y (not x)) )#t #f))

;3
(define (fact n)
(if (= n 0) 1 (* n(fact(- n 1)))))

;4
(define (fib n)
  (if(< n 2) n (+(fib (- n 1))(fib (- n 2)))))

;5
;(define (count-digits n)
;(if (< n 10) 1 (+ 1 (count-digits (/ n 10) )) ))
(define (count-digits n)
(cond
[(and (< n 10)(> n 0)) 1]
[(>= n 10) (+ (count-digits (/ n 10)) 1)]
[(< n 0) (count-digits (abs n))]
))

;6
(define (reverse-number base n)
  (define (helper n rev)
    (if (= n 0) rev
        (helper (quotient n base) (+ (* rev base) (remainder n base)))))
  (helper n 0))

(define (palindrome?  base n)
  (= n (reverse-number base n)))
;7
(define (fact1 n)
(define (iter n)
(if (= n 0) 1 (* n(iter(- n 1)))))
  (iter n))

;8
(define (fib1 n)
  (define (iter a b count)
    (if (= count 0)  a
        (iter b (+ a b)(- count 1))))
  (iter 0 1 n))

;9
(define (succ n)
  (+ n 1))

;10
(define (pred n)
  (define (iter a b)
  (if (= (succ a) b) a
      (iter (succ a) b)))
  (iter 0 n))

;11
 (define (add a b)
  (if (= b 0)
   a
   (add (succ a) (pred b))))

;12
(define (multiply a b)
  (define (iter x y sum)
  (if (= y 0) sum 
  (iter x (pred y) (add sum x))))
     (iter a b 0)  
  )
;13
(define (fact2 n)
(define (iter n)
(if (= n 0) 1 (multiply n(iter(pred n)))))
  (iter n))

;14
(define (safe-div n)
  (define (helper x target)
    (cond ((= (add x x )target) x)
          ((> (add x x )target) (pred x))
           (else (helper (succ x) target))
     ))    
  (helper 0 n))

;15
(define (fib2 n)
  (define (iter a b count)
    (if (= count 0)  a
        (iter b (add a b)(pred count))))
  (iter 0 1 n))

;16
(define (ack n a b)
  (cond ((= n 0) (pred  a b))
        ((= n 1) (add a b))
        ((= n 2) (multiply a b))
        ((= n 3) (fact2 a))
        ((= n 4) (safe-div a)) 
        ((= n 5) (fib2 a)) 
        (else (error "Not implemented for n > 5"))))