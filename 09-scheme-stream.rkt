#lang racket

(define (my-delay expr)
   (lambda () expr))

(define (my-force delayed-expr)
  (delayed-expr))

(define (my-stream-cons)
  (syntax-rules ()
   ((my-stream-cons x strm)
  (cons x (lambda () strm)))))
 
(define (my-stream-first strm)
  (car strm))
 
(define (my-stream-rest strm)
  ((cdr strm)))
 
(define (my-stream-takelist strm n)
  (if (= n 0)
      '()
      (cons (my-stream-first strm)(my-stream-takelist (my-stream-rest strm))(- n 1))))
 
(define (my-get-nats from)
  (my-stream-cons from (my-get-nats + from 1)))
 
(define (my-get-nats2 from)
  (my-stream-cons from (lambda () (my-get-nats2 + from 1))))

;1
(define (from n) (stream-cons n (from (+ n 1))))
(define nats (from 1))
 
;2
(define (stream-take s n)
  (if (<= n 0) '()
      (cons (stream-first s) (stream-take (stream-rest s) (- n 1)))))
;(stream-take nats 10)

;3
(define (stream-map f s)
  (stream-cons (f (stream-first s)) (stream-map f (stream-rest s))))
;(stream-take (stream-map (lambda (x) (* 2 x))  nats) 10)

;4
(define (stream-filter p s)
  (if (p (stream-first s))
      (stream-cons (stream-first s)(stream-filter p (stream-rest s)))
      (stream-filter p (stream-rest s))))
 
; (stream-take (stream-filter (lambda (x)(= 0 (remainder x 2)))  nats) 10)
 
;5
 
(define (is-prime? n)
  (define (helper x)
  (cond ((>= x n) #t)
        ((= 0 (remainder n x)) #f)
        (else (helper (+ 1 x)))))
(helper 2))
 
(define primes (stream-filter is-prime? nats))

;(stream-take primes 10)

;6
(define (sieve stream)
  (stream-cons
   (stream-first stream)
     (sieve
      (stream-filter
       (lambda (x)(not (= (remainder x (stream-first stream)) 0)))
    (stream-rest stream)))))

(define primes2
  (sieve (stream-map (lambda (x) (+ x 1)) nats)))
;(stream-take primes2 10)

;7
(define (iterate f x)
  (stream-cons x  (iterate f (f x))))
;(stream-take (iterate (lambda (x) (* x x)) 2) 5)

