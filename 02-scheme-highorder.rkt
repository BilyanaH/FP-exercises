#lang racket
;1 wrusta funciq
(define (o f g)
  (lambda (x) (f ( g x))))

;2
(define (repeated n f x)
  (if (= n 0) x
      (repeated (- n 1) f (f x))))

;3
(define (repeat n f)
  (lambda (x) repeated n f x))

; accumulate
(define (accumulate-n op init f begin end)
  (if (> begin end)
      init
      (op (f begin)(accumulate-n op init f (+ begin 1) end))))

;1
(define (count p a b) (accumulate-n + 0 (lambda (i) (if (p i) 1 0)) a b))
;2
(define (exists? p a b) (accumulate-n (lambda (x y)(or x y) #f p x y)))
;3
(define (forall? p a b) (accumulate-n (lambda (x y)(and x y) #t p x y)))
;4
(define (repeated2 n f x)
  (accumulate-n (lambda (acc _) (f acc)) x (lambda (i) i) 1 n))


(define (repeat2 n f)
  (accumulate-n (lambda (acc _) (lambda (x) (acc (f x))))
                (lambda (x) x)
                (lambda (i) i)
                1 n))
(define (repeated2.2 n f x)
  (accumulate-n (lambda (a1 a2) (f a2)) x (lambda (i) i) 1 n))

(define (succ x) (+ x 1))
(define (repeat2.2 n f)
  (lambda (x) (repeated2.2 n f x)))