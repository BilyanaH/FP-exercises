#lang racket
;0
(define (is-list? x) (pair? x))

;1
(define (len l)
  (if (null? l) 0
     (+ 1 (len(cdr l)))))

;2
(define (repeat n x)
  (if (= n 0) '()
      (cons x (repeat(- n 1) x))))
;3
(define (my-member? l x)
(cond
  ((null? l) #f)
  ((equal? x (car l)) #t)
  (else (my-member? (cdr l) x))))

;4
(define (at n l)
  (cond
    ((null? 0) #f)
    ((= n 0) (car l))
    (else (at (- n 1) (cdr l)))))

;5
(define (push-back x l)
  ( if (null? l) (cons x '())
       (cons (car l) (push-back x (cdr l)))))

;6
(define (my-reverse l)
  (if (null? l)  '()
     (push-back (car l) (my-reverse (cdr l)))))


(define (my-reverse2 l)
  (define (helper l acc)
    (if (null? l)
        acc
        (helper (cdr l) (cons (car l) acc))))
  (helper l '()))

;7
(define (insert x n l)
  (cond ((null? l) (cons x '()))
        ((equal? n 0) (cons x l))
        (else (cons (car l) (insert x (- n 1) (cdr l))))))

;8
(define (all-prefixes l)
  (define (helper current-prefix rest result)
    (if (null? rest) (cons current-prefix result)
        (helper (cons (car rest) current-prefix)
                (cdr rest)
                (cons current-prefix result)
                )))
  (helper '() l '()))

;9
(define (my-map f l)
  (if (null? l) '()
  (cons (f (car l)) (my-map f (cdr l)))))

;10
(define (my-filter p l)
  (cond ((null? l) '())
        ((p (car l)) (cons (car l) (my-filter p (cdr l))))
        (else (my-filter p (cdr l)))))

;11
(define (foldr l op init)
  (if (null? l) init
      (op (car l) (foldr (cdr l) op init))))

;12
(define (sum l) (foldr l + 0))

;13
(define (forall? p l)
  (foldr l (lambda (x acc)(and (p x) acc)) #t))

;14
(define (exists? p l)
  (foldr l (lambda (x acc) (or (p x) acc)) #f))

;15
(define (my-member2? l x)
  (exists? (lambda (y)(equal? x y)) l))

;16
(define (unique l)
  (cond
    ((null? l) '())
    ((null? (cdr l)) l)
    ((equal? (car l) (car (cdr l))) (unique (cdr l)))
    (else (cons (car l) (unique (cdr l))))))

;17
;(define (is-bijection-over f items)


; 17. (is-bijection-over f items) - проверява дали f е биекция върху items
(define (is-bijection-over2 f items)
  (let ((mapped-items (my-map f items)))
    (and (= (len items) (len mapped-items))
         (forall? (lambda (x) (my-member? mapped-items x)) items)
         (forall? (lambda (y) (my-member? items y)) mapped-items))))



(define (subset? l1 l2) 
        (if (null? l1) #t
            (and (my-member? l2 (car l1)) (subset? (cdr l1) l2))))

(define (is-bijection-over f items)
  (let ((mapped-items (my-map f items)))
  (and (= (len items) (len mapped-items))
       (subset? items mapped-items)
       (subset? mapped-items items))))

(define (identity x) x)
;(is-bijection-over identity '(1 2 3 4)) ; Очакван резултат: #t
(define (double x) (* 2 x))
;(is-bijection-over double '(1 2 3 4)) ; Очакван резултат: #f, защото двойката на числа не покрива оригиналното множество.
(define (swap-1-and-2 x)
  (cond ((= x 1) 2)
        ((= x 2) 1)
        (else x)))

;(is-bijection-over swap-1-and-2 '(1 2 3 4)) ; Очакван резултат: #t

; 18. (fixpoints-of-some-func fs xs) - намира елементите на xs, които са неподвижни точки за някоя функция от fs
(define (fixpoints-of-some-func fs xs)
  (my-filter (lambda (x) (exists? (lambda (f) (equal? (f x) x)) fs)) xs))

(define fs1 (list identity double))
;(fixpoints-of-some-func fs1 '(1 2 3 4)) 
(define fs2 (list identity swap-1-and-2))
;(fixpoints-of-some-func fs2 '(1 2 3 4)) 
(define fs3 (list identity))
;(fixpoints-of-some-func fs3 '(1 2 3 4)) 