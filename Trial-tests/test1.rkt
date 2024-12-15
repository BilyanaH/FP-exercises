#lang racket
(define (accumulate op nv a b term)
  (if (> a b) nv
      (op (term a) (accumulate op nv (+ a 1) b term ))))

;1a



(define (done? x)
  (define (helper a)
    (accumulate + 0 a (- x 1) (lambda (y)(if ( = (remainder x y) 0) y 0)) ))
(= (helper 1) (+ 2 x)))

;1b
;ne mi trqbva ama ya naprawih 
(define (all-done a b)
  (cond ((= a b) '())
        ((done? a) (cons a (all-done (+ a 1) b)))
         (else (all-done (+ a 1) b))))

(define (is-closer-to-done? element a b)
  (define (helper x y)
  (cond
    ((or (= x a) (= y b)) #f)
    ((and (done? x)(> element (/(+ x a)2)) (<  element (quotient(+ a x) 2))) #t)
    ((and (done? y)(< element (/(+ y b)2))  (>  element (quotient(+ a y) 2))) #t)
    (else (helper (- x 1) (+ y 1)))))
  (helper ( quotient(+ a b) 2) (quotient (+ a b) 2)))

(define (sum-almost-done a b)
  (define (helper element)
  (cond ((= element b) 0)
        ((is-closer-to-done? element a b) ( + element (helper (+ element 1))))
        (else (helper (+ element 1)))))
  (helper a))


;na Ivan
(define (sum-almost-done2 a b)
  (define (isCloserToDone? n)
    (define (helper x y)
      (if(or (= x a) (= y b)) #f
         (if(or (done? x) (done? y)) #t
            (helper (- x 1) (+ y 1)))))
    (helper n n)
    )
  (define (helper current)
    (if(> current b) 0
     (if (isCloserToDone? current)
         (+ current (helper (+ 1 current)))
         (helper (+ 1 current)))))
  (helper a))

;3


(define (length l)
  (if (null? l) 0 (+ 1 (length(cdr l)))))

(define (list-with-n-el currList n)
   (if (= 0 n) '()
   (cons
    (car currList) (list-with-n-el  (cdr currList) (- n 1)))))

(define (sub-lists-with-n-elements l n)
  (define (is-long-enough? l n)
    (cond ((= n 0)#t)
           ((null? l) #f)
          (else (is-long-enough? (cdr l) (- n 1)))))
   (if (not (is-long-enough? l n)) '()
   (cons (list-with-n-el l n) (sub-lists-with-n-elements (cdr l) n))))

(define (majors? l1 l2)
  (if (not(=(length l1)(length l2))) #f
      (if (null? l1) #t
      (and (<= (car l1) (car l2)) (majors? (cdr l1) (cdr l2)))) ))

(define (is-major? l)
  (define (try-all-majors? l1 l2)
    (if (null? l2) #f
        (or (majors? l1 (car l2))
             (try-all-majors? l1 (cdr l2)))
    ))
  (cond
    ((null? (cdr l)) #t)
    ((> (length(car l)) (length(cadr l))) #f)
    (else (and (try-all-majors? (car l) (sub-lists-with-n-elements (cadr l) (length (car l))))
                                (is-major? (cdr l))))))
;2

(define (func op n stack)
  (let ((a (car stack))
       (b (cadr stack)))
  (if (or (null? (cdr stack)) (= n 0) (not(number? a)) (not(number? b))) stack
      (func op (- n 1) (cons (op a b) (cddr stack))))))

(define (apply op stack)
 (cond ((null? stack) '())
       ((number? (car stack)) (cons (op(car stack))(apply op (cdr stack))))
        (else (cons (car stack)(apply op (cdr stack))))))


(define (run-machine l)
  (define (helper l stack)
  (cond
    ((null? l) stack)
    ((or (number? (car l)) (symbol? (car l)))  (helper (cdr l) (cons (car l) stack)))
    ((procedure? (car l)) (helper (cdr l) (apply (car l) stack)))
    ((pair? (car l)) (helper (cdr l) (func (caar l) (cdar l) stack)))
    (else (helper (cdr l) stack))))
  (helper l '()))

  