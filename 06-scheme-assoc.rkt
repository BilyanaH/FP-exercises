#lang racket
;1
(define assoc-empty '())

;2
(define (assoc-set l k v)
  (if (null? l) (list(cons k v))
      (if (equal? (caar l) k)
          (cons (cons k v) (cdr l))
          (cons (car l) (assoc-set (cdr l) k v)))))

;(assoc-set '((foo . 1) (bar . 2)) 'qux 3)
; Резултат: ((foo . 1) (bar . 2) (qux . 3))

;(assoc-set '((foo . 1) (bar . 2)) 'foo 3)
; Резултат: ((foo . 3) (bar . 2))


;3
(define (assoc-get l k)
   (if (null? l) #f
       (if (equal? (caar l) k) (cdar l)
           (assoc-get (cdr l) k))))

(define (assoc-get2 l k)
  (cdar (filter (lambda (p) (equal? (car p) k)) l)))

;(assoc-get '((foo . 1) (bar . 2)) 'foo)
; Резултат: 1

;(assoc-get '((foo . 1) (bar . 2)) 'qux)
; Резултат: #f

;4
(define (assoc-map f l)
  (if (null? l) '()
      (cons (cons (caar l ) (f(cdar l)))
            (assoc-map f (cdr l)))))

;(assoc-map (lambda (x) (* x 2)) '((foo . 1) (bar . 2) (ada . 3) (hey . 4)))

(define (assoc-map2 f l)
  (map (lambda (p) (cons (car p) (f (cdr p)))) l))

;5
(define (assoc-filter p l)
  (cond ((assoc? l) '())
    ((p (cdar l)) (cons (car l) (assoc-filter p (cdr l))))
    (else (assoc-filter p (cdr l)))))

;(assoc-filter (lambda (x) (> x 1)) '((foo . 1) (bar . 2)(ada . -3) (hey . 4)))

(define (assoc-filter2 p l)
  (filter (lambda  (x) (p (cdr x))) l))

;6
(define (forall? p l)
  (if(null? l) #t
     (and  (p (car l)) (forall? p (cdr l)))))

(define (exists? p l)
  (if (null? l)
      #f
      (or (p (car l)) (exists? p (cdr l)))))

(define (keys l) (map car l))

(define (member? x l)
  (exists? (lambda (y) (equal? x y) )l))

(define (unique? l)
  (if (null? l) #t
      (and (not (member? (car l) (cdr l)))
           (unique? (cdr l)))))

(define (assoc? l)
  (and (list? l)
       (unique? (keys l))
       (forall? pair? l)))
  

;(assoc? '((foo . 1) (bar . 2)))
; Резултат: #t

;(assoc? '((foo . 1) (foo . 2)))
; Резултат: #f (повтарящи се ключове)

;7
(define (assoc-merge l1 l2)
  (define (foreach l target)
    (if(null? l) target
       (assoc-set (foreach (cdr l) target) (caar l) (cdar l))))
  (foreach l2 l1)
  )
  
;8

;3
(define (assoc-get3 l k)
  (cond
    ((null? l) #f)
    ((eq? (car (car l)) k) (car l))
    (else (assoc-get3 (cdr l) k))))

(define (assoc-merge-resolve f l1 l2)
  (define (foreach l target)
    (if (null? l)
        target
          (assoc-set (foreach (cdr l) target) (car (car l))
           (if(assoc-get3 target (car (car l)))
               (f (car (car l)) (cdr (assoc-get3 target (car (car l)))) (cdr (car l)))
               (cdr (car l))))))
  
  (foreach l2 l1))

(define (assoc-merge2 l1 l2)
  (assoc-merge-resolve (lambda (k v1 v2) v2) l1 l2))

(define (assoc-merge-sum l1 l2)
  (assoc-merge-resolve (lambda (k v1 v2)(+ v1 v2)) l1 l2))

;(assoc-merge '((foo . 1) (bar . 2)) '((bar . 3) (qux . 4)))
; Резултат: ((foo . 1) (bar . 3) (qux . 4))

;(assoc-merge-resolve (lambda (k v1 v2) (+ v1 v2)) '((foo . 1) (bar . 2)) '((bar . 3) (qux . 4)))
; Резултат: ((foo . 1) (bar . 5) (qux . 4))

;(assoc-merge2 '((foo . 1) (bar . 2)) '((bar . 3) (qux . 4)))
; Резултат: ((foo . 1) (bar . 3) (qux . 4))

;(assoc-merge-sum '((foo . 1) (bar . 2)) '((bar . 3) (qux . 4)))
; Резултат: ((foo . 1) (bar . 5) (qux . 4))
