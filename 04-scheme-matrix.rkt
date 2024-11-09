#lang racket

;1
(define (at n l) ;ot predniq put
  (cond
    ((null? l) #f)
    ((= n 0) (car l))
    (else (at (- n 1) (cdr l)))))

(define m1 '((1 2 3)
            (4 5 6)
            (7 8 9)))

(define (mat-at m i j)
  (at j (at i m)))


;2
(define (map f l); ot lekcii
(if (null? l) '()
(cons (f (car l)) (map f (cdr l)))))

(define (mat-map f m)
  (map (lambda (row) (map f row)) m))

;3

(define (len l)
  (if (null? l) 0
     (+ 1 (len(cdr l)))))

(define (foldr l op init)
  (if (null? l) init
      (op (car l) (foldr (cdr l) op init))))

(define (forall? p l)
  (foldr l (lambda (x acc)(and (p x) acc)) #t))

(define (mat-forall? p m)
  (if (null? m) #t
   (and (forall? p (car m)) (mat-forall? p (cdr m)))))

(define (mat? m)
 (and 
   (list? m)
   (mat-forall? number? m)
   (forall? (lambda (x)(equal? (len (car m)) (len x) )) m)))


;4
(define (scalmul x m)
  (mat-map (lambda (el) (* el x) ) m))


 ;5
  (define (transpose m)
  (if (null? (car m))  
      '()              
      (cons (map car m)          
            (transpose (map cdr m)))))
 ;6

  (define m '((1 2 3)
             (4 5 6)))

 (define n '((7 8)
              (9 10)
              (11 12)))


(define (product row-m col-n)                
    (if (or (null? row-m) (null? col-n))         
        0
        (+ (* (car row-m) (car col-n)) (product (cdr row-m) (cdr col-n)))))
  
(define (matmul m n)
  (map
   (lambda (row-m)
     (map
      (lambda(col-n) (product row-m col-n))
      (transpose n)))
   m))

;7

(define (get-element mat i j)
  (if (or (< i 0) (>= i (length mat))
          (< j 0) (>= j (length (car mat))))
      0
      (mat-at mat i j)))


(define (extract-3x3 mat i j)
  (map (lambda (di)
         (map (lambda (dj) (get-element mat (+ i di) (+ j dj)))
              '(-1 0 1)))
       '(-1 0 1)))


(define (apply-kernel kernel mat)
  (let ((rows (length mat))
        (cols (length (car mat))))
    (map (lambda (i)
           (map (lambda (j)
                  (kernel (extract-3x3 mat i j)))
                (range cols)))
         (range rows))))


;проверка
(define (kernel submat)
  (foldl + 0 (flatten submat)))
