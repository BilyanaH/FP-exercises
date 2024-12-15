#lang racket
(define (list-of-lists list)
  (and (list? list)
       (or
        (null? lst)
        (and (list? (car list)) (list-of-lists (cdr lst)))) 
      ))
(define (graph? g)
  (and (list? g) (list-of-lists? (cdr list))))