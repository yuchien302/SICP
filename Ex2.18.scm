#lang planet neil/sicp

;; Exercise 2.18:
;; Define a procedure reverse that takes a list as argument and
;; returns a list of the same elements in reverse order:
;;
;; (reverse (list 1 4 9 16 25))
;; (25 16 9 4 1)

(define (reverse l)
  (define (reverse-iter l res)
    (if (null? l)
        res
        (reverse-iter
         (cdr l)
         (cons (car l) res))))
  (reverse-iter l nil))



;; Testing
(check-expect (reverse (list 1 4 9 16 25)) (list 25 16 9 4 1))
(check-expect (reverse (list 4)) (list 4))