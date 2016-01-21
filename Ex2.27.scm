#lang planet neil/sicp

;; Exercise 2.27:
;;
;; Modify your reverse procedure of Exercise 2.18 to produce a deep-reverse procedure
;; that takes a list as argument and returns as its value the list with its elements
;; reversed and with all sublists deep-reversed as well. For example,
;;
;; (define x 
;;   (list (list 1 2) (list 3 4)))
;;
;; x
;; ((1 2) (3 4))
;;
;; (reverse x)
;; ((3 4) (1 2))
;;
;; (deep-reverse x)
;; ((4 3) (2 1))



;; reverse
(define (reverse l)
  (define (reverse-iter l res)
    (if (null? l)
        res
        (reverse-iter
         (cdr l)
         (cons (car l) res))))
  (reverse-iter l '()))

;; deep-reverse
(define (deep-reverse l)
  (define (reverse-iter l res)
    (cond ((null? l) res)
          ((not (pair? l)) l)
          (else
           (reverse-iter
            (cdr l)
            (cons (deep-reverse (car l)) res)))))
  (reverse-iter l '()))



;; Testing
(define x 
  (list (list 1 2) (list 3 4)))

(reverse x)
(deep-reverse x)

(check-expect (deep-reverse x) '((4 3) (2 1)))
