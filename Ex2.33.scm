#lang planet neil/sicp

;; Exercise 2.33:
;;
;; Fill in the missing expressions to complete the following definitions of some
;; basic list-manipulation operations as accumulations:
;;
;; (define (map p sequence)
;;   (accumulate (lambda (x y) ⟨??⟩) 
;;               nil sequence))
;;
;; (define (append seq1 seq2)
;;   (accumulate cons ⟨??⟩ ⟨??⟩))
;;
;; (define (length sequence)
;;   (accumulate ⟨??⟩ 0 sequence))
;;

;; Given:
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x acc)
                (cons (p x) acc)) 
              nil sequence))


(define (append seq1 seq2)
  (accumulate cons seq2 seq1))


(define (length sequence)
  (accumulate (lambda (x acc)
                (inc acc))
              0 sequence))

(check-expect (map inc '(1 2 3)) '(2 3 4))
(check-expect (append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(check-expect (length '(1 2 3)) 3)