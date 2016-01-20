#lang planet neil/sicp

;; Exercise 2.1:
;; 
;; Consider the problem of representing line segments in a plane.
;; Each segment is represented as a pair of points: a starting point
;; and an ending point. Define a constructor make-segment and selectors
;; start-segment and end-segment that define the representation of
;; segments in terms of points. Furthermore, a point can be represented
;; as a pair of numbers: the xx coordinate and the yy coordinate.
;; Accordingly, specify a constructor make-point and selectors x-point
;; and y-point that define this representation. Finally, using your
;; selectors and constructors, define a procedure midpoint-segment that
;; takes a line segment as argument and returns its midpoint (the point
;; whose coordinates are the average of the coordinates of the endpoints).
;; To try your procedures, you’ll need a way to print points:
;;
;; (define (print-point p)
;;   (newline)
;;   (display "(")
;;   (display (x-point p))
;;   (display ",")
;;   (display (y-point p))
;;   (display ")"))



;; Point
;; Number, Number -> Point
(define (make-point x y) (cons x y))

;; Point -> Number
(define (x-point p) (car p))

;; Point -> Number
(define (y-point p) (cdr p))

;; Point -> Void
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))



;; Segment
;; Point, Point -> Segment
(define (make-segment s e) (cons s e))

;; Segment -> Point
(define (start-segment s) (car s))

;; Segment -> Point
(define (end-segment s) (cdr s))

;; Segment -> Point
(define (midpoint-segment s)
  (let ([p1 (start-segment s)]
        [p2 (end-segment s)])
    (make-point
     (/ (+ (x-point p1) (x-point p2)) 2)
     (/ (+ (y-point p1) (y-point p2)) 2))))



;; Testing
(check-expect (midpoint-segment
               (make-segment
                (make-point 1 3)
                (make-point 2 4)))
              (make-point (/ 3 2) (/ 7 2)))

(check-expect (midpoint-segment
               (make-segment
                (make-point -1 3)
                (make-point 2 -4)))
              (make-point (/ 1 2) (/ -1 2)))