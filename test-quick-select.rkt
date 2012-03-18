#lang racket/base
(require "quick-select.rkt"
         rackunit)

(check-equal? (quick-select! (vector 3 1 4) 0 2 0) 1)
(check-equal? (quick-select! (vector 3 1 4) 0 2 1) 3)
(check-equal? (quick-select! (vector 3 1 4) 0 2 2) 4)


(define (select l k)
  (define vec (list->vector l))
  (quick-select! vec 0 (sub1 (vector-length vec)) k))

(check-equal? (select '(3 1 4 1 5 9 2 6) 0) 1)
(check-equal? (select '(3 1 4 1 5 9 2 6) 1) 1)
(check-equal? (select '(3 1 4 1 5 9 2 6) 2) 2)
(check-equal? (select '(3 1 4 1 5 9 2 6) 3) 3)
(check-equal? (select '(3 1 4 1 5 9 2 6) 4) 4)
(check-equal? (select '(3 1 4 1 5 9 2 6) 5) 5)
(check-equal? (select '(3 1 4 1 5 9 2 6) 6) 6)
(check-equal? (select '(3 1 4 1 5 9 2 6) 7) 9)




