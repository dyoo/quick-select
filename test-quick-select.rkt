#lang racket/base
(require "quick-select.rkt"
         rackunit)

(check-equal? (quick-select! (vector 3 1 4) 0 2 0) 1)
(check-equal? (quick-select! (vector 3 1 4) 0 2 1) 3)
(check-equal? (quick-select! (vector 3 1 4) 0 2 2) 4)
