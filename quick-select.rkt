#lang racket/base

(require (for-syntax racket/base))

(provide quick-select!)



;; This is an implementation of the quick-select algorithm.

#|
 function select(list, left, right, k)
     if left = right // If the list contains only one element
         return list[left]  // Return that element
     select pivotIndex between left and right
     pivotNewIndex := partition(list, left, right, pivotIndex)
     pivotDist := pivotNewIndex - left + 1 
     // The pivot is in its final sorted position, 
     // so pivotDist reflects its 1-based position if list were sorted
     if pivotDist = k 
         return list[pivotNewIndex]
     else if k < pivotDist 
         return select(list, left, pivotNewIndex - 1, k)
     else
         return select(list, pivotNewIndex + 1, right, k - pivotDist)
|#



;; Invariants: left and right are valid indicies into the vector.
;; k is a natural.
(define (quick-select! vec left right k)
  (cond [(= left right)
	 (vector-ref vec left)]
	[else
	 (define pivot-index (random-int-in left right))
	 (define pivot-new-index (partition! vec left right pivot-index))
	 (define pivot-dist (- pivot-new-index left))
	 (cond
	  [(= pivot-dist k)
	   (vector-ref vec pivot-new-index)]
	  [(< k pivot-dist)
	   (quick-select! vec left (sub1 pivot-new-index) k)]
	  [else
	   (quick-select! vec 
			  (add1 pivot-new-index)
			  right 
			  (sub1 (- k pivot-dist)))])]))



(define (random-int-in left right)
  (+ left (random (- right left))))


#|
 function partition(list, left, right, pivotIndex)
     pivotValue := list[pivotIndex]
     swap list[pivotIndex] and list[right]  // Move pivot to end
     storeIndex := left
     for i from left to right-1 
         if list[i] < pivotValue
             swap list[storeIndex] and list[i]
             increment storeIndex
     swap list[right] and list[storeIndex]  // Move pivot to its final place
     return storeIndex
|#
(define (partition! vec left right pivot-index)
  (define pivot-value (vector-ref vec pivot-index))
  (swap vec pivot-index right)
  (define store-index left)
  (for ([i (in-range left right)])
     (when (less-than? (vector-ref vec i) pivot-value)
       (swap vec store-index i)
       (set! store-index (add1 store-index))))
  (swap vec right store-index)
  store-index)



(define less-than? <)



(define-syntax (swap stx)
  (syntax-case stx ()
    [(_ vec x y)
     #'(let ([t (vector-ref vec x)])
	 (vector-set! vec x (vector-ref vec y))
	 (vector-set! vec y t))]))
