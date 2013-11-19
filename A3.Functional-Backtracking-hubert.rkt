#lang racket
(require racket/block)

;(provide pair-of with shuffled prime-factor)
#| Implement the above backtrackable functions.

 They're described below, mainly in terms of the property each of the values
  returned (via backtracking) has. |#

(require "CSC324.2013F.Backtracking.Library.rkt")

(define (an-element ℓ)
  (match ℓ
    [`(,H . ,T) (-< H (an-element T))]
    [_ (fail)]))

#| A two-element list with first element from list `A`, second from list `B`. |#
(define (pair-of A B)
  (list (an-element A) (an-element (rest B))))

(module+ test
  (define l1 '(csc phl soc imc))
  (define l2 '(108 324 494 499))
  
  (pair-of l1 l2))

#| The list `ℓ` with `e` inserted somewhere. |#
(define (with ℓ e)
  (if (not (empty? ℓ))
      (-< (cons e ℓ)
          (append (list (first ℓ))
                  (with (rest ℓ) e)))
      (list e)))

(module+ test
  (define l3 '(1 2 3 4 5 6 7 8 9 10))
  (list-all (with l3 'x)))

#| A list of the elements of list `ℓ` in some order.
   Hint: use `with`. |#
#;(define (shuffled ℓ)
 (list-all 
  (if (empty? ℓ)
      (fail)
      (void))))

(define (remove-e ℓ i)
  (if (< i (length ℓ))
      (-< (append (take ℓ i) (drop ℓ (+ i 1)))
          (remove-e ℓ (+ i 1)))
      (fail)))

(define (shuffled ℓ)
  (with (remove-e ℓ 1) (an-element ℓ)))

(module+ test
  (define l4 '(1 2 3 4))
  (list-all (shuffled l4)))

#| A prime factor of positive natural number `n`, including repetitions. |#
(define (prime-factor n [d 2]) ; [d 2] means a default argument of 2 for `d`.
  (-< (if (% n 2))))
