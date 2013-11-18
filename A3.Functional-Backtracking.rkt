#lang racket
(provide pair-of with shuffled prime-factor)
#| Implement the above backtrackable functions.

 They're described below, mainly in terms of the property each of the values
  returned (via backtracking) has. |#

(require "CSC324.2013F.Backtracking.Library.rkt")

(define (an-element ℓ)
  (match ℓ
    [`(,H . ,T) (-< H (an-element T))]
    [_ (fail)]))

#| A two-element list with first element from list `A`, second from list `B`. |#
(define (pair-of A B) (void))

#| The list `ℓ` with `e` inserted somewhere. |#
(define (with ℓ e)
  (void))

#| A list of the elements of list `ℓ` in some order.
   Hint: use `with`. |#
(define (shuffled ℓ)
  (void))

#| A prime factor of positive natural number `n`, including repetitions. |#
(define (prime-factor n [d 2]) ; [d 2] means a default argument of 2 for `d`.
  (void))