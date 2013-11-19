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
(define (pair-of A B) (list (an-element A) (an-element B)))

#| The list `ℓ` with `e` inserted somewhere. |#
(define (with ℓ e)
  (match ℓ
    [`(,H . ,T) (-< (cons e ℓ) (cons H (with T e)))]
    [`() (list e)]
    [_ (fail)]))

#| A list of the elements of list `ℓ` in some order.
   Hint: use `with`. |#
(define (shuffled ℓ)
  (match ℓ
    [`() ℓ]
    [`(,H . ,T) (with (shuffled T) H)]
    [_ (fail)]))

#| A prime factor of positive natural number `n`, including repetitions. |#
(define (prime-factor n [d 2]) ; [d 2] means a default argument of 2 for `d`.
  (if (= d n) n
      (if (zero? (modulo n d))
          (-< d (prime-factor (/ n d) d))
          (prime-factor n (+ d 1)))))

(module+ test
  (define a (list 1 2 3))
  (define b (list 4 5 6))
  (define c (list))
  (define d 7)
  
  (displayln "Testing pair-of: 9 pairs")
  (pair-of a b) (?) (?) (?) (?) (?) (?) (?) (?) (?) 
  (displayln "Testing pair-of: 0 pairs with empty list")
  #;(pair-of a c) (?) 
  
  (displayln "Testing with: 4 positions")
  (with a d) (?) (?) (?) (?) 
  (displayln "Testing with: 1 position with empty list")
  (with c d) (?)  
  
  (displayln "Testing shuffled: 6 permutations")
  (shuffled a) (?) (?) (?) (?) (?)
  (displayln "Testing shuffled: 1 permutation with empty list")
  (shuffled c) (?)
  (displayln "Testing shuffled: 1 permutation with singliton")
  (shuffled (list d)) (?) 
  
  (displayln "Testing prime number for 2")
  (prime-factor 2) (?) 
  (displayln "Testing prime number for 4")
  (prime-factor 4) (?) (?) 
  (displayln "Testing prime number for 3")
  (prime-factor 3) (?) (?)
  (displayln "Testing prime number for 710518848894598995164656472095498960789946895931020")
  (prime-factor 710518848894598995164656472095498960789946895931020) 
  (?) (?) (?) (?) (?) (?) (?) (?) (?) (?) (?) (?) (?) (?) (?) (?) (?) (?)
  (?) (?) (?) (?) (?) (?) (?) (?) (?) (?) (?) (?) (?) (?) (?) (?) (?) (?)
  (?) (?)
  )
