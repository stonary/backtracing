#lang racket

; AUTHORS: g2hushi, g2husath

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
  
  (list-all (pair-of l1 l2)))

#| The list `ℓ` with `e` inserted somewhere. |#
(define (with ℓ e)
  (match ℓ
    [`(,H . ,T) (-< (cons e ℓ)
                    (append (list (first ℓ))
                            (with (rest ℓ) e)))]
    [_ (list e)]))

(module+ test
  (define l3 '(1 2 3 4 5 6 7 8 9 10))
  (list-all (with l3 'x)))

#| A list of the elements of list `ℓ` in some order.
   Hint: use `with`. |#

(define (shuffled ℓ)
  (if (empty? ℓ) (list)
      (with (shuffled (rest ℓ)) (first ℓ))))

(module+ test
  (define l4 '(1 2 3 4))
  (list-all (shuffled l4)))

#| A prime factor of positive natural number `n`, including repetitions. |#


(define (prime-factor n [d 2]) ; [d 2] means a default argument of 2 for `d`.
  (match n
    [0 (void)] ; Not expected to reach this case.
    [1 (fail)]
    [_ (match (modulo n d)
         [0 (-< d (prime-factor (/ n d) 2))]
         [_ (prime-factor n (+ d 1))])]))

#| Proof that prime-factor always return a prime.

   Base case: n=2
   - By precondition, d=2
   - The function then matches n and d. Since they match, the function performs n%2.
   - As 2%2 = 0, the function returns 2. Hence it works correctly.
   Base case: n=3
   - By precondition, d=2
   - The function matches n and d. Since they are unequal, the function moves on.
   - So then 3%2 is performed. As 3%2 = 1, prime-factor is called with n=3, d=3.
   - Since n=d, the function returns d. d=3 which is a prime number. Therefore the 
     function works correctly.

   Inductive Hypothesis: prime-factor works correctly for n.

   Inductive Step: call prime-factor on k+1.
   - d=2 by precondition.
   - The function matches n to d.
      - If n equals d, then the function return.
         - Getting to this case means that n has only 2 factors: 1 and itself. 
           Hence n is a prime number. Since d=n, the return is also a prime.
         - IH. ensures that the k is not a prime number. If it is, the function would have stopped,
           and return before k+1.
      - If n unequal to d, then the function does more comparison:
         - if n%d = 0 then:
            - d must be a prime. 
            - if d is not a prime, then the function cannot reach k+1 because it would be divisible
              by a smaller factor. IH. ensures that any smaller prime factor would be returned first.
         - Otherwise, d is not a factor of n. The function recurses until any of the cases above 
           happen.

   QED. |#


(module+ test
  (list-all (prime-factor 121 2))
  (list-all (prime-factor 14325 2))
  (list-all (prime-factor 1299827 2)))
