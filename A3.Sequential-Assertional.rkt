#lang racket

; AUTHORS: g2hushi, g2husath

(provide coin coin-bounded change-bounded change)
#| Implement the above backtrackable functions.

 Follow the declarative style (partially given below) where backtracking and
  sequenced code with assertions is used instead of standard conditionals.
 Use the given `assert`.
 Don't use `if`, `cases`, `cond`, etc, nor `fail` directly. |#

(require "CSC324.2013F.Backtracking.Library.rkt"
         racket/block)

(define (assert true/false) (unless true/false (fail)))

#| One of the numbers 200, 100, 25, 10, or 5 (in that order). |#
(define (coin) (-< 200 100 25 10 5))

#| A coin ≤ `b`. |#
(define (coin-bounded b)
  (block
   (define c (coin))
   (assert (<= c b))
   c))

#| A non-increasing list of coins adding up to `n`, using only coins ≤ `b`. |#
(define (change-bounded n b)
  (-< (block 
       (define c (coin-bounded b))
       (assert (< c n))
       (cons c (change-bounded (- n c) c)))
      (block 
       (define c (coin-bounded b))
       (assert (= c n))
       (list c))))

(define (change n) (change-bounded n 200))

(module+ test
  (displayln "testing coin:")
  (list-all (coin))
  (displayln "testing bounded coin:")
  (list-all (coin-bounded 100))
  
  (displayln "testing change-bounded: 200 with <= 25 changes")
  (list-all (change-bounded 200 25)) 
  
  (equal? 450 (length (list-all (change 325)))))