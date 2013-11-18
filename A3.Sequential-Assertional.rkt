#lang racket
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
(define (coin) '<TODO>)

#| A coin ≤ `b`. |#
(define (coin-bounded b)
  (define c (coin))
  '<TODO>)

#| A non-increasing list of coins adding up to `n`, using only coins ≤ `b`. |#
(define (change-bounded n b)
  (-< (block '<TODO>)
      (block '<TODO>)))

(define (change n) (change-bounded n 200))

(module+ test
  (equal? 450 (length (list-all (change 325)))))