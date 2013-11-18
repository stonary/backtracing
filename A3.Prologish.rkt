#lang racket
(provide an-element sub-list shuffled)
#| Re-implement the above functions.

 Use the implementation style of `the-length` shown in the `test` module below.
 It's the "Backtracking-Box" style from lecture, partially captured in the given
  `define-assert-match`.

 Do not use `-<`, `fail` nor `assert-match` directly, except in the helper
  function `with` where you may use `-<` and must use `assert-match`. |#

(require "CSC324.2013F.Backtracking.Library.rkt")

(define-syntax assert-match
  (syntax-rules (:-)
    [(assert-match «expr» «pattern» :- «result-expr» ...)
     (match «expr»
       [«pattern» «result-expr» ...]
       [_ (fail)])]))
(define-syntax define-assert-match
  (syntax-rules (:-)
    [(define-assert-match («f-id» «arg-id» «result-box-id»)
       («pattern» :- «result-expr» ...)
       ...)
     (define («f-id» «arg-id» «result-box-id»)
       (-< (assert-match «arg-id» «pattern» :- «result-expr» ...)
           ...))]))

(module+ test
  (define-assert-match (the-length L N)
    ('() :- (set-box! N 0))
    (`(,_ . ,T) :-
                (define N1 (box (void)))
                (the-length T N1)
                (set-box! N (add1 (unbox N1)))))
  (list-all (define N (box (void)))
            (the-length '(3 2 4) N)
            (unbox N)))

(define-assert-match (an-element L E)
  )

(define-assert-match (a-sub-list L S)
  )

(define (with ℓ e)
  (void))

(define-assert-match (shuffled L S)
  )