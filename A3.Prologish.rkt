#lang racket

; AUTHORS: g2hushi, g2husath

(provide an-element a-sub-list shuffled)
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
  (`(,H . ,T) :- (set-box! E H))
  (`(,H . ,T) :- (an-element T E))
)

(define-assert-match (a-sub-list L S)
  ('() :- (set-box! S '()))
  (`(,H . ,T) :- 
              (define S1 (box (void)))
              (a-sub-list T S1)
              (set-box! S (cons H (unbox S1))))
  (`(,H . ,T) :- (a-sub-list T S))
)

(define (with ℓ e)
  (with-helper ℓ e))
  
(define-assert-match (with-helper ℓ e)
  (`()        :-  (list e))
  (`(,H . ,T) :-  (cons e ℓ))
  (`(,H . ,T) :-  (cons H (with T e))))

(define-assert-match (shuffled L S)
  (`()        :- (set-box! S L))
  (`(,H . ,T) :- 
              (define b (box (void)))
              (shuffled T b)
              (set-box! S
                        (with (unbox b) H)))
)

(module+ test
  (define l (list 1 2 3))
  (define el (list))
  (define nl (list 1 2 (list 2)))
  (define sl (list 1))
  
  ; helper test function
  (define (test-func f L)
    (list-all  (define b (box (void)))
               (f L b)
               (unbox b)))
  
  (displayln "test an-element for a list")
  (test-func an-element l)
  (displayln "test an-element for empty list")
  (test-func an-element el)
  (displayln "test an-element for nested list")
  (test-func an-element nl)
  (displayln "test an-element for singleton")
  (test-func an-element sl)
  
  (displayln "test a-sub-list with empty list")
  (test-func a-sub-list el)
  (displayln "test a-sub-list with nested list")
  (test-func a-sub-list nl)
  (displayln "test a-sub-list with singleton")
  (test-func a-sub-list sl)
  (displayln "test a-sub-list")
  (test-func a-sub-list l) 
  
  (displayln "test with with empty list")
  (list-all (with el 4))
  (displayln "test with with singleton")
  (list-all (with sl 4))
  (displayln "test with with nested list")
  (list-all (with nl 4))
  (displayln "test with with list")
  (list-all (with l 4))
  
  (displayln "test shuffled with empty list")
  (test-func shuffled el)
  (displayln "test shuffled with singleton")
  (test-func shuffled sl) 
  (displayln "test shuffled with nested list")
  (test-func shuffled nl) 
  (displayln "test shuffled with list")
  (test-func shuffled l)  
)



