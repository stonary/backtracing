#lang racket #| CSC324 2013F Backtracking Library. |#

(provide -< fail)
#| Two operations for making code that backtracks:

 1. Either «e₀», «e₁», «e» ... : (-< «e₀» «e₁» «e» ...).

   Implemented to evaluate «e₀» the first time.
   Then if backtracking occurs (via `fail`, `?`, `all` or `list-all`)
    and backs up to this as the most recent unfinished choice point,
    it evaluates «e₁» and continues on as before, etc.

 2. Abort and backtrack: (fail). |#

(provide ? No? no all list-all trace!)
#| Operations for using code that backtracks. 

 Isolates the backtracking parts, so it doesn't interact with control flow
  of the code using them.

 1. Delineate expressions invoking backtracking code, then allow access
     to the result values one at a time (iterator/generator/lazy-list style).

    (? «e₀» «e» ...) : Stage a block of expressions for evaluation.

    (?) : Evaluate one result, or return unique symbol indicating none.
    (No? a-result) : whether `a-result` is the symbol indicating none.
    (no) : Evaluate until no more results.

 2. (all «e₀» «e» ...) : Evaluate until no more results, discarding results.
      So used purely for any side-effects during evaluation.
 
 3. (list-all «e₀» «e» ...) : Evaluate until no more results, returning a
     list of all the results. |#

#| Tracing: (trace! #t) to enable some tracing, (trace! #f) to disable. |#
(provide trace!)


#| IMPLEMENTATION THAT FOLLOWS IS NOT REQUIRED READING. |#


#| Implementation of `-<`, `fail` and `trace!`. |#
(define-syntax-rule (-< «e₀» «e» ...)
  #;(let/cc choice-point
      (pop/push (λ () (choice-point «e»)) ...)
      «e₀»)
  (let/cc choice-point
    (when (trace!) (printf "Choice point: ~v\n" '(-< «e₀» «e» ...)))
    (pop/push (λ () (announce «e») (choice-point «e»)) ...)
    (announce «e₀») «e₀»))
(define (fail) ((pop/push)))
(define trace! (make-parameter #f))
;
; Support for `trace!`.
;
(define-syntax-rule (announce «e») (when (trace!) (printf "Choice: ~v\n" '«e»)))
;
; Support for `-<` and `fail`.
;
(require racket/block)
(define get/set
  (block
   ; Local variable since Racket disallows mutating top-level when as a library.
   (define alternatives (void))
   (case-lambda [() alternatives]
                [(new-alternatives) (set! alternatives new-alternatives)])))
(define pop/push
  (case-lambda [() (match (get/set) [`(,alt . ,alts) (get/set alts) alt])]
               [alts (get/set (append alts (get/set)))]))


#| Implementation of `?`, `No?` and `no`. |#
(define-syntax ?
  (syntax-rules ()
    [(?) (prompt (fail))]
    [(? «e» ...) (pop/push (λ () (prompt «e» ...)))]))
(define (No? a) (equal? a No))
(define (no) (get/set (list no)) (abort No))
;
; Support for `?`, `No?` and `no`.
;
(require (only-in racket/control prompt abort))
(define No (gensym 'No.))
(get/set (list no))


#| Implementation of `all` and `list-all`. |#
(define-syntax-rule (all «e» ...) (prompt «e» ... (fail)))
(define-syntax-rule (list-all «e» ...)
  (block (define ℓ '())
         (all (set! ℓ (cons (block «e» ...) ℓ)))
         (reverse ℓ)))
