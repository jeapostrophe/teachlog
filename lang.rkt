#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         teachlog)
(provide #%module-begin
         #%datum
         #%top-interaction
         (except-out (all-from-out teachlog)
                     teachlog teachlog-interact
                     :- ? next)
         (rename-out [tl-:- :-]
                     [tl-? ?]
                     [tl-next next]))

(define current-theory (box empty-theory))

(define-syntax-rule (define-tl-form tl-:- :-)
  (define-syntax (tl-:- stx)
    (syntax-parse stx
      [(_ . args)
       (quasisyntax/loc stx
         (tl-top #,(syntax/loc stx (:- . args))))])))

(define-tl-form tl-:- :-)
(define-tl-form tl-? ?)
(define-tl-form tl-next next)

(define-syntax (tl-top stx)
  (syntax-parse stx
    [(_ e)
     (syntax/loc stx
       (set-box! current-theory
                 (teachlog-interact (unbox current-theory) e)))]))
