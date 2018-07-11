#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         teachlog)
(provide (rename-out [tl-mbegin #%module-begin]
                     [tl-top #%top-interaction])
         #%datum         
         (except-out (all-from-out teachlog)
                     teachlog teachlog-interact teachlog-do))

(define current-theory (box empty-theory))

(define-syntax (tl-top stx)
  (syntax-parse stx
    [(_ . e)
     (syntax/loc stx
       (teachlog-do current-theory e))]))

(define-syntax (tl-mbegin stx)
  (syntax-parse stx
    [(_ e ...)
     (syntax/loc stx
       (#%module-begin
        (tl-top . e) ...))]))
