#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         teachlog)
(provide (rename-out [tl-module-begin #%module-begin]
                     [tl-top #%top-interaction])
         #%datum
         (except-out (all-from-out teachlog)
                     teachlog teachlog-interact))

(define current-theory (box #f))

(define-syntax (tl-module-begin stx)
  (syntax-parse stx
    [(_ e ...)
     (syntax/loc stx
       (#%module-begin
        (set-box! current-theory (teachlog e ...))))]))

(define-syntax (tl-top stx)
  (syntax-parse stx
    [(_ . e)
     (syntax/loc stx
       (set-box! current-theory
                 (teachlog-interact (unbox current-theory)
                                    e)))]))
