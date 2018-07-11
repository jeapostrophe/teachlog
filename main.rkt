#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/match)

;; Runtime
(struct theory ())
(define empty-theory
  (theory))
(define (theory-add thy head body)
  (error 'theory-add "XXX"))
(define (theory-query thy query)
  (error 'theory-query "XXX"))
(define (theory-next thy)
  (error 'theory-next "XXX"))

;; UI
(define (teachlog-print v)
  (match v
    [(? void?) (void)]))

;; Syntax
(define-syntax (theory-ref stx)
  (raise-syntax-error 'theory-ref "Illegal reference" stx))
(begin-for-syntax
  (define (illegal-use what stx)
    (raise-syntax-error what "Illegal outside of teachlog" stx)))

(begin-for-syntax
  (struct info (name arity)
    #:property prop:procedure
    (λ (i stx) (illegal-use (info-name i) stx)))
  (struct relation-info info ())
  (struct data-info info ()))

(define-syntax (relation stx)
  (syntax-parse stx
    #:literals (theory-ref)
    [(_ (theory-ref _) ~! r:id a:nat)
     (syntax/loc stx
       (define-syntax r (relation-info 'r 'a)))]
    [(_ r:id a:nat) (illegal-use 'relation stx)]))
(provide relation)

(define-syntax (data stx)
  (syntax-parse stx
    #:literals (theory-ref)
    [(_ (theory-ref _) ~! d:id a:nat)
     (syntax/loc stx
       (define-syntax d (data-info 'd 'a)))]
    [(_ d:id a:nat) (illegal-use 'data stx)]))
(provide data)

(begin-for-syntax
  (define-syntax-class (static-info which-info? which)
    #:attributes (x)
    (pattern (r t:term ...)
             #:declare r (static which-info? which)
             #:do [(define expected (info-arity (attribute r.value)))
                   (define actual (length (syntax->list #'(t ...))))]
             #:fail-unless (= actual expected)
             (format "expected ~a arguments, got ~a" expected actual)
             #:attr x
             (quasisyntax/loc this-syntax
               (list '#,(info-name (attribute r.value)) t.x ...))))
  (define-syntax-class term
    #:attributes (x)
    (pattern x:number)
    (pattern x:string)
    (pattern n:id #:attr x #''n)
    (pattern i
             #:declare i (static-info data-info? "data")
             #:attr x (attribute i.x)))
  (define-syntax-class clause
    #:attributes (x)
    (pattern i
             #:declare i (static-info relation-info? "relation")
             #:attr x (attribute i.x))))

(define-syntax (:- stx)
  (syntax-parse stx
    #:literals (theory-ref)
    [(_ (theory-ref thy) ~! h:clause b:clause ...)
     (syntax/loc stx (theory-add thy h.x (list b.x ...)))]
    [(_ (hr . _) (br . _) ...) (illegal-use ':- stx)]))
(provide :-)

(define-syntax (? stx)
  (syntax-parse stx
    #:literals (theory-ref)
    [(_ (theory-ref thy) ~! h:clause)
     (syntax/loc stx (theory-query thy h.x))]
    [(_ q:clause) (illegal-use '? stx)]))
(provide ?)

(define-syntax (next stx)
  (syntax-parse stx
    #:literals (theory-ref)
    [(_ (theory-ref thy) ~!)
     (syntax/loc stx (theory-next thy))]
    [(_) (illegal-use 'next stx)]))
(provide next)

(begin-for-syntax
  (define-syntax-rule
    (define-literal-syntax-class the-class (the-literal ...))
    (define-syntax-class the-class
      #:literals (the-literal ...)
      (pattern the-literal)
      ...)))

(begin-for-syntax
  (define-literal-syntax-class teachlog-form
    (:- ? next))
  (define-literal-syntax-class teachlog-bind
    (relation data)))
(define-syntax (teachlog-do stx)
  (syntax-parse stx
    [(_ thy:id)
     #'thy]
    [(_ thy:id (b:teachlog-bind . bargs:expr) . es)
     (syntax/loc stx
       (let ()
         (b (theory-ref #f) . bargs)
         (teachlog-do thy . es)))]
    [(_ thy:id (f:teachlog-form . fargs:expr) . es)
     (syntax/loc stx
       (let-values ([(result next-thy) (f (theory-ref thy) . fargs)])
         (teachlog-print result)
         (teachlog-do next-thy . es)))]))

;; XXX test on its own
(define-syntax (teachlog stx)
  (syntax-parse stx
    [(_ . es)
     (syntax/loc stx
       (teachlog-do empty-theory . es))]))
(provide teachlog)

;; XXX test on own
;; XXX test with repl
(define-syntax (teachlog-interact stx)
  (syntax-parse stx
    [(_ thy e)
     (syntax/loc stx
       (teachlog-do thy e))]))
(provide teachlog-interact)

;; Language

(module reader syntax/module-reader
  teachlog/lang)
