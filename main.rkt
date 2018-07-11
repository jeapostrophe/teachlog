#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/match
         racket/list
         racket/set
         racket/stream)

;; Non-determinism Engine
(struct nd ())
(struct bind nd (x mx))
(struct choice nd (x y))
(struct fail nd ())
(struct ans nd (a))

(struct kont:return ())
(struct kont:bind (mx k))
(struct kont:choice (y k))

(struct st (p kont) #:transparent)

(define (sols q)  
  (match q
    [(list) empty-stream]
    [(cons (and s (st p k)) q)
     (match p
       [(bind x mx) (sols (cons (st x (kont:bind mx k)) q))]
       [(choice x y) (sols (cons (st x (kont:choice y k)) q))]
       [(fail)
        (match k
          [(kont:return) (sols q)]
          [(kont:bind _ k) (sols (cons (st p k) q))]
          [(kont:choice y k) (sols (cons (st y k) q))])]
       [(ans a)
        (match k
          [(kont:return) (stream-cons a (sols q))]
          [(kont:bind mx k) (sols (cons (st (mx a) k) q))]
          [(kont:choice y k) (sols (cons (st p k) (cons (st y k) q)))])])]))
(define (run p)
  (sols (list (st p (kont:return)))))

;; Logic Variables
(struct lvar (dx x))
(define (extract-vars x)
  (match x
    [(cons a d) (set-union (extract-vars a) (extract-vars d))]
    [(lvar _ _) (seteq x)]
    [_ (seteq)]))
(define (rename-vars ρ x)
  (define (rec x) (rename-vars ρ x))
  (match x
    [(cons a d) (cons (rec a) (rec d))]
    [(lvar _ _) (hash-ref ρ x)]
    [_ x]))
(define (freshen r)
  (rename-vars
   (for/hasheq ([v (in-set (extract-vars r))])
     (match-define (lvar dx _) v)
     (values v (lvar dx (gensym))))
   r))
(define (bound? env v)
  (and (lvar? v) (hash-has-key? env v)))
(define (unbound? env v)
  (and (lvar? v) (not (hash-has-key? env v))))

(define (env-deref env v)
  (define (rec v) (env-deref env v))
  (match v
    [(list) (list)]
    [(cons a d) (cons (rec a) (rec d))]
    [(lvar dx x)
     (define xv (hash-ref env v #f))
     (if xv
       (env-deref env xv)
       x)]
    [_ v]))

;; Unification
(define (unify env lhs rhs)
  (cond
    [(equal? lhs rhs)
     (ans env)]
    [(unbound? env lhs)
     (ans (hash-set env lhs rhs))]
    [(unbound? env rhs)
     (ans (hash-set env rhs lhs))]
    [(bound? env lhs)
     (unify env (hash-ref env lhs) rhs)]
    [(bound? env rhs)
     (unify env lhs (hash-ref env rhs))]
    [(and (pair? lhs) (pair? rhs))
     (bind (unify env (car lhs) (car rhs))
           (λ (new-env)
             (unify new-env (cdr lhs) (cdr rhs))))]
    [else
     (fail)]))

;; Logic Engine
(define (search1 all-rules env rule1 q)
  (match-define (cons head body) (freshen rule1))
  (bind (unify env head q)
        (λ (new-env)
          (searchN all-rules new-env body))))

(define (search* all-rules env rules q)
  (match rules
    [(list) (fail)]
    [(cons rule1 rules)
     (choice (search1 all-rules env rule1 q)
             (search* all-rules env rules q))]))

(define (search all-rules env q)
  (search* all-rules env all-rules q))

(define (searchN all-rules env qs)
  (for/fold ([p (ans env)])
            ([q (in-list qs)])
    (bind p (λ (new-env) (search all-rules new-env q)))))

(define (search-top all-rules q)
  (bind (search all-rules (hasheq) q)
        (λ (env)
          (ans (env-deref env q)))))

;; Runtime
(struct theory (rules sols))
(define empty-theory
  (theory empty #f))
(define (theory-add thy head body)
  (match-define (theory rules sols) thy)
  (values (when sols
            "Theory changed: Dropping pending solutions")
          (theory (cons (cons head body) rules)
                  #f)))
(define (theory-query thy query)
  (match-define (theory rules sols) thy)
  (theory-next (theory rules (run (search-top rules query)))))
(define (theory-next thy)
  (match-define (theory rules sols) thy)
  (match sols
    [#f (values "No active query" thy)]
    [(? stream-empty?) (values "No solutions" thy)]
    [s (values (stream-first s) (theory rules (stream-rest s)))]))

;; UI
(define (teachlog-print v)
  (local-require racket/pretty)
  (match v
    [(? string?) (displayln v)]
    [(? void?) (void)]
    [(? list?) (pretty-display v)]))

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
    (pattern n:id #:attr x #'(lvar 'n 'n))
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
