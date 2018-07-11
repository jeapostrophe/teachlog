#lang racket/base
(require (for-syntax racket/base
                     syntax/id-set
                     syntax/parse
                     racket/match)
         racket/match
         racket/stream)

;; Non-determinism Engine
(struct nd ())
(struct bind nd (x mx))
(struct choice nd (x y))
(struct fail nd ())
(struct ans nd (a))

(struct kont:return ())
(struct kont:bind (mx k))

(struct st (p kont))

(define (sols q)
  (match q
    [(list) empty-stream]
    [(cons (st p k) q)
     (match p
       [(bind x mx) (sols (list* (st x (kont:bind mx k)) q))]
       [(choice x y) (sols (list* (st x k) (st y k) q))]
       [(fail)
        (match k
          [(kont:return) (sols q)]
          [(kont:bind _ k) (sols (list* (st p k) q))])]
       [(ans a)
        (match k
          [(kont:return) (stream-cons a (sols q))]
          [(kont:bind mx k) (sols (list* (st (mx a) k) q))])])]))
(define (run p)
  (sols (list (st p (kont:return)))))

;; Logic Variables
(struct lvar (dx x))
(define-syntax-rule (with-lvars (v ...) e)
  (let ([v (lvar 'v (gensym 'v))] ...) e))

(define (env-deref env v)
  (define (rec v) (env-deref env v))
  (match v
    [(list) (list)]
    [(cons a d) (cons (rec a) (rec d))]
    [(lvar dx x) (env-deref env (hash-ref env v x))]
    [_ v]))

;; Unification
(define (unify env lhs rhs)
  (cond
    [(equal? lhs rhs)
     (ans env)]
    [(lvar? lhs)
     (cond
       [(hash-ref env lhs #f)
        => (λ (lhs-v) (unify env lhs-v rhs))]
       [else
        (ans (hash-set env lhs rhs))])]
    [(lvar? rhs)
     (unify env rhs lhs)]
    [(and (pair? lhs) (pair? rhs))
     (bind (unify env (car lhs) (car rhs))
           (λ (new-env)
             (unify new-env (cdr lhs) (cdr rhs))))]
    [else
     (fail)]))

;; Logic Engine
(define (search1 all-rules env rule1 q)
  (match-define (cons head body) (rule1))
  (bind (unify env head q)
        (λ (new-env)
          (searchN all-rules new-env body))))

(define (search* all-rules env rules q)
  (match rules
    [(list) (fail)]
    [(cons ruleN rules)
     (choice (search* all-rules env rules q)
             (search1 all-rules env ruleN q))]))

(define (search all-rules env q)
  (search* all-rules env all-rules q))

(define (searchN all-rules env qs)
  (for/fold ([p (ans env)])
            ([q (in-list qs)])
    (bind p (λ (new-env) (search all-rules new-env q)))))

(define (search-top all-rules q)
  (run (bind (search all-rules (hasheq) q)
             (λ (env)
               (ans (env-deref env q))))))

;; Runtime
(struct theory (rules sols))
(define empty-theory
  (theory '() #f))
(define (theory-add thy new-rule)
  (match-define (theory rules sols) thy)
  (values (when sols
            "Theory changed: Dropping pending solutions")
          (theory (cons new-rule rules) #f)))
(define (theory-query thy query)
  (match-define (theory rules _) thy)
  (theory-next
   (theory rules (search-top rules query))))
(define (theory-next thy)
  (match-define (theory rules sols) thy)
  (match sols
    [#f (values "No active query" thy)]
    [(? stream-empty?) (values "No solutions" thy)]
    [s (values (stream-first s) (theory rules (stream-rest s)))]))
(provide empty-theory)

;; UI
(define (teachlog-print v)
  (local-require racket/pretty)
  (match v
    [(? string?) (displayln v)]
    [(? void?) (void)]
    [(? list?) (pretty-write v)]))

;; Syntax
(begin-for-syntax
  (define empty-free-id-set (immutable-free-id-set))
  (define-syntax-class (static-info which-info? which)
    #:attributes (x vars)
    (pattern (r t:term ...)
             #:declare r (static which-info? which)
             #:attr x this-syntax
             #:attr vars
             (apply free-id-set-union empty-free-id-set (attribute t.vars))))
  (define-syntax-class term
    #:attributes (x vars)
    (pattern (~or x:number x:string)
             #:attr vars empty-free-id-set)
    (pattern n:id
             #:attr x #'n
             #:attr vars (free-id-set-add empty-free-id-set #'n))
    (pattern ((~literal unsyntax) e)
             #:attr x #'e
             #:attr vars empty-free-id-set)
    (pattern i
             #:declare i (static-info data-info? "data")
             #:attr x (attribute i.x)
             #:attr vars (attribute i.vars)))
  (define-syntax-class clause
    #:attributes (x vars)
    (pattern i
             #:declare i (static-info relation-info? "relation")
             #:attr x (attribute i.x)
             #:attr vars (attribute i.vars))))

(begin-for-syntax
  (struct info (name arity)
    #:property prop:procedure
    (λ (i stx)
      (match-define (info name arity) i)
      (syntax-parse stx
        [(_ t:term ...)
         #:do [(define actual (length (syntax->list #'(t ...))))]
         #:fail-unless (= actual arity)
         (format "expected ~a arguments, got ~a" arity actual)
         #:with rname name
         (syntax/loc stx
           (list 'rname t.x ...))])))
  (struct relation-info info ())
  (struct data-info info ()))

(define-syntax-rule (define-info-syntax relation relation-info)
  (define-syntax (relation stx)
    (syntax-parse stx
      [(_ r:id a:nat)
       (syntax/loc stx
         (define-syntax r (relation-info 'r 'a)))])))

(define-info-syntax relation relation-info)
(provide relation)

(define-info-syntax data data-info)
(provide data)

(define-syntax (:- stx)
  (syntax-parse stx
    [(_ thy h:clause b:clause ...)
     #:with (v ...)
     (free-id-set->list
      (apply free-id-set-union (attribute h.vars)
             (attribute b.vars)))
     (syntax/loc stx
       (theory-add thy
                   (λ ()
                     (with-lvars (v ...)
                       (list h.x b.x ...)))))]))
(provide :-)

(define-syntax (? stx)
  (syntax-parse stx
    [(_ thy h:clause)
     #:with (v ...) (free-id-set->list (attribute h.vars))
     (syntax/loc stx
       (with-lvars (v ...)
         (theory-query thy h.x)))]))
(provide ?)

(define-syntax-rule (next thy) (theory-next thy))
(provide next)

(begin-for-syntax
  (define-syntax-rule
    (define-literal-syntax-class the-class (the-literal ...))
    (define-syntax-class the-class (pattern (~literal the-literal)) ...)))

(begin-for-syntax
  (define-literal-syntax-class teachlog-form (:- ? next))
  (define-literal-syntax-class teachlog-bind (relation data)))

(define-syntax (teachlog-interact stx)
  (syntax-parse stx
    [(_ thy (~and e (b:teachlog-bind . _))) #'e]
    [(_ thy (f:teachlog-form . fargs:expr))
     (syntax/loc stx
       (let-values ([(result next-thy) (f thy . fargs)])
         (teachlog-print result)
         next-thy))]))
(provide teachlog-interact)

(define-syntax (teachlog-do stx)
  (syntax-parse stx
    [(_ thy:id (~and e (b:teachlog-bind . _))) #'e]
    [(_ thy:id (~and e (f:teachlog-form . _)))
     (syntax/loc stx
       (set-box! thy (teachlog-interact (unbox thy) e)))]))
(provide teachlog-do)

(define-syntax (teachlog stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:theory thy:expr)) e ...)
     (syntax/loc stx
       (let ([the-thy (box (~? thy empty-theory))])
         (teachlog-do the-thy e) ...
         (unbox the-thy)))]))
(provide teachlog)

;; Language

(module reader syntax/module-reader
  #:language '(submod teachlog lang))

(module+ lang
  (provide (rename-out [tl-mbegin #%module-begin]
                       [tl-top #%top-interaction])
           #%datum
           relation data
           ? :- next)

  (define current-theory (box empty-theory))

  (define-syntax-rule (tl-top . e)
    (teachlog-do current-theory e))

  (define-syntax-rule (tl-mbegin e ...)
    (#%module-begin (tl-top . e) ...)))
