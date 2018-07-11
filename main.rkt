#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     syntax/id-set
                     syntax/parse
                     racket/syntax)
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
(struct lvar (dx x) #:transparent)
(define-syntax-rule (with-lvars (v ...) e)
  (let ([v (lvar 'v (gensym 'v))] ...) e))
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
(define (render-query vs q)
  (env-deref (for/hasheq ([v (in-set vs)])
               (values v (lvar-dx v)))
             q))

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
  (match-define (cons head body) (rule1))
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

(define (search-top all-rules vs q)
  (bind (search all-rules (hasheq) q)
        (λ (env)
          (ans
           (for/list ([v (in-set vs)])
             (list (lvar-dx v) '= (env-deref env v)))))))

;; Runtime
(define (snoc l x) (append l (list x)))
(struct theory (rules q sols))
(define empty-theory
  (theory empty #f #f))
(define (theory-add thy new-rule)
  (match-define (theory rules _ sols) thy)
  (values (when sols
            "Theory changed: Dropping pending solutions")
          (theory (snoc rules new-rule)
                  #f #f)))
(define (theory-query thy vars query)
  (match-define (theory rules _ _) thy)
  (theory-next
   (theory rules (render-query vars query)
           (run
            (search-top rules vars query)))))
(define (theory-next thy)
  (match-define (theory rules q sols) thy)
  (match sols
    [#f (values "No active query" thy)]
    [(? stream-empty?)
     (values (list q '=> #f)
             thy)]
    [s
     (values (list q '=> (stream-first s))
             (theory rules q (stream-rest s)))]))
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
  (struct info (name arity)
    #:property prop:procedure
    (λ (i stx)
      (raise-syntax-error (info-name i) "Illegal outside of :- or ?" stx)))
  (struct relation-info info ())
  (struct data-info info ()))

(define-syntax (relation stx)
  (syntax-parse stx
    [(_ r:id a:nat)
     (syntax/loc stx
       (define-syntax r (relation-info 'r 'a)))]))
(provide relation)

(define-syntax (data stx)
  (syntax-parse stx
    [(_ d:id a:nat)
     (syntax/loc stx
       (define-syntax d (data-info 'd 'a)))]))
(provide data)

(begin-for-syntax
  (define empty-free-id-set (immutable-free-id-set))
  (define-syntax-class (static-info which-info? which)
    #:attributes (x vars)
    (pattern (r t:term ...)
             #:declare r (static which-info? which)
             #:do [(record-disappeared-uses #'r)
                   (define expected (info-arity (attribute r.value)))
                   (define actual (length (syntax->list #'(t ...))))]
             #:fail-unless (= actual expected)
             (format "expected ~a arguments, got ~a" expected actual)
             #:attr x
             (quasisyntax/loc this-syntax
               (list '#,(info-name (attribute r.value)) t.x ...))
             #:attr vars
             (apply free-id-set-union empty-free-id-set (attribute t.vars))))
  (define-syntax-class term
    #:attributes (x vars)
    (pattern x:number
             #:attr vars empty-free-id-set)
    (pattern x:string
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

(define-syntax (:- stx)
  (with-disappeared-uses
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
                         (list h.x b.x ...)))))])))
(provide :-)

(define-syntax (? stx)
  (with-disappeared-uses
    (syntax-parse stx
      [(_ thy h:clause)
       #:with (v ...) (free-id-set->list (attribute h.vars))
       (syntax/loc stx
         (with-lvars (v ...)
           (theory-query thy (list v ...) h.x)))])))
(provide ?)

(define next theory-next)
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

(define-syntax (teachlog-interact stx)
  (syntax-parse stx
    [(_ thy (~and e (b:teachlog-bind . _)))
     (syntax/loc stx e)]
    [(_ thy (f:teachlog-form . fargs:expr))
     (syntax/loc stx
       (let-values ([(result next-thy) (f thy . fargs)])
         (teachlog-print result)
         next-thy))]))
(provide teachlog-interact)

(define-syntax (teachlog-do stx)
  (with-disappeared-uses
    (syntax-parse stx
      [(_ thy:id (~and e (b:teachlog-bind . _)))
       (syntax/loc stx e)]
      [(_ thy:id (~and e (f:teachlog-form . _)))
       (syntax/loc stx
         (set-box! thy (teachlog-interact (unbox thy) e)))])))
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
          (tl-top . e) ...))])))
