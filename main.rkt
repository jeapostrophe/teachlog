#lang racket/base
(require (for-syntax racket/base
                     syntax/id-set
                     syntax/parse
                     racket/match)
         syntax/parse/define
         racket/match
         racket/stream)

;; Non-determinism Engine
(struct nd ())
(struct ans nd (a))
(struct bind nd (mx f))
(struct fail nd ())
(struct choice nd (mx my))

(struct st (p k))
(struct kont:return ())
(struct kont:bind (f k))

(define (sols q)
  (match q
    [(list) empty-stream]
    [(cons (st p k) q)
     (match p
       [(bind mx f) (sols (list* (st mx (kont:bind f k)) q))]
       [(choice mx my) (sols (list* (st mx k) (st my k) q))]
       [(fail)
        (match k
          [(kont:return) (sols q)]
          [(kont:bind _ k) (sols (list* (st p k) q))])]
       [(ans a)
        (match k
          [(kont:return) (stream-cons a (sols q))]
          [(kont:bind f k) (sols (list* (st (f a) k) q))])])]))
(define (run p)
  (sols (list (st p (kont:return)))))

;; Logic Variables
(struct lvar (dx x) #:transparent)
(define-simple-macro (with-lvars (v:id ...) e:expr)
  (let ([v (lvar 'v (gensym 'v))] ...) e))

(define (env-deref env v)
  (define (rec v) (env-deref env v))
  (match v
    [(list) (list)]
    [(cons a d) (cons (rec a) (rec d))]
    [(lvar _ x) (rec (hash-ref env v x))]
    [_ v]))

;; Unification
(define (unify env lhs rhs)
  (match* (lhs rhs)
    [(x x) (ans env)]
    [((? lvar?) _)
     (match (hash-ref env lhs #f)
       [#f (ans (hash-set env lhs rhs))]
       [lhs-v (unify env lhs-v rhs)])]
    [(_ (? lvar?))
     (unify env rhs lhs)]
    [((cons la ld) (cons ra rd))
     (bind (unify env la ra)
           (λ (new-env) (unify new-env ld rd)))]
    [(_ _) (fail)]))

;; Logic Engine
(define (search1 all-rules env rule1 q)
  (match-define (cons head body) (rule1))
  (bind (unify env head q)
        (λ (new-env) (searchN all-rules new-env body))))

(define (search* all-rules env rules q)
  (match rules
    [(list) (fail)]
    [(cons ruleN rules)
     (choice (search* all-rules env rules q)
             (search1 all-rules env ruleN q))]))

(define (searchN all-rules env qs)
  (for/fold ([p (ans env)]) ([q (in-list qs)])
    (bind p (λ (new-env) (search* all-rules new-env all-rules q)))))

(define (search-top all-rules q)
  (run (bind (searchN all-rules (hasheq) (list q))
             (λ (env) (ans (env-deref env q))))))

;; Runtime
(struct theory (rules sols))
(define empty-theory (theory '() #f))
(define (theory-add thy new-rule)
  (match-define (theory rules sols) thy)
  (values (when sols
            "Theory changed; Dropping pending solutions")
          (theory (cons new-rule rules) #f)))
(define (theory-query thy query)
  (match-define (theory rules _) thy)
  (theory-next (theory rules (search-top rules query))))
(define (theory-next thy)
  (match-define (theory rules sols) thy)
  (match sols
    [#f (values "No active query" thy)]
    [(? stream-empty?) (values "No solutions" thy)]
    [s (values (stream-first s) (theory rules (stream-rest s)))]))
(provide empty-theory)

;; Syntax helpers
(begin-for-syntax
  (define empty-free-id-set (immutable-free-id-set))
  (define (free-id-set-union* l)
    (apply free-id-set-union empty-free-id-set l))
  (define-syntax-class (static-info which-info? which)
    #:attributes (vars)
    #:local-conventions ([w (static which-info? which)])
    (pattern w
             #:attr vars empty-free-id-set)
    (pattern (w t:term ...)
             #:attr vars (free-id-set-union* (attribute t.vars))))
  (define-syntax-class term
    #:attributes (x vars)
    (pattern (~or x:number x:string
                  ((~literal unsyntax) x)
                  (~and x ((~literal quote) _)))
             #:attr vars empty-free-id-set)
    (pattern (~var x (static-info data-info? "data"))
             #:attr vars (attribute x.vars))
    (pattern x:id
             #:attr vars (free-id-set-add empty-free-id-set #'x)))
  (define-syntax-class clause
    #:attributes (vars)
    (pattern (~var x (static-info relation-info? "relation"))
             #:attr vars (attribute x.vars))))

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
         #`(list '#,name t.x ...)]
        [r:id (syntax/loc stx (r))])))
  (struct relation-info info ())
  (struct data-info info ()))

(define-simple-macro (define-info-syntax relation:id relation-info:id)
  (define-simple-macro (relation r:id (~optional a:nat))
    (define-syntax r (relation-info 'r ((... ~?) 'a 0)))))

;; Syntax Interface
(define-info-syntax relation relation-info)
(define-info-syntax data data-info)

(define-simple-macro (:- thy h:clause b:clause ...)
  #:with (v ...)
  (free-id-set->list
   (free-id-set-union* (cons (attribute h.vars) (attribute b.vars))))
  (theory-add thy
              (λ ()
                (with-lvars (v ...)
                  (list h b ...)))))

(define-simple-macro (? thy h:clause)
  #:with (v ...) (free-id-set->list (attribute h.vars))
  (with-lvars (v ...)
    (theory-query thy h)))

(define-simple-macro (next thy) (theory-next thy))
(provide relation data :- ? next)

;; Language and interface helpers
(begin-for-syntax
  (define-syntax-class teachlog-bind
    (pattern (~literal relation))
    (pattern (~literal data))))

(define (teachlog-print v)
  (local-require racket/pretty)
  (match v
    [(? string?) (displayln v)]
    [(? void?) (void)]
    [_ (pretty-write v)]))

(define (teachlog-do!* thy-box tl-stmt)
  (define-values (result next-thy) (tl-stmt))
  (teachlog-print result)
  (set-box! thy-box next-thy))
(define-syntax-parser teachlog-do!
  [(_ thy:id (~and e (b:teachlog-bind . _)))
   #'e]
  [(_ thy:id (~and e (f . fargs:expr)))
   #'(teachlog-do!* thy (λ () (f (unbox thy) . fargs)))])

;; Main interface
(define-simple-macro (teachlog (~optional (~seq #:theory thy:expr)) e ...)
  (let ([the-thy (box (~? thy empty-theory))])
    (teachlog-do! the-thy e) ...
    (unbox the-thy)))
(provide teachlog)

;; Language
(module reader syntax/module-reader
  #:language '(submod teachlog lang))

(module+ lang
  (provide (rename-out [tl-mbegin #%module-begin]
                       [tl-top #%top-interaction])
           #%datum quote
           relation data
           ? :- next)

  (define current-theory (box empty-theory))

  (define-simple-macro (tl-top . e)
    (teachlog-do! current-theory e))

  (define-simple-macro (tl-mbegin e ...)
    (#%module-begin (tl-top . e) ...)))

;; Examples
(module+ test
  (require (submod ".." interop test)))

;;;; Using without #lang
(module* interop racket/base
  (require (submod ".."))

  (relation parent 2)
  (relation ancestor 2)

  (define targ
    (teachlog
     (:- (parent "maekar" "aegon-5"))
     (:- (parent "aegon-5" "aerys-2"))
     (:- (parent "aerys-2" "viserys"))
     (:- (parent "aerys-2" "daenerys"))
     (:- (parent "daenerys" #,(string-append "dro" "gon")))
     (:- (ancestor X Y)
         (parent X Y))
     (:- (ancestor X Z)
         (parent X Y)
         (ancestor Y Z))))

  (module+ test
    (teachlog #:theory targ
              (? (ancestor X "drogon"))
              (next) (next) (next) (next))))
