#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         syntax/parse/define
         (prefix-in tl: (submod teachlog lang)))

(module reader syntax/module-reader
  teachlog/ugly
  #:read my-read
  #:read-syntax my-read-syntax
  #:whole-body-readers? #t
  (require racket/list
           teachlog/grammar
           brag/support)

  (define (tokenize ip)
    (port-count-lines! ip)
    (define my-lexer
      (lexer-src-pos
       [(repetition 1 +inf.0 numeric)
        (token 'NUMBER (string->number lexeme))]
       ["rel" (token 'REL 'relation)]
       ["data" (token 'DATA 'data)]
       ["next" (token 'NEXT 'next)]
       [":-" (token 'IMPLIED-BY ':-)]
       ["?" (token 'QMARK '?)]
       ["(" (token 'LPAREN)] [")" (token 'RPAREN)]
       ["/" (token 'SLASH)] ["." (token 'DOT)]
       ["," (token 'COMMA)] ["'" (token 'QUOTE)]
       [whitespace (my-lexer ip)]
       [(:: "%" (complement (:: any-string "\n" any-string)) "\n") (my-lexer ip)]
       [(:: "\"" (complement (:: any-string "\"" any-string)) "\"")
        (token 'STRING (substring lexeme 1 (sub1 (string-length lexeme))))]
       [(:+ alphabetic) (token 'ID (string->symbol lexeme))]
       [(eof) (void)]))
    (define (next-token) (my-lexer ip))
    next-token)

  (define (my-read in)
    (syntax->datum (my-read-syntax #f in)))

  (define (my-read-syntax src ip)
    (rest (syntax-e (parse src (tokenize ip))))))

(begin-for-syntax
  (define-syntax-class term
    #:attributes (x)
    (pattern ((~datum term) (~or x:number x:string x:id)))
    (pattern ((~datum term) #f e:id)
             #:attr x #''e)
    (pattern ((~datum term) d:id t:term ...)
             #:attr x #'(d t.x ...)))
  (define-syntax-class clause
    #:attributes (x)
    (pattern ((~datum clause) r:id t:term ...)
             #:attr x #'(r t.x ...)))
  (define-syntax-class stmt
    #:attributes (x)
    (pattern ((~datum rel) the-rel r:id n:nat)
             #:attr x #'(the-rel r n))
    (pattern ((~datum data) the-data d:id n:nat)
             #:attr x #'(the-data d n))
    (pattern ((~datum data) the-data d:id)
             #:attr x #'(the-data d))
    (pattern ((~datum fact) c:clause)
             #:attr x #'(tl::- c.x))
    (pattern ((~datum rule) h:clause the-bar b:clause ...)
             #:attr x #'(the-bar h.x b.x ...))
    (pattern ((~datum query) c:clause the-q)
             #:attr x #'(the-q c.x))
    (pattern ((~datum next) the-next)
             #:attr x #'(the-next))))

(define-simple-macro (umbegin e:stmt ...)
  (tl:#%module-begin e.x ...))

(provide (rename-out [umbegin #%module-begin]
                     [tl:relation relation] [tl:data data]
                     [tl:? ?] [tl::- :-] [tl:next next])
         #%datum)
