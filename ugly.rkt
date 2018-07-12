#lang racket/base

(module parser racket/base
  (require syntax/parse
           teachlog/grammar
           brag/support)
  (provide ugly-read ugly-read-syntax)

  (define (tokenize ip)
    (define ugly-lexer
      (lexer-src-pos
       [(repetition 1 +inf.0 numeric)
        (token 'NUMBER (string->number lexeme))]
       ["rel" (token 'REL 'relation)]
       ["data" (token 'DATA 'data)]
       ["next" (token 'NEXT 'next)]
       [":-" (token 'IMPLIED-BY ':-)]
       ["?" (token 'QMARK '?)]
       ["'" (token 'QUOTE 'quote)]
       ["(" (token 'LPAREN)] [")" (token 'RPAREN)]
       ["/" (token 'SLASH)] ["." (token 'DOT)] ["," (token 'COMMA)]
       [whitespace (ugly-lexer ip)]
       [(:: "%" (complement (:: any-string "\n" any-string)) "\n") (ugly-lexer ip)]
       [(:: "\"" (complement (:: any-string "\"" any-string)) "\"")
        (token 'STRING (substring lexeme 1 (sub1 (string-length lexeme))))]
       [(:+ alphabetic) (token 'ID (string->symbol lexeme))]
       [(eof) (void)]))
    (port-count-lines! ip)
    (λ () (ugly-lexer ip)))

  (define (ugly-read in)
    (syntax->datum (ugly-read-syntax #f in)))

  (define (ugly-read-syntax src ip)
    (syntax-parse (parse src (tokenize ip))
      [((~literal program) . contents) #'contents])))

(module* reader syntax/module-reader
  teachlog/ugly
  #:read ugly-read
  #:read-syntax ugly-read-syntax
  #:whole-body-readers? #t
  (require (submod ".." parser)))

(require (for-syntax racket/base
                     syntax/parse)
         syntax/parse/define
         (prefix-in tl: (submod teachlog lang)))

(begin-for-syntax
  (define-syntax-class term
    #:attributes (x)
    (pattern ((~datum term) (~or x:number x:string x:id)))
    (pattern ((~datum term) (~and (~datum quote) the-quote) e:id)
             #:attr x #'(the-quote e))
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
                     [tl:#%top-interaction #%top-interaction]
                     [tl:relation relation] [tl:data data]
                     [tl:? ?] [tl::- :-] [tl:next next])
         #%datum quote)
