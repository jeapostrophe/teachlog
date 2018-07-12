#lang racket/base

(module reader syntax/module-reader
  teachlog/ugly
  #:read my-read
  #:read-syntax my-read-syntax
  #:whole-body-readers? #t
  (require teachlog/grammar
           brag/support)

  (define (tokenize ip)
    (port-count-lines! ip)
    (define my-lexer
      (lexer-src-pos
       [(repetition 1 +inf.0 numeric)
        (token 'NUMBER (string->number lexeme))]
       ["rel" (token 'REL)]
       ["data" (token 'DATA)]
       ["next" (token 'NEXT)]
       ["(" (token 'LPAREN)]
       [")" (token 'RPAREN)]
       ["/" (token 'SLASH)]
       ["." (token 'DOT)]
       ["," (token 'COMMA)]
       ["'" (token 'QUOTE)]
       [":-" (token 'IMPLIED-BY)]
       ["?" (token 'QMARK)]
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
    (list (parse src (tokenize ip)))))

(require (for-syntax racket/base
                     syntax/parse)
         (prefix-in tl: (submod teachlog lang)))

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
    (pattern ((~datum rel) r:id n:nat)
             #:attr x #'(tl:relation r n))
    (pattern ((~datum data) d:id n:nat)
             #:attr x #'(tl:data d n))
    (pattern ((~datum data) d:id)
             #:attr x #'(tl:data d))
    (pattern ((~datum fact) c:clause)
             #:attr x #'(tl::- c.x))
    (pattern ((~datum rule) h:clause b:clause ...)
             #:attr x #'(tl::- h.x b.x ...))
    (pattern ((~datum query) c:clause)
             #:attr x #'(tl:? c.x))
    (pattern ((~datum next))
             #:attr x #'(tl:next))))

(define-syntax (umbegin stx)
  (syntax-parse stx
    [(_ ((~datum program) e:stmt ...))
     #'(tl:#%module-begin e.x ...)]))

(provide (rename-out [umbegin #%module-begin])
         #%datum)
