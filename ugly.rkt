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

(require syntax/parse/define
         (prefix-in tl: (submod teachlog lang)))

(define-simple-macro (fact thy c)
  (tl::- thy c))
(define-simple-macro (rule thy h the-bar b ...)
  (the-bar thy h b ...))
(define-simple-macro (query thy c the-q)
  (the-q thy c))

(provide (rename-out [tl:#%module-begin #%module-begin]
                     [tl:#%top-interaction #%top-interaction]
                     [tl:relation relation] [tl:data data]
                     [tl:? ?] [tl::- :-] [tl:next next])
         #%datum quote
         fact rule query)
