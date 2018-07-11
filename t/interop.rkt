#lang racket/base
(require teachlog)

(relation parent 2)
(define ft
  (teachlog
   (:- (parent "maekar" "aegon-5"))
   (:- (parent "aegon-5" "aerys-2"))
   (:- (parent "aerys-2" "viserys"))
   (:- (parent "aerys-2" "daenerys"))
   (:- (parent "daenerys" #,(string-append "dro" "gon")))))

(parent "ed" "sansa")

(module+ main
  (teachlog #:theory ft
            (? (parent "daenerys" Y))
            (next)))
