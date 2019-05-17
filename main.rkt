#lang racket
(require redex)

(define-language tglc
  (e ::= x v (λ (f x) e) (e e) (+ e e) (ref e) (! e) (:= e e))
  (x f ::= variable-not-otherwise-mentioned)
  (a ::= integer) ;address
  (v ::= a integer) 
  (E ::= hole (E e) (v E) (+ E e) (+ v E) (ref E) (! E) (:= E e))
  #:binding-forms
  (λ (f x) e #:refers-to x))
(default-language tglc)

(test-equal (redex-match? tglc e (term y)) #t)
(test-equal (redex-match? tglc e (term (λ (coolfunction y) (y 1)))) #t)
(test-equal (redex-match? tglc e (term (+ 2 1))) #t)

(test-results)