#lang racket
(require redex)

(define-language tglc
  (e ::= x v (fun f (x) e) (e e) (+ e e) (ref e) (! e) (:= e e)) ; expr (incomplete)
  (x f ::= variable-not-otherwise-mentioned)
  (a ::= integer) ; address
<<<<<<< Updated upstream
  (σ ::= · (↦ a h)) ; heaps (incomplete)
  (h ::= (λ (x) e) v) ; heap values
  (β ::= · (↦ a h)) ; blame sets (incomplete)
  (b ::= ⟨a r⟩ L) ; blame elems (incomplete)
  (L ::= *) ; labelled types (incomplete)
=======
  
  (σ ::= · (↦ a h)) ; heaps (incomplete)
  (h ::= (λ (x) e) v) ; heap values
  
  (β ::= · (↦ a h)) ; blame sets (incomplete)
  (b ::= L) ; blame elems (incomplete)

  (T ::= int (→ T T) * (ref T)) ; types
  (L ::= int∈ (→q L L) (refq L) * ⊥l) ; labelled types 

  
>>>>>>> Stashed changes
  (v ::= a integer) ; values
  (E ::= hole (E e) (v E) (+ E e) (+ v E) (ref E) (! E) (:= E e) (⇒ ) ; E (incomplete)
  #:binding-forms
  (λ (f x) e #:refers-to x))) ;; not sure if this is correct





(default-language tglc)

(module+ test
  (test-equal (redex-match? tglc e (term y)) #t)
  (test-equal (redex-match? tglc e (term 123)) #t)
  (test-equal (redex-match? tglc e (term (fun coolfunction (y) y))) #t)
  (test-equal (redex-match? tglc e (term (x x))) #t)
  (test-equal (redex-match? tglc e (term (ref x))) #t)
  (test-equal (redex-match? tglc e (term (! x))) #t)
  (test-equal (redex-match? tglc e (term (:= x y))) #t)
  (test-equal (redex-match? tglc e (term (+ 2 1))) #t))

(module+ test
  (test-equal (redex-match? tglc a 123123123) #t)
  (test-equal (redex-match? tglc v 123123) #t))

(module+ test
  (test-results))