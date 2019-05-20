#lang racket
(require redex)

(define count 0)

(define-language tglc
  (e ::= x v (fun f (x) e) (e e) (+ e e) (ref e) (! e) (:= e e)) ; expr (incomplete)
  (x f ::= variable-not-otherwise-mentioned)
  (a ::= natural) ; address
  (σ ::= · ⟨(↦ a h) σ⟩) ; heaps (incomplete)
  (h ::= (λ (x) e) v) ; heap values
  (β ::= · ⟨(↦ a b) β⟩) ; blame sets (incomplete)
  (b ::= ⟨a r⟩ L) ; blame elems
  (T ::= int (→ T T) * (ref T)) ; types
  (L ::= intq (→q L L) (refq L) * ⊥l) ; labelled types 
  (v ::= a natural) ; values
  (E ::= hole (E e) (v E) (+ E e) (+ v E) (ref E) (! E) (:= E e) (⇒ ) ; E (incomplete)
  #:binding-forms
  (λ (f x) e #:refers-to x))) ;; not sure if this is correct

(default-language tglc)

(define-metafunction tglc
  \delta : σ a -> h or \bottom)
  [(\delta σ a (if (< a count) h \bottom))] ; metafunction for mapsto

(define-judgement-form tglc
  #:mode (→ I O)
  #:contract (→ ⟨e σ β⟩ ⟨e σ β⟩) ; one state -> different state

  [( where v_answer (\delta σ a) ((\delta σ a) v)
    -------------- "deref"
    (→ ⟨e σ β⟩ ⟨v_answer σ β⟩))]

  )

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