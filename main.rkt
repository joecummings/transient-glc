#lang racket
(require redex)

(define-language tglc
  (e ::= x v (fun f (x) e) (e e) (+ e e) (ref e) (! e) (:= e e)) ; expr (incomplete)
  (x f ::= variable-not-otherwise-mentioned)
  (a ::= natural) ; address
  (σ ::= · (⟨a h⟩ σ)) ; heaps (incomplete)
  (h ::= (λ (x) e) v) ; heap values
  (β ::= · (⟨a b⟩ β)) ; blame sets (incomplete)
  (b ::= ⟨a r⟩ L) ; blame elems
  (T ::= int (→ T T) * (ref T)) ; types
  (L ::= intq (→q L L) (refq L) * ⊥l) ; labelled types 
  (v ::= a natural (λ (x) e)) ; values (including lambdas bc of bug)
  (E ::= hole (E e) (v E) (+ E e) (+ v E) (ref E) (! E) (:= E e) (⇒ ) ; E (incomplete)
  #:binding-forms
  (λ (f x) e #:refers-to x))) ;; not sure if this is correct

(default-language tglc)

(define-metafunction tglc
  lookup : σ a -> v or ⊥
  [(lookup (⟨a_1 h_1⟩ ... ⟨a h⟩ ⟨a_2 h_2⟩ ...) a)
   h]
  [(lookup any_1 any_2) ⊥])


(define-judgement-form tglc
  #:mode (→ I O)
  #:contract (→ ⟨e σ β⟩ ⟨e σ β⟩) ; one state -> different state

  [(where v_answer (lookup σ a)
    -------------------------------"deref"
    (→ ⟨e σ β⟩ ⟨v_answer σ β⟩))]

  )