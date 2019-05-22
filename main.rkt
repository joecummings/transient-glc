#lang racket
(require redex)

(define-language tglc
  (e ::= x v (fun f (x) e) (e e) (+ e e) (ref e) (! e) (:= e e) (:: e cast-e)) ; expr (incomplete)
  (cast-e := (⇒ T T) (⇔ T T))
  (x f ::= variable-not-otherwise-mentioned)
  (a ::= natural) ; address
  (σ ::= · (⟨a h⟩ ...)) ; heaps (incomplete)
  (h ::= (λ (x) e) v) ; heap values
  (β ::= · (⟨a b⟩ β)) ; blame sets (incomplete)
  (b ::= ⟨a r⟩ L) ; blame elems
  (T ::= int (→ T T) * (ref T)) ; types
  (L ::= (int q) (→ q L L) (ref q L) * (⊥ l)) ; labelled types
  (S ::= int → ref *) ; type tags
  (q ::= l ∈)
  (v ::= a natural (λ (x) e)) ; values (including lambdas bc of bug)
  (E ::= hole (E e) (v E) (+ E e) (+ v E) (ref E) (! E) (:= E e) (⇒ ) ; E (incomplete)
  #:binding-forms
  (λ (f x) e #:refers-to x))) ;; not sure if this is correct

(default-language tglc)

(define-metafunction tglc
  cast : cast-e -> L
  [(cast (⇒ * *)) *]
  [(cast (⇒ int int)) (int ∈)]
  [(cast (⇒ int *)) (int ∈)]
  [(cast (⇒ * int)) (int l)]
  [(cast (⇒ (→ T_1 T_2) (→ T_3 T_4))) (→ ∈ (cast (⇒ T_3 T_1)) (cast (⇒ T_2 T_4)))]
  [(cast (⇒ (→ T_1 T_2) *)) (→ ∈ (cast (⇒ * T_1)) (cast (⇒ T_2 *)))]
  [(cast (⇒ * (→ T_1 T_2))) (→ l (cast (⇒ T_1 *)) (cast (⇒ * T_2)))]
  [(cast (⇒ (ref T_1) (ref T_2))) (ref ∈ (cast (⇔ T_1 T_1)))]
  [(cast (⇒ (ref T_1 *))) (ref ∈ (cast (⇔ * T_1)))]
  [(cast (⇒ (* (ref T_1)))) (ref l (cast (⇔ T_1 *)))]
  [(cast (⇒ (T_1 T_2))) (⊥ l)])

(define-metafunction tglc
  hastype : σ v S -> #t or #f)

(define-judgement-form tglc
  #:mode (→ I O)
  #:contract (→ (e σ β) (e σ β)) ; one state -> different state

  

#;
(define-metafunction tglc
  lookup : σ a -> v or ⊥
  [(lookup (⟨a_1 h_1⟩ ... ⟨a h⟩ ⟨a_2 h_2⟩ ...) a) h]
  [(lookup any_1 any_2) ⊥])

#;
(define-judgment-form tglc
  #:mode (→ I O)
  #:contract (→ (e σ β) (e σ β)) ; one state -> different state

  [(where v_answer (lookup σ a)
    -------------------------------"deref"
    (→ ⟨e σ β⟩ ⟨v_answer σ β⟩))]

  )