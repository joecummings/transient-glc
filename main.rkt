#lang racket
(require redex)
(require rackunit)

(define-language tglc
  (e ::= x v (fun f (x) e) (e e) (+ e e) (ref e) (! e) (:= e e) (:: e cast-e)) ; expr (incomplete)
  (cast-e := (⇒ T T) (⇔ T T))
  (x f ::= variable-not-otherwise-mentioned)
  (a ::= natural) ; address
  (σ ::= · ((a h) ... σ)) ; heaps (incomplete)
  (h ::= (λ (x) e) v) ; heap values
  (β ::= · ((a b) ... β)) ; blame sets (incomplete)
  (b ::= (a r) L) ; blame elems
  (T ::= int (→ T T) * (ref T)) ; types
  (L ::= (int q) (→ q L L) (ref q L) * (⊥ l)) ; labelled types
  (S ::= int → ref *) ; type tags
  (q ::= l ∈) ; labelled types 
  (v ::= a n) ; values
  (n ::= natural)
  (E ::= hole (E e) (v E) (+ E e) (+ v E) (ref E) (! E) (:= E e) (⇒ ) ; E (incomplete)
  #:binding-forms
  (λ (f x) e #:refers-to x))) ;; not sure if this is correct

(default-language tglc)

#;
(define-metafunction tglc
  cast : cast-e -> L
  [(cast (⇒ * *)) *]
  [(cast (⇒ int int)) (int ∈)]
  [(cast (⇒ int *)) (int ∈)]
  [(cast (⇒ * int)) (int l)]
  [(cast (⇒ (→ T_1 T_2) (→ T_3 T_4))) (→ ∈ (⇒ T_3 T_1) (⇒ T_2 T_4))]
  [(cast (⇒ (→ T_1 T_2) *)) (→ ∈ (⇒ * T_1) (⇒ T_2 *))]
  [(cast (⇒ * (→ T_1 T_2))) (→ l (⇒ T_1 *)  (⇒ * T_2))]
  [(cast (⇒ (ref T_1) (ref T_2))) (ref ∈ (⇔ T_1 T_1))]
  [(cast (⇒ (ref T_1 *))) (ref ∈ (⇔ * T_1))]
  [(cast (⇒ (* (ref T_1)))) (ref l (⇔ T_1 *))]
  [(cast (⇒ (any_1 any_2))) (⊥ l)])

(define-metafunction tglc
  hastype : σ v S -> #t or #f
  [(hastype (σ n int)) #t]
  [(hastype (σ a int)) #f]
  [(hastype (σ v *)) #t]
  [(hastype (σ n →)) #f]
  [(hastype (σ a →)) ,(if (term (value? (lookup σ a))) #f #t)]
  [(hastype (σ n ref)) #f]
  [(hastype (σ a ref)) ,(if (term (value? (lookup σ a))) #t #f)])

(test-equal (term (hastype ((0 42) ·) 1 int)) #t)

(define-metafunction tglc
  lookup : σ a (#t or #f) -> v
  [(lookup ((a_1 h_1) ... (a v) (a_2 h_2) ... σ) a #t) v]
  [(lookup ((a_1 h_1) ... (a h) (a_2 h_2) ... σ) a #f) h]
  [(lookup ((a_1 h_1) ... (a h) (a_2 h_2) ... σ) a #t) ,(error 'lookup "not a value: ~e" (term a))]
  [(lookup any_1 any_2 any_3) ,(error 'lookup "not found: ~e" (term any_1))])

(define-judgment-form tglc
  #:mode (→ I O)
  #:contract (→ (e σ) (e σ)) ; one state -> different state


  [(where v_answer (lookup σ a #t))
    -------------------------------"deref"
    (→ ((! a) σ) (v_answer σ))]

  )

(test-judgment-holds
 (→ ((! 1) ((0 (λ (x) x)) (1 27) ·)) (27 ((0 (λ (x) x)) (1 27) ·))))

;(check-exn (term (lookup ((0 (λ (x) x)) (1 27) ·) 0)))





