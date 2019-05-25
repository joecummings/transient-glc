#lang racket
(require rackunit)
(require redex)

(provide tglc lookup hastype plus -→
         replace extend fresh-a)

(define-language tglc
  (e ::= x v (fun f (x) e) (app e e) (+ e e) (ref e) (! e) (:= e e) (:: e cast-e) (⇓ e (S e r))) ; expr
  (cast-e := (⇒ T T) (⇔ T T))
  (v ::= a n) ; values
  (x y f ::= variable-not-otherwise-mentioned)

  (a ::= (addr natural)) ; address
  (σ ::= · ((a h) ... σ)) ; heaps
  (h ::= (λ (x) e) v) ; heap values
  
  (β ::= · ((a b) ... β)) ; blame sets
  (b ::= (a r) L) ; blame elems
  (l ::= natural) ; blame labels

  (T ::= int (→ T T) * (ref T)) ; types
  (L ::= (int q) (→ q L L) (ref q L) * (⊥ l)) ; labelled types
  (S ::= int → ref *) ; type tags
  (q ::= l ∈) ; optional labels
  (r ::= RES ARG DEREF) ; tags
 
  (n ::= natural) ; naturals
  (E ::= hole (app E e) (app v E) (+ E e) (+ v E) (ref E) (! E) (:= E e) (:: E cast-e) (⇓ E (S e r)) (⇓ v (S E r)))  ; E 
  #:binding-forms
  (λ (x) e #:refers-to x))

(default-language tglc)

(define-metafunction tglc
  cast : cast-e -> L
  [(cast (⇒ * *)) *]
  [(cast (⇒ int int)) (int ∈)]
  [(cast (⇒ int *)) (int ∈)]
  [(cast (⇒ * int)) (int l)]
  [(cast (⇒ (→ T_1 T_2) (→ T_3 T_4))) (→ ∈ (cast (⇒ T_3 T_1)) (cast (⇒ T_2 T_4)))]
  [(cast (⇒ (→ T_1 T_2) *)) (→ ∈ (cast (⇒ * T_1)) (cast (⇒ T_2 *)))]
  [(cast (⇒ * (→ T_1 T_2))) (→ (⇒ T_1 *)  (⇒ * T_2))]
  [(cast (⇒ (ref T_1) (ref T_2))) (ref ∈ (⇔ T_1 T_1))]
  [(cast (⇒ (ref T_1 *))) (ref ∈ (⇔ * T_1))]
  [(cast (⇒ (* (ref T_1)))) (ref l (⇔ T_1 *))]
  [(cast (⇒ (any_1 any_2))) (⊥ l)])

(define-metafunction tglc
  value? : h -> #t or #f
  [(value? v) #t]
  [(value? h) #f])

(define-metafunction tglc
  lookup : σ a -> h
  [(lookup ((a_1 h_1) ... (a h) (a_2 h_2) ... σ) a) h]
  [(lookup any_1 any_2) ,(error 'lookup "not found: ~e" (term any_1))])

(define-metafunction tglc
  hastype : σ v S -> #t or #f
  [(hastype σ n int) #t]
  [(hastype σ a int) #f]
  [(hastype σ a *) #t]
  [(hastype σ n *) #t]
  [(hastype σ a →) ,(if (term (value? (lookup σ a))) #f #t)]
  [(hastype σ n →) #f]
  [(hastype σ a ref) ,(if (term (value? (lookup σ a))) #t #f)]
  [(hastype σ n ref) #f])

(define-metafunction tglc
  plus : n_1 n_2 -> n
  [(plus n_1 n_2) ,(+ (term n_1) (term n_2))])

(define-metafunction tglc
  replace : σ (a v_1) -> σ
  [(replace ((a_1 h_1) ... (a v) (a_2 h_2) ... σ) (a v_1)) ((a_1 h_1) ... (a v_1) (a_2 h_2) ... σ)]
  [(replace (any_1 (any_2 v))) ,(error 'replace "address not found: ~e" (term any_2))]
  [(replace (any_1 (any_2 h))) ,(error ('replace "expected a value, given: ~e" (term h)))])

(define-metafunction tglc
  add-to-address : a -> a
  [(add-to-address (addr n)) (addr (plus n 1))])

(define-metafunction tglc
  fresh-a : σ -> a
  [(fresh-a ((a h) (a_1 h_1) ... ·)) (add-to-address a)])

(define-metafunction tglc
  extend : σ (a h) -> σ
  [(extend ((a_1 h_1) ... ·) (a h)) ((a h) (a_1 h_1) ... ·)])

(define-judgment-form tglc
  #:mode (-→ I O)
  #:contract (-→ (e σ) (e σ)) ; one state -> different state

  [(where a (fresh-a σ)) 
   -------------------------------------------- "fun"
   (-→ ((fun f (x) e) σ) (a (extend σ (a (λ (x) (substitute e f a))))))]
  
  [(where/error (λ (x) e) (lookup σ a))
   ------------------------------------"app"
   (-→ ((app a v) σ) ((substitute e x v) σ))]

  [(where a (fresh-a σ))
   ----------------------------- "new"
   (-→ ((ref v) σ) (a (extend σ (a v))))]
  
  [(where/error v (lookup σ a)) 
   -----------------------------"deref"
   (-→ ((! a) σ) (v σ))]

  [(where/error v_1 (lookup σ a))
   ----------------------------------------"assign"
   (-→ ((:= a v) σ) (0 (replace σ (a v))))]

  [(where n_prime (plus n_1 n_2))
   --------------------------------"add"
   (-→ ((+ n_1 n_2) σ) (n_prime σ))]

  [(where #t (hastype σ v T_2)) ;; missing blame and (where v = a)
   ----------------------------------""
   (-→ ((:: v (⇒ T_1 T_2)) σ) (v σ))]

  )