#lang racket
(require redex)
(require "tglc-def.rkt" "blame.rkt" "types.rkt")

(provide lookup plus -→ replace extend fresh-a)

(default-language tglc)

(define-metafunction tglc
  cast : cast-e -> L
  [(cast (⇒ l * *)) *]
  [(cast (⇒ l int int)) (int ∈)]
  [(cast (⇒ l int *)) (int ∈)]
  [(cast (⇒ l * int)) (int l)]
  [(cast (⇒ l (→ T_1 T_2) (→ T_3 T_4))) (→ ∈ (cast (⇒ l T_3 T_1)) (cast (⇒ l T_2 T_4)))]
  [(cast (⇒ l (→ T_1 T_2) *)) (→ ∈ (cast (⇒ l * T_1)) (cast (⇒ l T_2 *)))]
  [(cast (⇒ l * (→ T_1 T_2))) (→ l (cast (⇒ l T_1 *)) (cast (⇒ l * T_2)))]
  [(cast (⇒ l (ref T_1) (ref T_2))) (ref ∈ (cast (⇔ l l T_2 T_1)))]
  [(cast (⇒ l (ref T_1) *)) (ref ∈ (cast (⇔ l l * T_1)))]
  [(cast (⇒ l (* (ref T_1)))) (ref l (cast (⇔ l T_1 *)))]
  [(cast (⇒ l (T_1 T_2))) (⊥ l)]

  [(cast (⇔ l * *)) *]
  [(cast (⇔ l int int)) (int ∈)]
  [(cast (⇔ l int *)) (int l)]
  [(cast (⇔ l * int)) (int l)]
  [(cast (⇔ l (→ T_1 T_2) (→ T_3 T_4))) (→ ∈ (cast (⇔ l T_3 T_1)) (cast (⇔ l T_2 T_4)))]
  [(cast (⇔ l (→ T_1 T_2) *)) (→ l (cast (⇔ l * T_1)) (cast (⇔ l T_2 *)))]
  [(cast (⇔ l * (→ T_1 T_2))) (→ l (cast (⇔ l T_1 *)) (cast (⇔ l * T_2)))]
  [(cast (⇔ l (ref T_1) (ref T_2))) (ref ∈ (cast (⇔ l l T_2 T_1)))]
  [(cast (⇔ l (ref T_1) *)) (ref l (cast (⇔ l l * T_1)))]
  [(cast (⇔ l (* (ref T_1)))) (ref l (cast (⇔ l T_1 *)))]
  [(cast (⇔ l (T_1 T_2))) (⊥ l)])

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
  #:contract (-→ (e σ β) ς) ; one state -> different state

  [(where a (fresh-a σ)) 
   -------------------------------------------------------------------------"fun"
   (-→ ((fun f (x) e) σ β) (a (extend σ (a (λ (x) (substitute e f a)))) β))]
  
  [(where/error (λ (x) e) (lookup σ a))
   ---------------------------------------------"app"
   (-→ ((app a v) σ β) ((substitute e x v) σ β))]

  [(where a (fresh-a σ))
   ------------------------------------------"new"
   (-→ ((ref v) σ β) (a (extend σ (a v)) β))]
  
  [(where/error v (lookup σ a)) 
   -----------------------------"deref"
   (-→ ((! a) σ β) (v σ β))]

  [(where/error v_1 (lookup σ a))
   --------------------------------------------"assign"
   (-→ ((:= a v) σ β) (0 (replace σ (a v)) β))]

  [(where n_3 (plus n_1 n_2))
   --------------------------------"add"
   (-→ ((+ n_1 n_2) σ β) (n_3 σ β))]

  [(where #t (hastype σ v T_2)) (where a v)
   -------------------------------------------------"cast succeed v = a"
   (-→ ((:: v (⇒ l T_1 T_2)) σ β) (v σ (ϱ β a (cast (⇒ l T_1 T_2)))))]

  [(where #t (hastype σ v T_2))
   ----------------------------------"cast succeed v != a"
   (-→ ((:: v (⇒ l T_1 T_2)) σ β) (v σ β))]
  
  [(where #f (hastype σ v T_2))
   ---------------------------------------"cast fail BLAME"
   (-→ ((:: v (⇒ l T_1 T_2)) σ β) (BLAME l))]

  [(where #t (hastype σ v S)) (where a_1 v)
   ---------------------------------------------"check succeed v = a"
   (-→ ((⇓ v (S a r)) σ β) (v σ (ϱ β a_1 (a r))))]

  [(where #t (hastype σ v S))
   ----------------------------------"check succeed v != a"
   (-→ ((⇓ v (S a r)) σ β) (v σ β))]
  
  [(where #f (hastype σ v S)) 
   -------------------------------------------"check fail blame"
   (-→ ((⇓ v (S a r)) σ β) ((blame σ v a r β)))]
  )

