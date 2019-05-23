#lang racket
(require rackunit)
(require redex)

(define-language tglc
  (e ::= x v (fun f (x) e) (e e) (+ e e) (ref e) (! e) (:= e e) (:: e cast-e)) ; expr (incomplete)
  (cast-e := (⇒ T T) (⇔ T T))
  (x f ::= variable-not-otherwise-mentioned)
  (a ::= (addr natural)) ; address
  (σ ::= · ((a h) ... σ)) ; heaps (incomplete)
  (h ::= (λ (x) e) v) ; heap values
  (β ::= · ((a b) ... β)) ; blame sets (incomplete)
  (b ::= (a r) L) ; blame elems
  (T ::= int (→ T T) * (ref T)) ; types
  (L ::= (int q) (→ q L L) (ref q L) * (⊥ l)) ; labelled types
  (S ::= int → ref *) ; type tags
  (q ::= l ∈) ; labelled types 
  (v ::= a n) ; values
  (n ::= natural) ; naturals
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

(test-equal (term (lookup (((addr 0) (λ (x) 1)) ·) (addr 0))) (term (λ (x) 1)))
  
(test-equal (term (hastype (((addr 0) 42) ·) 38 *)) #t)
(test-equal (term (hastype (((addr 0) 42) ·) (addr 0) ref)) #t)
(test-equal (term (hastype (((addr 0) 42) ·) 42 ref)) #f)
(test-equal (term (hastype (((addr 0) (λ (x) x)) ·) (addr 0) →)) #t)


(test-equal (term (lookup (((addr 0) 1) ·) (addr 0))) 1)
(check-exn exn:fail? (λ () (term (lookup (((addr 0) (λ (x) x)) ((addr 1) 27) ·) (addr 3)))) "not found")

(define-metafunction tglc
  throw-on-lambda : h -> v
  [(throw-on-lambda v) v]
  [(throw-on-lambda any_1) ,(error 'throw-on-lambda "found a lambda: ~e" (term any_1))])

(test-equal (term (throw-on-lambda 1)) 1)
(check-exn exn:fail? (λ () (term (throw-on-lambda (λ (x) x)))) "found a lambda")

(define-metafunction tglc
  plus : n_1 n_2 -> n
  [(plus n_1 n_2) ,(+ (term n_1) (term n_2))])

(test-equal (term (plus 1 2)) 3)
(test-equal (term (plus 0 0)) 0)

(define-metafunction tglc
  replace : σ (a v_1) -> σ
  [(replace ((a_1 h_1) ... (a v) (a_2 h_2) ... σ) (a v_1)) ((a_1 h_1) ... (a v_1) (a_2 h_2) ... σ)]
  [(replace (any_1 (any_2 v))) ,(error 'replace "address not found: ~e" (term any_2))]
  [(replace (any_1 (any_2 h))) ,(error ('replace "expected a value, given: ~e" (term h)))])

(test-equal (term (replace ((0 1) ·) (0 0))) (term ((0 0) ·)))
(check-exn exn:fail? (λ () (term (replace ((0 1) ·) (1 1)))) "address not found")
(check-exn exn:fail? (λ () (term (replace ((0 1) ·) (1 (λ (x) x))))) "expected a value")

(define-judgment-form tglc
  #:mode (→ I O)
  #:contract (→ (e σ) (e σ)) ; one state -> different state

  [(where v_answer (throw-on-lambda (lookup σ a))) 
   ------------------------------------------------"deref"
   (→ ((! a) σ) (v_answer σ))]

  [(where v_prime (throw-on-lambda (lookup σ a)))
   -----------------------------------------------"assign"
   (→ ((:= a v) σ) (0 (replace σ (a v))))]

  [(where n_prime (plus n_1 n_2))
   --------------------------------"add"
   (→ ((+ n_1 n_2) σ) (n_prime σ))]

  )

(test-judgment-holds
   (→ ((! (addr 1)) (((addr 0) (λ (x) x)) ((addr 1) 27) ·))
      (27 (((addr 0) (λ (x) x)) ((addr 1) 27) ·))))
(test-judgment-holds
   (→ ((+ 1 2) ·) (3 ·)))
(test-judgment-holds
   (→ ((:= 0 3) ((0 0) ·)) (0 ((0 3) ·))))







