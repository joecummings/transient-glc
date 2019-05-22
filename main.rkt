#lang racket
(require rackunit)
(require redex)

(define-language tglc
  (e ::= x v (fun f (x) e) (e e) (+ e e) (ref e) (! e) (:= e e)) ; expr (incomplete)
  (x f ::= variable-not-otherwise-mentioned)
  (a ::= natural) ; address
  (σ ::= · ((a h) ... σ)) ; heaps (incomplete)
  (h ::= (λ (x) e) v) ; heap values
  (β ::= · ((a b) ... β)) ; blame sets (incomplete)
  (b ::= (a r) L) ; blame elems
  (T ::= int (→ T T) * (ref T)) ; types
  (L ::= intq (→q L L) (refq L) * ⊥l) ; labelled types 
  (v ::= a natural) ; values
  (E ::= hole (E e) (v E) (+ E e) (+ v E) (ref E) (! E) (:= E e) (⇒ ) ; E (incomplete)
     #:binding-forms
     (λ (f x) e #:refers-to x))) ;; not sure if this is correct

(default-language tglc)

(define-metafunction tglc
  lookup : σ a -> v
  [(lookup ((a_1 h_1) ... (a v) (a_2 h_2) ... σ) a) v]
  [(lookup ((a_1 h_1) ... (a h) (a_2 h_2) ... σ) a) ,(error 'lookup "not a value: ~e" (term a))]
  [(lookup any_1 any_2) ,(error 'lookup "not found: ~e" (term any_1))])

(test-equal (term (lookup ((0 1) ·) 0)) 1)
(check-exn exn:fail? (λ () (term (lookup ((0 (λ (x) x)) (1 27) ·) 0))) "not a value")
(check-exn exn:fail? (λ () (term (lookup ((0 (λ (x) x)) (1 27) ·) 3))) "not found")

(define-judgment-form tglc
  #:mode (→ I O)
  #:contract (→ (e σ) (e σ)) ; one state -> different state

  [(where v_answer (lookup σ a))
   -------------------------------"deref"
   (→ ((! a) σ) (v_answer σ))]

  )

(test-judgment-holds
   (→ ((! 1) ((0 (λ (x) x)) (1 27) ·)) (27 ((0 (λ (x) x)) (1 27) ·))))







