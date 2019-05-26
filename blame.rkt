#lang racket
(require redex)
(require "tglc-def.rkt")

(provide extract label extend-β ρ blame)

(default-language tglc)

(define-metafunction tglc
  extract : r-bar L -> L
  [(extract · L) L]
  [(extract (RES ... r-bar_1) (→ q L_1 L_2)) (extract r-bar_1 L_2)]
  [(extract (ARG ... r-bar_1) (→ q L_1 L_2)) (extract r-bar_1 L_1)]
  [(extract (DEREF ... r-bar_1) (ref q L_1)) (extract r-bar_1 L_1)]
  [(extract (r ... r-bar_1) *) *])

(define-metafunction tglc
  label : L -> q
  [(label *) ∈]
  [(label (int q)) q]
  [(label (→ q L_1 L_2)) q]
  [(label (ref q)) q]
  [(label (⊥ l)) l])

;; updates address a in the blame map if present (have have multiple 'a' point to list of b's
;; QUESTION: this will just put all element in the list, instead of the union-set of the elements
(define-metafunction tglc
  extend-β : β (a b_4) -> β
  [(extend-β ((a_1 (b_1 ...)) ... (a (b_2 ...)) (a_3 (b_3 ...)) ... β) (a b_4))
   ((a_1 (b_1 ...)) ... (a ( b_2 ... b_4)) (a_3 (b_3 ...)) ... β)])

(define-metafunction tglc
  ρ : β a b -> β
  [(ρ β a_1 b) (extend-β β (a_1 b))])

(define-metafunction tglc
  blame : σ v a r β -> (e σ β)
  [(blame any_1 any_2 any_3 any_4 any_5) (e σ β)])