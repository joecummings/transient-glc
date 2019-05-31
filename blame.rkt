#lang racket
(require redex)
(require "tglc-def.rkt" "types.rkt" "translate.rkt")

(provide extract label extend-β ϱ blame resolve collect-blame lookup-β L-to-T)

(default-language tglc)

(define-metafunction tglc
  extract : r ... L -> L
  [(extract L) L]
  [(extract RES r_1 ... (→ q L_1 L_2)) (extract r_1 ... L_2)]
  [(extract ARG r_1 ... (→ q L_1 L_2)) (extract r_1 ... L_1)]
  [(extract DEREF r_1 ... (ref q L_1)) (extract r_1 ... L_1)]
  [(extract r_1 r_2 ... *) *])

(define-metafunction tglc
  label : L -> q
  [(label *) ∈]
  [(label (int q)) q]
  [(label (→ q L_1 L_2)) q]
  [(label (ref q L_1)) q]
  [(label (⊥ l)) l])

(define-metafunction tglc
  lookup-β : β a -> (b ...)
  [(lookup-β ((a_1 b_1 ...) ... (a b ...) (a_2 b_2 ...) ... β) a) (b ...)]
  [(lookup-β any_1 any_2) ,(error 'lookup-β "not found: ~e" (term any_2))])

(define-metafunction tglc
  collect-blame : r ... β b -> (L ...)
  [(collect-blame r_1 ... any_2 L)
    (L_1)
    (where L_1 (extract r_1 ... L)) (where l (label L_1))]
  [(collect-blame r_1 ... any_2 L)
    ()
    (where L_1 (extract r_1 ... L)) (where ∈ (label L_1))]
  [(collect-blame r_1 ... any_2 (a r))
    ,(set-union
      (first
        (term ((collect-blame r r_1 ... any_2 b_1) ...))))
    (where (b_1 ...) (lookup-β any_2 a))])

(define-metafunction tglc
  L-to-T : L -> T
  [(L-to-T *) *]
  [(L-to-T (int q)) int]
  [(L-to-T (ref q L_1)) (ref T_1)
    (where T_1 (L-to-T L_1))]
  [(L-to-T (→ q L_1 L_2)) (→ T_1 T_2)
    (where T_1 (L-to-T L_1)) (where T_2 (L-to-T L_2))])

(define-metafunction tglc
  resolve : σ v L ... -> weird-L
  [(resolve any_1 any_2 (⊥ l) L_1 ...) 
    (l (resolve any_1 any_2 L_1 ...))]
  [(resolve any_1 any_2 L_1 L_2 ...) 
    ((label L_1) (resolve any_1 any_2 L_2 ...))
    (where #f (hastype any_1 any_2 (T-to-S (L-to-T L_1))))]
  [(resolve any_1 any_2 L_1 L_2 ...) 
    ((label L_1) (resolve any_1 any_2 L_2 ...))]
  [(resolve any_1 any_2) ·])

;; updates address a in the blame map if present (have have multiple 'a' point to list of b's
;; QUESTION: this will just put all element in the list, instead of the union-set of the elements
(define-metafunction tglc
  extend-β : β (a b_4) -> β
  [(extend-β ((a_1 b_1 ...) ... (a b_2 ...) (a_3 b_3 ...) ... β) (a b_4))
   ((a_1 b_1 ...) ... (a b_2 ... b_4) (a_3 b_3 ...) ... β)])

(define-metafunction tglc
  ϱ : β a b -> β
  [(ϱ β a_1 b) (extend-β β (a_1 b))])

(define-metafunction tglc
  blame : σ v a r β -> ς
  [(blame any_1 any_2 any_3 any_4 any_5) (BLAME weird-L)
    (where (b_1 ...) (lookup-β any_5 any_3))
    (where (L_1 ...) 
      ,(set-union
        (first
          (term ((collect-blame any_4 any_5 b_1) ...)))))
    (where weird-L (resolve any_1 any_2 L_1 ...))])

    