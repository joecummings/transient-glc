#lang racket

(require redex)
(require "tglc-def.rkt")

(provide T-to-S ▹ ∼ lookup-Γ ↝ fresh-l)

(default-language tglc)

(define-metafunction tglc
  T-to-S : T -> S
  [(T-to-S *) *]
  [(T-to-S int) int]
  [(T-to-S (→ T_1 T_2)) →]
  [(T-to-S (ref T)) ref])

(define-judgment-form tglc
  #:mode (▹ I I)
  #:contract (▹ T T) 
  [
   -------------------- "ref"
   (▹ (ref T) (ref T))]
  [
   -------------- "ref *"
   (▹ * (ref *))]
  [
   ---------------------------- "func"
   (▹ (→ T_1 T_2) (→ T_1 T_2))]
  [
   --------------- "func *"
   (▹ * (→ * *))])

(define-judgment-form tglc
  #:mode (∼ I I)
  #:contract (∼ T T)
  [
   ----------- "∼ int"
   (∼ int int)]
  [
   -------- "* T"
   (∼ * T)]
  [
   -------- "T *"
   (∼ T *)]
  [(∼ T_1 T_2)
   ------------------------ "ref"
   (∼ (ref T_1) (ref T_2))]
  [(∼ T_1 T_3) (∼ T_2 T_4)
   ------------------------ "func"
   (∼ (→ T_1 T_2) (→ T_3 T_4))])

(define-metafunction tglc
  lookup-Γ : Γ x -> T
  [(lookup-Γ ((: x T) (: x_1 T_1) ...  Γ) x) T]
  [(lookup-Γ ((: x_1 T_1) ... (: x T) (: x_2 T_2) Γ) x) T]
  [(lookup-Γ any_1 any_2) ,(error 'lookup "not found: ~e" (term any_1))])

(define-metafunction tglc
  fresh-l : l -> l
  [(fresh-l n) ,(+ 1 (term n))])

(define-judgment-form tglc
  #:mode (↝ I I I O O O)
  #:contract (↝ Γ es l e T l_2)
  [
   ----------------------- "naturals"
   (↝ Γ n l n int l) ]
  
  [ (where/error T_1 (lookup-Γ Γ x))
   --------------------------- "vars"
   (↝ Γ x l x T_1 l)]
  
  [ (↝ Γ es l e T l_1)
    -------------------- "refs"
    (↝ Γ (ref es) l (ref e) (ref T) l_1)]
  
  [ (↝ Γ es_1 l e_1 T_1 l_1) (∼ T_1 int) (where l_2 (fresh-a l_1))
                       (↝ Γ es_2 l_2 e_2 T_2 l_3) (∼ T_2 int) (where l_4 (fresh-a l_3))
    --------------------------------------------------------------------------------- "addition"
    (↝ Γ (+ es_1 es_2) l ( + (:: e_1 (⇒ l_2 T_1 int)) (:: e_2 (⇒ l_4 T_2 int))) int l_4)]
  )



