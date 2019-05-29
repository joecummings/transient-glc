#lang racket

(require redex)
(require "tglc-def.rkt" "main.rkt" "blame.rkt")

(provide T-to-S ▹ ∼ lookup-Γ ↝ extend-Γ fresh-l)

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
  [(lookup-Γ ((: x_1 T_1) ... (: x T) Γ) x) T]
  [(lookup-Γ ((: x_1 T_1) ... (: x T) (: x_2 T_2) Γ) x) T]
  [(lookup-Γ ((: x_1 T_1) ... (x T)) x) T]
  [(lookup-Γ any_1 any_2) ,(error 'lookup "not found: ~e in Γ ~e" (term any_2) (term any_1))])

(define-metafunction tglc
  extend-Γ : Γ (: x T) -> Γ
  [(extend-Γ ((: x_1 T_1) ... ·) (: x T)) ((: x T) (: x_1 T_1) ... ·)])

(define-metafunction tglc
  fresh-l : n -> n
  [(fresh-l n) ,(+ 1 (term n))])


(define-metafunction tglc
  let-tglc : x e e -> e
  [(let-tglc x e_1 e_2) (app (fun f (x) e_2) e_1)])

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
  
  [ (↝ Γ es_1 l e_1 T_1 l_1) (∼ T_1 int) (where l_2 (fresh-l l_1))
                       (↝ Γ es_2 l_2 e_2 T_2 l_3) (∼ T_2 int) (where l_4 (fresh-l l_3))
    --------------------------------------------------------------------------------- "addition"
    (↝ Γ (+ es_1 es_2) l ( + (:: e_1 (⇒ l_2 T_1 int)) (:: e_2 (⇒ l_4 T_2 int))) int l_4)]
  
  [ (↝ (extend-Γ (extend-Γ Γ (: f (→ T_1 T_2))) (: x T_1)) es l e_2 T_3 l_1) (∼ T_2 T_3)
   ------------------------------------------------------------------------------------ "functions"
   (↝ Γ (→ (fun f (: x T_1)) (T_2 es)) l
      (fun f (x) (let-tglc x (⇓ x ((T-to-S T_1) f ARG)) e_2)) (→ T_1 T_2) l_1) ]

  ;[ (↝ Γ es_1 l_? e_1 T l_?) (▹ T (ref T_1)) (where l_? (fresh-l l_?))
  ;                           (↝ Γ es_2 l_? e_2 T_2 l_?) (∼ T_1 T_2) (where l_? (fresh-l l_?))
  ; ---------------------------------------------------------------------------------------- "assignments"
  ; (↝ Γ (:= es_1 es_2) l_?
  ;    (:= (:: e_1 (⇒ l_? T (ref T_1))) (:: e_2 (⇒ l_? T_2 T_1))) int l_?)]

  ;[ (↝ Γ es l_? e T l_?) (▹ T (ref T_1)) (where x_1 (fresh x)) (where l_? (fresh l_?))
  ; --------------------------------------------------------------------------------------- "derefs"
  ; (↝ Γ (! es) l_? (let-tglc x (:: e (⇒ l_? T (ref T_1))) (⇓ (! x) (T_1 x DEREF))) T_1 l_?) ]

  ;[ (↝ Γ es_1 l_? e_1 T l_?) (▹ T (→ T_1 T_2)) (where f_1 (fresh f))
  ;  (↝ Γ es_2 l_? e_2 T_3 l_?) (∼ T_1 T_3) (where l_? (fresh-l l_?))
  ; ------------------------------------------------------------------------------- "apps"
  ; (↝ Γ (app es_1 es_2) l_?
  ;    (let-tglc f_1 (:: e_1 (⇒ l_? T (→ T_1 T_2)))
  ;              (app f_1 (⇓ (:: e_2 (⇒ l_? T_3 T_1)) (T_2 f_1 RES)))) T_2 l_?)]
  )



