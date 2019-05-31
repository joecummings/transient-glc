#lang racket

(require redex)
(require "tglc-def.rkt")

(provide T-to-S ▹ ∼ lookup-Γ ↝ extend-Γ fresh-l fresh-x fresh-f)

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
  [(lookup-Γ ((: x_1 T_1) ... (: x T) (: x_2 T_2) ... Γ) x) T]
  [(lookup-Γ ((: x T) (: x_1 T_1) ...  Γ) x) T]
  [(lookup-Γ ((: x_1 T_1) ... (: x T) Γ) x) T]

  [(lookup-Γ ((: x_1 T_1) ... (x T)) x) T]
  [(lookup-Γ any_1 any_2) ,(error 'lookup "not found: ~e in Γ ~e" (term any_2) (term any_1))])

(define-metafunction tglc
  extend-Γ : Γ (: x T) -> Γ
  [(extend-Γ · (: x T)) ((: x T) ·)]
  [(extend-Γ ((: x_1 T_1) ... ·) (: x T)) ((: x T) (: x_1 T_1) ... ·)])

(define-metafunction tglc
  fresh-l : n -> (n l)
  [(fresh-l n) (,(+ 1 (term n)) n)])

(define-metafunction tglc
  fresh-x : n -> (n x)
  [(fresh-x n) (,(+ 1 (term n))
                ,(string->symbol [~a "xx" (term n)]))])

(define-metafunction tglc
  fresh-f : n -> (n f)
  [(fresh-f n) (,(+ 1 (term n))
                ,(string->symbol [~a "ff" (term n)]))])

(define-metafunction tglc
  let-tglc : x e e -> e
  [(let-tglc x e_1 e_2) (app (fun f (x) e_2) e_1)])

(define-judgment-form tglc
  #:mode (↝ I I I O O O)
  #:contract (↝ Γ es n e T n)
  [
   --------------------------------- "naturals"
   (↝ Γ n n_0 n int n_0) ]
  
  [ (where/error T_1 (lookup-Γ Γ x))
   --------------------------------- "vars"
   (↝ Γ x n_0 x T_1 n_0)]
  
  [ (↝ Γ es n_0 e T n_1)
    -------------------------------- "refs"
    (↝ Γ (ref es) n_0 (ref e) (ref T) n_1)]
  
  [ (↝ Γ es_1 n_0 e_1 T_1 n_1) (∼ T_1 int) (where (n_2 l_2) (fresh-l n_1))
                       (↝ Γ es_2 n_2 e_2 T_2 n_3) (∼ T_2 int) (where (n_4 l_4) (fresh-l n_3))
    --------------------------------------------------------------------------------------- "addition"
    (↝ Γ (+ es_1 es_2) n_0 ( + (:: e_1 (⇒ l_2 T_1 int)) (:: e_2 (⇒ l_4 T_2 int))) int n_4)]
  
  [ (↝ (extend-Γ (extend-Γ Γ (: f (→ T_1 T_2))) (: x T_1)) es n_0 e_2 T_3 n_1) (∼ T_2 T_3)
   ---------------------------------------------------------------------------------------- "functions"
   (↝ Γ (→ (fun f (: x T_1)) (T_2 es)) n_0
      (fun f (x) (let-tglc x (⇓ x ((T-to-S T_1) f ARG)) e_2)) (→ T_1 T_2) n_1) ]

  [ (↝ Γ es_1 n_0 e_1 T n_1) (where (ref T_1) T) (where (n_2 l_2) (fresh-l n_1))
                             (↝ Γ es_2 n_2 e_2 T_2 n_3) (∼ T_1 T_2) (where (n_4 l_4) (fresh-l n_3))
   ---------------------------------------------------------------------------------------- "assignments"
   (↝ Γ (:= es_1 es_2) n_0
      (:= (:: e_1 (⇒ l_2 T (ref T_1))) (:: e_2 (⇒ l_4 T_2 T_1))) int n_4)]

  [ (↝ Γ es n_0 e T n_1) (where (ref T_1) T) (where (n_2 x_2) (fresh-x n_1)) (where (n_3 l_3) (fresh-l n_2))
   --------------------------------------------------------------------------------------- "derefs"
   (↝ Γ (! es) n_0
      (let-tglc x_2 (:: e (⇒ l_3 T (ref T_1))) (⇓ (! x_2) ((T-to-S T_1) x_2 DEREF))) T_1 n_3) ]

  [ (↝ Γ es_1 n_0 e_1 T n_1) (where (→ T_1 T_2) T) (where (n_2 f_2) (fresh-f n_1))
    (↝ Γ es_2 n_2 e_2 T_3 n_3) (∼ T_1 T_3) (where (n_4 l_4) (fresh-l n_3))
   ------------------------------------------------------------------------------- "apps"
   (↝ Γ (es_1 es_2) n_0
      (let-tglc f_2 (:: e_1 (⇒ l_4 T (→ T_1 T_2)))
                (⇓ (app f_2 (:: e_2 (⇒ l_4 T_3 T_1))) ((T-to-S T_2) f_2 RES))) T_2 n_4)]
  )

