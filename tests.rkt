#lang racket
(require rackunit)
(require redex)
(require "tglc-def.rkt" "main.rkt" "blame.rkt" "translate.rkt" "types.rkt")

(module+ test
  (test-equal (redex-match? tglc e (term y)) #t)
  (test-equal (redex-match? tglc e (term 123)) #t)
  (test-equal (redex-match? tglc e (term (fun coolfunction (y) y))) #t)
  (test-equal (redex-match? tglc e (term (ref x))) #t)
  (test-equal (redex-match? tglc e (term (! x))) #t)
  (test-equal (redex-match? tglc e (term (:= x y))) #t)
  (test-equal (redex-match? tglc e (term (+ 2 1))) #t))

(module+ test
  (test-equal (redex-match? tglc a (term (addr 123123123))) #t)
  (test-equal (redex-match? tglc v 123123) #t)
  (test-equal (redex-match? tglc T (term (→ int *))) #t))

(module+ test
  (test-equal (term (lookup (((addr 0) (λ (x) 1)) ·) (addr 0))) (term (λ (x) 1)))
  (test-equal (term (lookup (((addr 0) 1) ·) (addr 0))) 1)
  (check-exn exn:fail? (λ () (term (lookup (((addr 0) (λ (x) x)) ((addr 1) 27) ·) (addr 3)))) "not found"))

(module+ test
  (test-equal (term (hastype (((addr 0) 42) ·) 38 *)) #t)
  (test-equal (term (hastype (((addr 0) 42) ·) (addr 0) ref)) #t)
  (test-equal (term (hastype (((addr 0) 42) ·) 42 ref)) #f)
  (test-equal (term (hastype (((addr 0) (λ (x) x)) ·) (addr 0) →)) #t))

(module+ test
  (test-equal (term (plus 1 2)) 3)
  (test-equal (term (plus 0 0)) 0))

(module+ test
  (test-equal (term (replace (((addr 0) 0) ·) ((addr 0) 3))) (term (((addr 0) 3) ·)))
  (check-exn exn:fail? (λ () (term (replace (((addr 0) 1) ·) ((addr 1) 1)))) "address not found")
  (check-exn exn:fail? (λ () (term (replace (((addr 0) 1) ·) ((addr 1) (λ (x) x))))) "expected a value"))

(module+ test
  (test-equal (term (fresh-a (((addr 5) 1) ·))) (term (addr 6))))

(module+ test
  (test-equal (term (extend (((addr 0) 1) ·) ((addr 1) 2))) (term (((addr 1) 2) ((addr 0) 1) ·)))
  (test-equal (term (extend (((addr 0) 1) ·) ((fresh-a (((addr 0) 1) ·)) 2))) (term (((addr 1) 2) ((addr 0) 1) ·))))

(module+ test
  (test-equal (term (extract · (int ∈))) (term (int ∈)))
  (test-equal (term (extract (RES ·) (→ 1 (int 1) *))) (term *))
  (test-equal (term (extract (ARG ·) (→ 1 (int 1) *))) (term (int 1)))
  (test-equal (term (extract (DEREF ·) (ref 1 (int 1)))) (term (int 1)))
  (test-equal (term (extract (RES ·) *)) (term *)))

(module+ test
  (test-equal (term (label *)) (term ∈))
  (test-equal (term (label (int 1))) (term 1))
  (test-equal (term (label (→ 1 * *))) (term 1))
  (test-equal (term (label (ref 1 (int 1)))) (term 1))
  (test-equal (term (label (⊥ 1))) (term 1)))

(module+ test
  (test-equal (term (collect-blame · · (int 1))) (term ((int 1))))
  (test-equal (term (collect-blame · · *)) (term ()))
  (test-equal (term (lookup-β (((addr 1) (* * *)) ·) (addr 1))) (term (* * *)))
  (test-equal (term (collect-blame · (((addr 1) ((ref 5 (int 5)) *)) ·) ((addr 1) DEREF)))
              (term ((int 5)))))

(module+ test
  (test-equal (term (T-to-S *)) (term *))
  (test-equal (term (T-to-S int)) (term int))
  (test-equal (term (T-to-S (→ int *))) (term →))
  (test-equal (term (T-to-S (ref int))) (term ref)))

(module+ test
  (test-judgment-holds
   (▹ (ref int) (ref int)))
  (test-judgment-holds
   (▹ * (ref *)))
  (test-judgment-holds
   (▹ (→ (→ int int) int) (→ (→ int int) int)))
  (test-judgment-holds
   (▹ * (→ * *))))

(module+ test
  (test-judgment-holds
   (∼ int int))
  (test-judgment-holds
   (∼ * (→ int int)))
  (test-judgment-holds
    (∼ (ref int) *))
  (test-judgment-holds
    (∼ (ref *) (ref int)))
  (test-judgment-holds
    (∼ (→ int *) (→ int *))))

(module+ test
  (test-equal (term (lookup-Γ ((: x int) ·) x)) (term int)))

(module+ test
  (test-judgment-holds
   (↝ · 42 0 42 int 0))
  (test-judgment-holds
   (↝ ((: x int) ·) x 0 x int 0))
  (test-judgment-holds
   (↝ · (ref 42) 0 (ref 42) (ref int) 0)))

(module+ test
  (test-equal (term (fresh-l 42)) (term 43)))

(module+ test
  (test-judgment-holds
   (-→ ((! (addr 1)) (((addr 0) (λ (x) x)) ((addr 1) 27) ·) ·)
       (27 (((addr 0) (λ (x) x)) ((addr 1) 27) ·) ·)))
  (test-judgment-holds
   (-→ ((+ 1 2) · ·) (3 · ·)))
  (test-judgment-holds
   (-→ ((:= (addr 0) 3) (((addr 0) 0) ·) ·) (0 (((addr 0) 3) ·) ·)))
  (test-judgment-holds
   (-→ ((ref 3) (((addr 1) 42) ((addr 0) 37) ·) ·) ((addr 2) (((addr 2) 3) ((addr 1) 42) ((addr 0) 37) ·) ·)))
  (test-judgment-holds
   (-→ ((ref 3) (((addr 0) 0) ·) ·)
       ((addr 1) (((addr 1) 3) ((addr 0) 0) ·) ·)))
  (test-judgment-holds
   (-→ ((fun f (x) 6) (((addr 0) 42) · ) ·) ((addr 1) (((addr 1) (λ (x) 6)) ((addr 0) 42) · ) ·)))
  (test-judgment-holds
   (-→ ((app (addr 0) 4) (((addr 0) (λ (x) x)) ·) ·)
       (4 (((addr 0) (λ (x) x)) ·) ·)))
  ;"cast succeed v != a"
  (test-judgment-holds 
   (-→ ((:: 4 (⇒ 3 int int)) · ·)
       (4 · ·)))
  ;"check succeed v != a"
  (test-judgment-holds 
   (-→ ((⇓ 42 (int (addr 0) RES)) (((addr 0) 37)·) ·)
       (42 (((addr 0) 37)·) ·)))
  ;"cast succeed v = a"
  (test-judgment-holds
   (-→ ((:: (addr 0) (⇒ 123 int *)) (((addr 0) 11) ·) (((addr 0) ((int 144))) ·))
       ((addr 0) (((addr 0) 11) ·) (((addr 0) ((int 144) (int ∈))) ·))))
   
  )


(module+ test
  (test-results))