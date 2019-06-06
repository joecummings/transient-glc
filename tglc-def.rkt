#lang racket
(require redex)

(provide tglc)

(define-language tglc
  (e ::= x v (fun f (x) e) (app e e) (+ e e)
     (ref e) (! e) (:= e e) (:: e cast-e) (⇓ e (S e r))) ; target lang expr
  (es ::= x (→ (fun f (: x T)) (T es)) (app es es)
      (ref es) (! es) (:= es es) n (+ es es)) ; source lang expr
  (Γ ::= · ((: x T) ... Γ))
  (cast-e := (⇒ l T T) (⇔ l T T))
  (v ::= a n) ; values
  (x y f ::= variable-not-otherwise-mentioned)
  (n ::= natural) ; naturals

  (a ::= (addr natural)) ; address
  (σ ::= · ((a h) ... σ)) ; heaps
  (h ::= (λ (x) e) v) ; heap values
  
  (β ::= · ((a b ...) ... β)) ; blame sets
  (b ::= (a r) L) ; blame elems
  (l ::= natural) ; blame labels

  (T ::= int (→ T T) * (ref T)) ; types
  (L ::= (int q) (→ q L L) (ref q L) * (⊥ l)) ; labelled types
  (S ::= int → ref *) ; type tags
  (q ::= l ∈) ; optional labels
  (r ::= RES ARG DEREF) ; tags

  (weird-L ::= · (q ... weird-L))
 
  (E ::= hole (app E e) (app v E) (+ E e) (+ v E)
     (ref E) (! E) (:= E e) (:: E cast-e) (⇓ E (S e r)) (⇓ v (S E r)))  ; E 
  (ς ::= (e σ β) (BLAME weird-L))
  #:binding-forms
  (λ (x) e #:refers-to x))