#lang racket
(require rackunit)
(require redex)
(require "../tglc-def.rkt" "../main.rkt" "../blame.rkt" "../translate.rkt" "../types.rkt")

(define-metafunction tglc
  blame-not-empty? : ς -> #t or #f
  [(blame-not-empty? (BLAME weird-L)) #t]
  [(blame-not-empty? (BLAME ·)) #f])

;;; (define count 0)

(define (reduces-to-blame? ς)
  (cond
    [(equal? 'failed ς) #f]
    [(redex-match? tglc (BLAME weird-L) ς) #t]
    [else #f])))

(redex-check tglc ((⇓ v (S a r)) σ β) 
	(let ([target (with-handlers ([exn:fail? (λ (exn) 'failed)])
										(apply-reduction-relation -→ (term ((⇓ v (S a r)) σ β))))])
  (if (reduces-to-blame? target) 
      (term (blame-not-empty? target))
      #t)
  #:attempts 1000000)