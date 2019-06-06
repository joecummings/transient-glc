#lang racket
(require rackunit)
(require redex)
(require "../tglc-def.rkt" "../main.rkt" "../blame.rkt" "../translate.rkt" "../types.rkt")

(define-metafunction tglc
  blame-not-empty? : ς -> #t or #f
  [(blame-not-empty? (BLAME weird-L)) #t]
  [(blame-not-empty? (BLAME ·)) #f])

(define count 0)

(define (reduces-to-blame? ς)
  (set! count (+ 1 count))
  (displayln count)
  (displayln ς)
  (displayln "-------")
  (let ([target (with-handlers ([exn:fail? (λ (exn) 'failed)])
                        (apply-reduction-relation -→ ς))])
  (cond
    [(equal? 'failed target) #f]
    [(redex-match? tglc (BLAME weird-L) target) #t]
    [else #f])))

(redex-check tglc ((⇓ v (S a r)) σ β) 
  (if (reduces-to-blame? (term ((⇓ v (S a r)) σ β))) 
      (term (blame-not-empty? (apply-reduction-relation -→ (term (⇓ v (S a r))))))
      #t)
  #:attempts 1000000)