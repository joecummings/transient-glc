#lang racket
(require redex)
(require "tglc-def.rkt")

(provide hastype lookup)

(default-language tglc)

(define-metafunction tglc
  lookup : σ a -> h
  [(lookup ((a_1 h_1) ... (a h) (a_2 h_2) ... σ) a) h]
  [(lookup any_1 any_2) ,(error 'lookup "not found: ~e" (term any_1))])

(define-metafunction tglc
  value? : h -> #t or #f
  [(value? v) #t]
  [(value? h) #f])

(define-metafunction tglc
  hastype : σ v S -> #t or #f
  [(hastype σ n int) #t]
  [(hastype σ a int) #f]
  [(hastype σ a *) #t]
  [(hastype σ n *) #t]
  [(hastype σ a →) ,(if (term (value? (lookup σ a))) #f #t)]
  [(hastype σ n →) #f]
  [(hastype σ a ref) ,(if (term (value? (lookup σ a))) #t #f)]
  [(hastype σ n ref) #f])