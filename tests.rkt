#lang racket
;(require rackunit)
(require redex)

(require "main.rkt")

(module+ test
  (test-equal (redex-match? tglc e (term y)) #t)
  (test-equal (redex-match? tglc e (term 123)) #t)
  (test-equal (redex-match? tglc e (term (fun coolfunction (y) y))) #t)
  (test-equal (redex-match? tglc e (term (x x))) #t)
  (test-equal (redex-match? tglc e (term (ref x))) #t)
  (test-equal (redex-match? tglc e (term (! x))) #t)
  (test-equal (redex-match? tglc e (term (:= x y))) #t)
  (test-equal (redex-match? tglc e (term (+ 2 1))) #t))

(module+ test
  (test-equal (redex-match? tglc a 123123123) #t)
  (test-equal (redex-match? tglc v 123123) #t))

;(module+ test
  ;(check-exn exn:fail? (λ () (term (lookup ((0 (λ (x) x)) (1 27) ·) 0))) "not a value")
 ; (check-exn exn:fail? (λ () (term (lookup ((0 (λ (x) x)) (1 27) ·) 3))) "not found"))

(module+ test
  (test-results))