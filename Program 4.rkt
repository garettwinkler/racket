#lang racket
;Garett Winkler
;Program 4
;This program computes the sum of an arithmetic sequency and 
;computes the value of a power function using the "fast" algorithm.

(define (arithmeticSeq initial diff term)
  (if (= term 0)
      initial
     (+ diff (arithmeticSeq initial diff (- term 1)))
  )
)



;-------------------------------------------------------------------------------

(define (square number)
  (* number number)
)

(define (computeNth base exponent)
  (if (= exponent 0)
      1
      (if (even? exponent)
          (square (computeNth base (/ exponent 2)))
          (* base (square (computeNth base (/ (- exponent 1) 2)))
          )
      )
   )
)

  