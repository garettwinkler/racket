#lang racket
;Garett Winkler
;Program 11 phase I
;This program has functions for returning the item in a list of lists (a matrix)
;and a function for placing an item in a given position in a matrix. 

;get cell returns the item at the given grid location in a list of lists (a matrix)
(define (getCell Matrix Row Column)
  (if (null? Matrix)
      '()
      (if (= Row 1)
          (getCellTwo (car Matrix) Column)
          (getCell (cdr Matrix) (- Row 1) Column)
      )
  )
)

;helper function for getCell
(define (getCellTwo Matrix Column)
  (if (null? Matrix)
      '()
      (if (= Column 1)
          (car Matrix)
          (getCellTwo (cdr Matrix) (- Column 1))
       )
  )
)

;setCell returns a new matrix witht eh given grid location reset to the given value Item
(define (setCell Matrix Row Column Item)
  (if (null? Matrix)
      '()
      (if (= Row 1)
          (cons (setCellTwo (car Matrix) Column Item) (cdr Matrix))
          (cons (car Matrix)(setCell (cdr Matrix)(- Row 1) Column Item))
       )
   )
)

;helper for setCell
(define (setCellTwo Matrix Column Item)
  (if (null? Matrix)
      '()
      (if (= Column 1)
          (cons Item (cdr Matrix))
          (cons (car Matrix)(setCellTwo (cdr Matrix)(- Column 1) Item))
      )
  )
)

;testing
(define Matrix1 '((2 4 6 8)(1 3 5 7)(2 9 0 1)))
(getCell Matrix1 2 3)
(getCell Matrix1 1 1)
(getCell Matrix1 3 4)

(setCell Matrix1 2 3 25)
(setCell Matrix1 1 1 25)
(setCell Matrix1 3 4 25)