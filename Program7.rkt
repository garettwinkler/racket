#lang racket
;Garett Winkler
;Program 7
;This program has functions that return the dot product of two lists,
;returns whether or not a list contains duplicates, and returns the 
;largest difference between two corresponding positions in two given
;lists.

;returns the dotProduct of two lists
(define (dotProduct listOne listTwo) 
  (if (null? listOne)
      0
      (+ 
       (* (car listOne) (car listTwo)) 
       (dotProduct (cdr listOne) (cdr listTwo))
      )
   )
)



;/////////////////////////////////////////////////////////////////////////////////////////////////

;returns whether or not the list contains duplicates (t or f)
(define (areDuplicates list) 
  (if (null? list)
      #f
      (if (>= (matches list (car list)) 2)  ;>=2 because the car is the number passed it will always find at least one match but it's not a duplicate
         #t
         (areDuplicates (cdr list))
       )
   )
)

;helper for areDuplicates.  counts matches to given number in the list
(define (matches list number) 
  (if (null? list)
      0
      (+
       (if (= (car list) number) 1 0)
       (matches (cdr list) number)
      )
   )
)



;/////////////////////////////////////////////////////////////////////////////////////////////////////


(define (largestDiff listOne listTwo) 
 (if (null? listOne)
     #f
     (if (null? (cdr listOne))
         (abs (- (car listOne) (car listTwo)))
         (if (> 
              (abs (- (car listOne) (car listTwo)))
              (largestDiff (cdr listOne) (cdr listTwo))
             )
           (abs (- (car listOne) (car listTwo)))
           (largestDiff (cdr listOne) (cdr listTwo))
         )
     )
  )
)



     
