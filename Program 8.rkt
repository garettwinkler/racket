#lang racket

;Garett Winkler
;Program 8
;This program manipulates bags of strings

;0 Returns the string
(define (getValue Pair)
  (car Pair)
)

;1 Returns the count of that string
(define (getCount Pair)
  (cdr Pair)
)

;2 creates a new pair of one occurence of the given item
(define (newPair Item)
  (cons Item 1)
)

;3 increments a pair, representing one more of the item
(define (incPair Pair)
  (cons (getValue Pair) (+ (getCount Pair) 1))
)

;4 decrements a pair, representing one less
(define (decPair Pair)
  (cons (getValue Pair) (- (getCount Pair) 1))
)

;5 inserts the given item into the given list
(define (insertBag List Item)
  (if (null? List)
     (cons (newPair Item) List)
     (if (string=? (getValue (car List)) Item)
         (cons (incPair (car List)) (cdr List))
         (cons (car List) (insertBag (cdr List) Item))
      )
  )
)

;6 returns the number of occurrences of the given item
(define (getBagCount List Item)
  (if (null? List)
      0
      (if (string=? (getValue (car List)) Item)
          (getCount (car List))
          (getBagCount (cdr List) Item)
       )
   )
)

;7 deletes one occurence of the given item
(define (deleteBag List Item)
  (if (null? List)
      '()
      (if (string=? (getValue (car List)) Item)
          (if (= (getCount (car List)) 1)
              (cdr List)
              (cons (decPair (car List)) (cdr List))
           )
          (cons (car List) (deleteBag (cdr List) Item))
       )
   )
)

;8 deletes all occurences of the given items
(define (deleteAllBag List Item)
  (if (null? List)
      '()
      (if (string=? (getValue (car List)) Item)
          (cdr List)
          (cons (car List) (deleteAllBag (cdr List) Item))
        )
    )
)

;9 makes new bag that is combination of two given bags
(define (unionBag ListA ListB)
  (if (and (null? ListA) (null? ListB))
      '()
      (if (null? ListA)
          ListB
          (cons (scanAndCombine ListA ListB) (unionBag (cdr ListA) (deleteAllBag ListB (getValue (car ListA)))))
      )
  )
)

;helper function for unionBag
(define (scanAndCombine ListA ListB)
  (if (null? ListB)
      (car ListA)
      (if (string=? (getValue (car ListA)) (getValue (car ListB)))
          (combine (car ListA) (car ListB))
          (scanAndCombine ListA (cdr ListB))
      )
  )
)
     
;helper function for unionBag
(define (combine pair1 pair2)
  (cons (getValue pair1) (+ (getCount pair1) (getCount pair2)))
)

;10 intersectBag, makes a bag of the "intersection" between the two bags
(define (intersectBag ListA ListB)
  (if (or (null? ListA) (null? ListB))
     '()
     (if (null? (scanAndPick ListA ListB))
         (intersectBag (cdr ListA) ListB)
         (cons (scanAndPick ListA ListB) (intersectBag (cdr ListA) ListB))
     )
  )
)

;helper function for intersectBag. (picks the lesser frequency pair)
(define (scanAndPick ListA ListB)
  (if (or (null? ListA) (null? ListB))
      '()
      (if (string=? (getValue (car ListA)) (getValue (car ListB)))
          (if (< (getCount (car ListA)) (getCount (car ListB)))
              (car ListA)
              (car ListB)
          )
          (scanAndPick ListA (cdr ListB))
      )
   )
)
   
;testing
(define List1 '(("a" . 3) ("b" . 1)("c" . 2)))
(define List2 '())
(define List3 '(("a" . 3) ("b" . 1)("c" . 2)))
(define List4 '(("c" . 2) ("b" . 1)("a" . 3)))
(define List5 '(("c" . 2) ("b" . 1)("a" . 3)("d" . 1)))
(define List6 '(("b" . 1) ("a" . 1)("c" . 3)))
(define List7 '(("d" . 5) ("b" . 1)("c" . 1)))






