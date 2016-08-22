#lang racket


;Garett Winkler
;Program 11b
;This program...connect 4

(define GRWGame '())

(define (GRWStartGame)
  (begin
    (set! GRWGame '(1 (_ _ _ _ _ _ _)
                    (_ _ _ _ _ _ _)
                    (_ _ _ _ _ _ _)
                    (_ _ _ _ _ _ _)
                    (_ _ _ _ _ _ _)
                    (_ _ _ _ _ _ _))
     )
    (newline)
    (display "You ain't no daisy....") (newline) (newline)
  )
)

;displays the game state (the board)
(define (GRWShowGame)
  (begin
    (display (car (cdr (cdr (cdr (cdr (cdr (cdr GRWGame)))))))) (newline)
    (display (car (cdr (cdr (cdr (cdr (cdr GRWGame))))))) (newline)
    (display (car (cdr (cdr (cdr (cdr GRWGame)))))) (newline)
    (display (car (cdr (cdr (cdr GRWGame))))) (newline)
    (display (car (cdr (cdr GRWGame)))) (newline)
    (display (car (cdr GRWGame))) (newline) 
    #t
  )
)

;marks a move
(define (GRWMarkMove col)
  (begin
    (set! GRWGame
          (if (= (car GRWGame) 1)
              (cons 2 (GRWsetCell (cdr GRWGame) (GRWcheckRow 1 col) col 1)) ;maybe change 1 and 2 for X and O later
              (cons 1 (GRWsetCell (cdr GRWGame) (GRWcheckRow 1 col) col 2))
          )
    )
    (if (GRWWinP col)
        (if (= (car GRWGame) 1)
            (display "Player 2 has won ")
            (display "Player 1 has won ")
         )
        '()
    )
  )
  col
)

;makes a move
(define (GRWMakeMove)
  (GRWMarkMove (GRWgetValidRan (GRWChooseMove)))
)

;chooses a move (checks for win, block, then random)
(define (GRWChooseMove)
  (cond
    ((eq? #t (GRWWillWinP 1)) 1)
    ((eq? #t (GRWWillWinP 2)) 2)
    ((eq? #t (GRWWillWinP 3)) 3)
    ((eq? #t (GRWWillWinP 4)) 4)
    ((eq? #t (GRWWillWinP 5)) 5)
    ((eq? #t (GRWWillWinP 6)) 6)
    ((eq? #t (GRWWillWinP 7)) 7)
    ((eq? #t (GRWneedBlock 1)) 1)       ;maybe write loop to loop through columns later
    ((eq? #t (GRWneedBlock 2)) 2)
    ((eq? #t (GRWneedBlock 3)) 3)
    ((eq? #t (GRWneedBlock 4)) 4)
    ((eq? #t (GRWneedBlock 5)) 5)
    ((eq? #t (GRWneedBlock 6)) 6)
    ((eq? #t (GRWneedBlock 7)) 7)
    ;hard code opening moves?
    (#t (+ 1 (random 7)))
  )
)


;loops until a valid ran column is chosen
(define (GRWgetValidRan ranCol)
  (if (eq? #t (GRWLegalMoveP ranCol))
      ranCol
      (GRWgetValidRan (+ 1 (random 7)))  
   )
)


;checks if it is a legal move (if the column is full)
(define (GRWLegalMoveP col)
  (if (<= (GRWcheckRow 1 col) 6)
      #t
      #f
  )
)
  
;helper for markMove and others
(define (GRWcheckRow row col)
  (if (= row 7)
      row
      (if (eq? (GRWgetCell (cdr GRWGame) row col) '_ )
          row
          (GRWcheckRow (+ 1 row) col)
      )
  )
)

;checks if a move in the given column would result in a win
(define (GRWWillWinP col)
  (if (or 
        (eq? #t (GRWvertWin col))     
        (eq? #t (GRWhorizWin col))
        (eq? #t (GRWdiag col))
      )
   
      #t
      #f
  )
)

;returns whether a diagonal win is possible (willWin)
(define (GRWdiag col)
 (if (or (<= 3
              (+ 
               (GRWupR (GRWcheckRow 1 col) col) 
               (GRWdownL (GRWcheckRow 1 col) col)
              )
           )
          (<= 3
             (+
               (GRWupL (GRWcheckRow 1 col) col) 
               (GRWdownR (GRWcheckRow 1 col) col)
              )
           )
       )
       #t
       #f
   )
)


;how many in a row downLeft (willWin)
(define (GRWdownR row col)
   (if (eq? '() (GRWgetCell (cdr GRWGame) row col))
       0
       (if (eq? (car GRWGame)(GRWgetCell (cdr GRWGame) (- row 1) (+ col 1)))
           (+ 1 (GRWdownR (- row 1) (+ col 1)))
           0
       )
  )
)

;how many in a row downRight (willWin)
(define (GRWdownL row col)
   (if (eq? '() (GRWgetCell (cdr GRWGame) row col))
       0
       (if (eq? (car GRWGame)(GRWgetCell (cdr GRWGame) (- row 1) (- col 1)))
           (+ 1 (GRWdownL (- row 1) (- col 1)))
           0
       )
  )
)

;how many in a row upLeft (willWin)
(define (GRWupL row col)
   (if (eq? '() (GRWgetCell (cdr GRWGame) row col))
       0
       (if (eq? (car GRWGame)(GRWgetCell (cdr GRWGame) (+ row 1) (- col 1)))
           (+ 1 (GRWupL (+ row 1) (- col 1)))
           0
       )
  )
)

;how many in a row upRight (willWin)
(define (GRWupR row col)
   (if (eq? '() (GRWgetCell (cdr GRWGame) row col))
       0
       (if (eq? (car GRWGame)(GRWgetCell (cdr GRWGame) (+ row 1) (+ col 1)))
           (+ 1 (GRWupR (+ row 1) (+ col 1)))
           0
       )
  )
)


;returns how many in a row right (willWin)
(define (GRWright row col)
  (if (eq? '() (GRWgetCell (cdr GRWGame) row col))                 
       0
       (if (eq? (car GRWGame)(GRWgetCell (cdr GRWGame) row (+ col 1))) 
          (+ 1 (GRWright row (+ col 1)))
          0
       )
   )
)
;returns how many in a row left (willWin)
(define (GRWleft row col)
  (if (eq? '() (GRWgetCell (cdr GRWGame) row col))                 
       0
       (if (eq? (car GRWGame)(GRWgetCell (cdr GRWGame) row (- col 1))) 
          (+ 1 (GRWleft row (- col 1)))
          0
       )
   )
)

;checks if a horizontal win is possible (willWin)
(define (GRWhorizWin col)
  (if (>= (+
             (GRWright (GRWcheckRow 1 col) col)
             (GRWleft (GRWcheckRow 1 col) col)
           )
           3
      )
      #t
      #f
  )    
)



;checks if a vertical win is possible (willWin)
(define (GRWvertWin col)
  (if (and 
         (= 3 (GRWdown (- (GRWcheckRow 1 col) 1) col)) ;(don't need >= 3 because should never happen, game would be already won)
         (not (= 7 (GRWcheckRow 1 col)))            ;second condition to check for full column/invalid move
       )  
       #t
       #f
   )
)

;retuns how many are in a row vertically (willWin)
(define (GRWdown row col)
  (if (eq? (GRWgetCell (cdr GRWGame) row col) '())
      0
      (if (= (car GRWGame) (GRWgetCell (cdr GRWGame) row col))
          (+ 1 (GRWdown (- row 1) col))
          0
      )
  )
)

;checks if the last move resulted in a win  
(define (GRWWinP col)
  (if (or 
        (eq? #t (GRWcheckVertical col))
        (eq? #t (GRWcheckHorizontal col))
        (eq? #t (GRWcheckDiagonal col))
       )      
      #t
      #f
   )
)

;checks if the opponent can win next turn
(define (GRWneedBlock col)
  (if (or
        (eq? #t (GRWvertBlock col))
        (eq? #t (GRWhorizBlock col))
        (eq? #t (GRWdiagBlock col))
      )
      #t
      #f
  )
)
        

;tests if there is a diagonal win. (Win)
(define (GRWcheckDiagonal col)
  (if (or (<= 5
              (+ 
               (GRWupRight (- (GRWcheckRow 1 col) 1) col) 
               (GRWdownLeft (- (GRWcheckRow 1 col) 1) col)
              )
           )
          (<= 5
             (+
               (GRWupLeft (- (GRWcheckRow 1 col) 1) col) 
               (GRWdownRight (- (GRWcheckRow 1 col) 1) col)
              )
           )
       )
       
       #t
       #f
   )
)


;tests if there is a diagonal win. (Win)
(define (GRWdiagBlock col)
  (if (or (<= 3
              (+ 
               (GRWupright (GRWcheckRow 1 col) col) 
               (GRWdownleft (GRWcheckRow 1 col) col)
              )
           )
          (<= 3
             (+
               (GRWupleft (GRWcheckRow 1 col) col) 
               (GRWdownright (GRWcheckRow 1 col) col)
              )
           )
       )
       #t
       #f
   )
)

;returns how many in a row up/right (block)
(define (GRWupright row col)
  (if (or 
         (eq? (car GRWGame)(GRWgetCell (cdr GRWGame) (+ row 1) (+ col 1))) 
         (eq? '_ (GRWgetCell (cdr GRWGame) (+ row 1) (+ col 1)))
         (eq? '() (GRWgetCell (cdr GRWGame)(+ row 1) (+ col 1)))                  
       )
       0
       (+ 1 (GRWupright (+ row 1) (+ col 1)))
  )
)

;returns how many in a row down/right (block)
(define (GRWdownright row col)
   (if (or 
         (eq? (car GRWGame)(GRWgetCell (cdr GRWGame) (- row 1) (+ col 1))) 
         (eq? '_ (GRWgetCell (cdr GRWGame) (- row 1) (+ col 1)))
         (eq? '() (GRWgetCell (cdr GRWGame) (- row 1) (+ col 1)))                  
       )
       0
       (+ 1 (GRWdownright (- row 1) (+ col 1)))
  )
)

;returns how many in a row up/left (block)
(define (GRWupleft row col)
   (if (or 
         (eq? (car GRWGame)(GRWgetCell (cdr GRWGame) (+ row 1) (- col 1))) 
         (eq? '_ (GRWgetCell (cdr GRWGame) (+ row 1) (- col 1)))
         (eq? '() (GRWgetCell (cdr GRWGame) (+ row 1) (- col 1)))                  
       )
       0
       (+ 1 (GRWupleft (+ row 1) (- col 1)))
  )
)

;returns how many in a row down/left (block)
(define (GRWdownleft row col)
   (if (or 
         (eq? (car GRWGame)(GRWgetCell (cdr GRWGame) (- row 1) (- col 1))) 
         (eq? '_ (GRWgetCell (cdr GRWGame) (- row 1) (- col 1)))
         (eq? '() (GRWgetCell (cdr GRWGame) (- row 1) (- col 1)))                  
       )
       0
       (+ 1 (GRWdownleft (- row 1) (- col 1)))
  )
)

;returns how many in a row up/right  (Win)
(define (GRWupRight row col)
  (if (or 
         (eq? (car GRWGame)(GRWgetCell (cdr GRWGame) row col)) 
         (eq? '_ (GRWgetCell (cdr GRWGame) row col))
         (eq? '() (GRWgetCell (cdr GRWGame) row col))                  
       )
       0
       (+ 1 (GRWupRight (+ row 1) (+ col 1)))
  )
)

;returns how many in a row down/right(Win)
(define (GRWdownRight row col)
   (if (or 
         (eq? (car GRWGame)(GRWgetCell (cdr GRWGame) row col)) 
         (eq? '_ (GRWgetCell (cdr GRWGame) row col))
         (eq? '() (GRWgetCell (cdr GRWGame) row col))                  
       )
       0
       (+ 1 (GRWdownRight (- row 1) (+ col 1)))
  )
)

;returns how many in a row up/left(Win)
(define (GRWupLeft row col)
   (if (or 
         (eq? (car GRWGame)(GRWgetCell (cdr GRWGame) row col)) 
         (eq? '_ (GRWgetCell (cdr GRWGame) row col))
         (eq? '() (GRWgetCell (cdr GRWGame) row col))                  
       )
       0
       (+ 1 (GRWupLeft (+ row 1) (- col 1)))
  )
)

;returns how many in a row down/left(Win)
(define (GRWdownLeft row col)
   (if (or 
         (eq? (car GRWGame)(GRWgetCell (cdr GRWGame) row col)) 
         (eq? '_ (GRWgetCell (cdr GRWGame) row col))
         (eq? '() (GRWgetCell (cdr GRWGame) row col))                  
       )
       0
       (+ 1 (GRWdownLeft (- row 1) (- col 1)))
  )
)

;tests if there is a horizontal win. (Win)  
(define (GRWcheckHorizontal col) 
  (if (<= 5 ( + 
             (GRWinARowRight (- (GRWcheckRow 1 col) 1) col)      ;checks for 5 because overlap in right and left
             (GRWinARowLeft (- (GRWcheckRow 1 col) 1) col)
           )
       )
       #t
       #f
   )
)

;checks if opponent can make a horizontal win  (block)
(define (GRWhorizBlock col)
    (if (<= 3 ( + 
                (GRWRight (GRWcheckRow 1 col)  col)      
                (GRWLeft (GRWcheckRow 1 col) col)
              )
       )
       #t
       #f
   )
)


;returns how many in a row right horizontally (block)  
(define (GRWRight row col)
  (if (or 
       (eq? (car GRWGame)(GRWgetCell (cdr GRWGame) row (+ col 1))) 
       (eq? '_ (GRWgetCell (cdr GRWGame) row (+ col 1)))
       (eq? '() (GRWgetCell (cdr GRWGame) row (+ col 1)))                 
       )
      0
      (+ 1 (GRWRight row (+ col 1)))
   )
)

;returns how many in a row left horizontally (block)    
(define (GRWLeft row col)
  (if (or 
       (eq? (car GRWGame)(GRWgetCell (cdr GRWGame) row (- col 1))) 
       (eq? '_ (GRWgetCell (cdr GRWGame) row (- col 1)))
       (eq? '() (GRWgetCell (cdr GRWGame) row (- col 1)))         
       )
      0
      (+ 1 (GRWLeft row (- col 1)))
  )
)

;returns how many in a row right horizontally, (Win) 
(define (GRWinARowRight row col)
  (if (or 
       (eq? (car GRWGame)(GRWgetCell (cdr GRWGame) row col)) 
       (eq? '_ (GRWgetCell (cdr GRWGame) row col))
       (eq? '() (GRWgetCell (cdr GRWGame) row col))                 
       )
      0
      (+ 1 (GRWinARowRight row (+ col 1)))
   )
)

;returns how many in a row left horizontally, (Win) 
(define (GRWinARowLeft row col)
  (if (or 
       (eq? (car GRWGame)(GRWgetCell (cdr GRWGame) row col)) 
       (eq? '_ (GRWgetCell (cdr GRWGame) row col))
       (eq? '() (GRWgetCell (cdr GRWGame) row col))         
       )
      0
      (+ 1 (GRWinARowLeft row (- col 1)))
  )
)


;tests if there is vertical win. (Win)
(define (GRWcheckVertical col)
   (if (= 4 (GRWinARow (- (GRWcheckRow 1 col) 1) col))  ;(don't need to check >= 4 b/c shouldn't happen, win already occurred)
       #t
       #f
   )
)

;tests if a win is possible for opponent (block)
(define (GRWvertBlock col)
  (if (and 
          (<= 3 (GRWinARow (- (GRWcheckRow 1 col) 1) col))  ;(don't NEED to check >= 3 b/c shouldn't happen, win already occurred)
          (not (= 7 (GRWcheckRow 1 col)))            ;second condition to check for full column/invalid move
       )
       #t
       #f
   )
)

; returns how many in a row vertically  (Win)
(define (GRWinARow row col)
  (if (or 
         (eq? (GRWgetCell (cdr GRWGame) row col) '())
         (= (car GRWGame) (GRWgetCell (cdr GRWGame) row col))     
       )   
      0
      (+ 1 (GRWinARow (- row 1) col))
       
  )
)

;get cell returns the item at the given grid location in a list of lists (a matrix)
(define (GRWgetCell Matrix Row Column)
  (if (null? Matrix)
      '()
      (if (= Row 1)
          (GRWgetCellTwo (car Matrix) Column)
          (GRWgetCell (cdr Matrix) (- Row 1) Column)
      )
  )
)

;helper function for getCell
(define (GRWgetCellTwo Matrix Column)
  (if (null? Matrix)
      '()
      (if (= Column 1)
          (car Matrix)
          (GRWgetCellTwo (cdr Matrix) (- Column 1))
       )
  )
)

;setCell returns a new matrix witht eh given grid location reset to the given value Item
(define (GRWsetCell Matrix Row Column Item)
  (if (null? Matrix)
      '()
      (if (= Row 1)
          (cons (GRWsetCellTwo (car Matrix) Column Item) (cdr Matrix))
          (cons (car Matrix)(GRWsetCell (cdr Matrix)(- Row 1) Column Item))
       )
   )
)

;helper for setCell
(define (GRWsetCellTwo Matrix Column Item)
  (if (null? Matrix)
      '()
      (if (= Column 1)
          (cons Item (cdr Matrix))
          (cons (car Matrix)(GRWsetCellTwo (cdr Matrix)(- Column 1) Item))
      )
  )
)


;testing






