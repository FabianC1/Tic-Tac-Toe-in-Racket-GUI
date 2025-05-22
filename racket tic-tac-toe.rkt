#lang racket/gui

;; Define a global variable to keep track of the score
(define score 0)

;; Define a function to start a new game
(define (new-game)
  ;; Reset the score to 0
  (set! score 0)
  ;; Update the score label in the GUI
  (update-score)
  ;; Start the game
  (start-game))

;; Define a function to update the score label in the GUI
(define (update-score)
  ;; Get the score label widget from the GUI
  (send score-label set-label (string-append "Score: " (number->string score))))

;; Define the main function to start a game
(define (start-game)
  ;; Define variables to keep track of scores for player X and player O
  (define player1-score 0)
  (define player2-score 0)

  ;; Define a function to reset the board
  (define (reset-board)
    ;; Set the board to a nested list of strings representing empty cells
    (set! board '(("-" "-" "-") ("-" "-" "-") ("-" "-" "-"))))

  ;; Define the board as a nested list of strings
  (define board '(("-" "-" "-") ("-" "-" "-") ("-" "-" "-")))

  ;; Define a function to display the scores
  (define (display-scores)
    (display (format "Player X: ~a\n" player1-score))
    (display (format "Player O: ~a\n" player2-score)))

  ;; Define a function to display the board
  (define (display-board)
    ;; Print the column labels
    (display "   1   2   3\n")
    ;; Iterate over the rows of the board
    (for ((i '(1 2 3)))
      (begin
        ;; Print the row number
        (display (format "~a  " i))
        ;; Iterate over the columns of the board
        (for ((j '(1 2 3)))
          ;; Print the cell value
          (display (format "| ~a " (list-ref (list-ref board (- i 1)) (- j 1)))))
        ;; Print the end of the row
        (display "|")
        (displayln "")
        ;; Print a horizontal divider between rows
        (when (not (equal? i 3))
          (display "  ---+---+---\n")))))

  ;; Define a function to check if the board is full
  (define (full-board?)
    ;; Return true if all cells on the board are filled
    (not (ormap (lambda (row) (ormap (lambda (cell) (equal? cell "-")) row)) board)))
  


  ; Define a function to check if there is a winner
  (define (winner?)
    ; Check for a winning symbol (horizontal, vertical, or diagonal)
    (let ((winning-symbol (or (horizontal-win?) (vertical-win?) (diagonal-win?))))
      ; If there is a winning symbol, update the score and return the symbol
      (when winning-symbol
        (if (equal? winning-symbol "X")
            (set! player1-score (+ player1-score 1))
            (set! player2-score (+ player2-score 1))))
      winning-symbol))

  ; Define a function to check for a horizontal win
  (define (horizontal-win?)
    ; Check each row for a win
    (ormap (lambda (row)
             (and (equal? (list-ref row 0) (list-ref row 1))
                  (equal? (list-ref row 0) (list-ref row 2))
                  (not (equal? (list-ref row 0) "-"))))
           board))

  ; Define a function to check for a vertical win
  (define (vertical-win?)
    ; Check each column for a win
    (ormap (lambda (col)
             (and (equal? (list-ref (list-ref board 0) col) (list-ref (list-ref board 1) col))
                  (equal? (list-ref (list-ref board 0) col) (list-ref (list-ref board 2) col))
                  (not (equal? (list-ref (list-ref board 0) col) "-"))))
           '(0 1 2)))

  ; Define a function to check for a diagonal win
  (define (diagonal-win?)
    ; Check both diagonals for a win
    (or (and (equal? (list-ref (list-ref board 0) 0) (list-ref (list-ref board 1) 1))
             (equal? (list-ref (list-ref board 0) 0) (list-ref (list-ref board 2) 2))
             (not (equal? (list-ref (list-ref board 0) 0) "-")))
        (and (equal? (list-ref (list-ref board 0) 2) (list-ref (list-ref board 1) 1))
             (equal? (list-ref (list-ref board 0) 2) (list-ref (list-ref board 2) 0))
             (not (equal? (list-ref (list-ref board 0) 2) "-")))))

  ; Define a function to check if the game is over
  (define (game-over?)
    ; Check for a winner, a full board, or a tie
    (or (winner?) (full-board?) (tie?)))

  ; Define a function to check for a tie
  (define (tie?)
    ; Check if any cells are empty
    (not (ormap (lambda (row) (ormap (lambda (cell) (equal? cell "-")) row)) board)))
  

  ; This function takes a player as input and prompts them to enter a move on the board.
  (define (get-move player)
    (display (format "~a's turn:\n" player)) ; Displays the player's turn.
    (let loop () ; Defines a loop function to repeatedly prompt for input until a valid move is made.
      (display "Enter row (1-3): ")
      (let ((row (read))) ; Reads the player's input for the row.
        (display "Enter column (1-3): ")
        (let ((col (read))) ; Reads the player's input for the column.
          (if (and (integer? row) ; Checks if the row input is an integer.
                   (integer? col) ; Checks if the column input is an integer.
                   (<= 1 row 3) ; Checks if the row input is between 1 and 3 inclusive.
                   (<= 1 col 3) ; Checks if the column input is between 1 and 3 inclusive.
                   (equal? (list-ref (list-ref board (- row 1)) (- col 1)) "-")) ; Checks if the selected position on the board is empty.
              (begin ; Executes if the move is valid.
                (set! board (map (lambda (r) ; Updates the game board by replacing the selected position with the player's symbol.
                                   (map (lambda (c)
                                          (if (and (= (- row 1) r) (= (- col 1) c))
                                              player
                                              (list-ref (list-ref board r) c)))
                                        '(0 1 2)))
                                 '(0 1 2)))
                (display-board) ; Displays the updated game board.
                #t) ; Returns true to indicate that a valid move has been made.
              (begin (displayln "Invalid move, please try again.") ; Executes if the move is invalid and prompts the player to try again.
                     (loop))))))) ; Loops until a valid move is made.


; This function starts a game of tic-tac-toe.
(define (play-game)
  (display-board) ; Displays the initial game board.
  (let loop ((player "X")) ; Defines a loop function to alternate between players until the game is over.
    (cond ((winner?) ; Checks if a player has won the game.
           (displayln (format "~a wins!" player)) ; Displays the winning player.
           (displayln "Game over!")
           (display-scores)) ; Displays the current score.
          ((tie?) ; Checks if the game is a tie.
           (displayln "It's a tie!") ; Displays that the game is a tie.
           (displayln "Game over!")
           (display-scores)) ; Displays the current score.
          (else ; Executes if the game is ongoing.
           (get-move player) ; Prompts the current player to make a move.
           (loop (if (equal? player "X") "O" "X")))))) ; Switches to the other player
  (play-game)

  ; This function resets the board and starts a new game.
  (define (replay-game)
    (reset-board) ; Resets the game board.
    (start-game)) ; Starts a new game.

  ; This creates a "Replay" button that calls the replay-game function when clicked.
  (define replay-button (new button% [parent frame]
                             [label "Replay"]
                             [callback (lambda (button event)
                                         (replay-game))]))

  ; This updates the score and displays it.
  (set! score (+ score 1))
  (update-score))


; Create a new frame object with a title of "Game" and dimensions of 300x200.
(define frame (new frame%
                   [label "Game"]
                   [width 300]
                   [height 200]))

; Create a new message object that displays the current score.
; The message will be displayed within the frame window.
(define score-label (new message% [parent frame]
                         [label (string-append "Score: " (number->string score))]))

; Create a new button object with the label "New Game" that will call the "new-game" function when clicked.
; The button will be displayed within the frame window.
(define new-game-button (new button% [parent frame]
                             [label "New Game"]
                             [callback (λ (button event) (new-game))]))

; Display the frame window.
(send frame show #t)