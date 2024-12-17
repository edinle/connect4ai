######################################
#### Game Code #######################
######################################

initialize_board <- function() {
  matrix(0, nrow = 6, ncol = 7)
}

display_board <- function(board) {
  # Print numeric representation
  print(board)
  
  # Create visual plot
  plot(0, 0, type = "n", xlim = c(0.5, 7.5), ylim = c(0.5, 6.5),
       xlab = "", ylab = "", xaxt = "n", yaxt = "n",
       main = "Connect Four Board")
  
  # Add grid
  grid(nx = 7, ny = 6, col = "gray")
  
  # Add column numbers
  axis(1, at = 1:7, labels = 1:7)
  
  # Plot pieces
  for (row in 1:6) {
    for (col in 1:7) {
      if (board[row, col] != 0) {
        points(col, 7-row, pch = 19, cex = 3,
               col = ifelse(board[row, col] == 1, "red", "yellow"))
      } else {
        points(col, 7-row, pch = 1, cex = 3, col = "black")
      }
    }
  }
}

drop_piece <- function(board, column, player) {
  if (!(column %in% 1:7)) 
    stop("column needs to be between 1 and 7.")
  
  for (row in nrow(board):1) {
    if (board[row, column] == 0) {
      board[row, column] <- player
      return(board)
    }
  }
  stop("Column is full!")
}

check_winner <- function(board, player) {
  # Horizontal check
  for (row in 1:6) {
    for (col in 1:4) {
      if (all(board[row, col:(col+3)] == player)) 
        return(TRUE)
    }
  }
  
  # Vertical check
  for (row in 1:3) {
    for (col in 1:7) {
      if (all(board[row:(row+3), col] == player)) 
        return(TRUE)
    }
  }
  
  # Diagonal check (positive slope)
  for (row in 4:6) {
    for (col in 1:4) {
      if (all(diag(board[row:(row-3), col:(col+3)]) == player)) 
        return(TRUE)
    }
  }
  
  # Diagonal check (negative slope)
  for (row in 1:3) {
    for (col in 1:4) {
      if (all(diag(board[row:(row+3), col:(col+3)]) == player)) 
        return(TRUE)
    }
  }
  
  return(FALSE)
}


# This implementation of a Connect 4 AI uses the minimax algorithm with alpha-beta pruning 
# to determine the best move for the AI player. The minimax algorithm is a decision-making
# algorithm used  to evaluate all possible moves by assuming one player maximizes their 
# advantage while the other minimizes it, which leads to an optimal strategy. 
# Alpha-beta pruning helps this by eliminating branches of the search tree that cannot 
# influence the final decision, which helps improve efficiency without affecting the
# outcome. Connect 4 is a solved game, meaning that if both players play perfectly, the first
# player to place in the middle column will always win, assuming they play the best possible moves. 
# If the first player does not place in the center column, a perfectly played game ends in
# a draw.
#
# The AI uses a combination of game state evaluation, strategic heuristics, and the minimax algorithm
# with depth-based exploration to make optimal moves in each turn.

# To play the game, "source" the game first, then enter "play_connect_four(player_1_human = TRUE)"
# into the console.


######################################
############## AI CODE ###############
######################################

ai_connect4 <- function(board) {
  # Helper functions
  
  # Function to check if there's a winning position for a given piece (1 or 2)
  winning_move <- function(board, piece) {
    ROWS <- nrow(board)
    COLS <- ncol(board)
    output <- FALSE
    
    for (r in 1:ROWS) {
      for (c in 1:(COLS-3)) {
        if (all(board[r, c:(c+3)] == piece)) {
          output <- TRUE
        } 
      }
    }
    
    for (r in 1:(ROWS-3)) {
      for (c in 1:COLS) {
        if (all(board[r:(r+3), c] == piece)) {
          output <- TRUE
        } 
      }
    }
    
    for (r in 4:ROWS) {
      for (c in 1:(COLS-3)) {
        if (all(sapply(0:3, function(i) board[r-i, c+i] == piece))) {
          output <- TRUE
        }
      }
    }
    
    for (r in 4:ROWS) {
      for (c in 4:COLS) {
        if (all(sapply(0:3, function(i) board[r-i, c-i] == piece))) {
          output <- TRUE
        }
      }
    }
    output
  }
  
  # Function to evaluate a window of 4 positions and assign a score
  evaluate_window <- function(window, piece) {
    opponent_piece <- ifelse(piece == 1, 2, 1)
    score <- 0
    
    if (sum(window == piece) == 4) {
      score <- score + 100
    } else if (sum(window == piece) == 3 && sum(window == 0) == 1) {
      score <- score + 5
    } else if (sum(window == piece) == 2 && sum(window == 0) == 2) {
      score <- score + 2
    }
    if (sum(window == opponent_piece) == 3 && sum(window == 0) == 1) {
      score <- score - 4
    }
    score
  }
  
  # Function to calculate overall score for the entire board
  score_position <- function(board, piece) {
  score <- 0
  ROWS <- nrow(board)
  COLS <- ncol(board)
  
  # Prioritize center column for higher chance of winning
  center_col <- board[, COLS %/% 2 + 1]
  score <- score + sum(center_col == piece) * 6
  
  # Horizontal windows
  for (r in 1:ROWS) {
    for (c in 1:(COLS - 3)) {
      window <- board[r, c:(c + 3)]
      score <- score + evaluate_window(window, piece)
    }
  }
  
  # Vertical windows
  for (c in 1:COLS) {
    for (r in 1:(ROWS - 3)) {
      window <- board[r:(r + 3), c]
      score <- score + evaluate_window(window, piece)
    }
  }
  
  # Diagonal windows (upward)
  for (r in 1:(ROWS - 3)) {
    for (c in 1:(COLS - 3)) {
      window <- c(board[r, c], board[r + 1, c + 1], board[r + 2, c + 2], board[r + 3, c + 3])
      score <- score + evaluate_window(window, piece)
    }
  }
  
  # Diagonal windows (downward)
  for (r in 4:ROWS) {
    for (c in 1:(COLS - 3)) {
      window <- c(board[r, c], board[r - 1, c + 1], board[r - 2, c + 2], board[r - 3, c + 3])
      score <- score + evaluate_window(window, piece)
    }
  }
  
  score
  }

  # The algorithm calculating the best move to make given a depth of the search tree.
  # Depth is how many layers algorithm scores boards. Complexity grows exponentially.
  # Alpha and beta are best scores a side can achieve assuming the opponent makes the best play.
  # More on alpha-beta pruning here: https://www.youtube.com/watch?v=l-hh51ncgDI.
  # maximizing_palyer is a boolean value that tells whether we are maximizing or minimizing
  # in this implementation, AI is maximizing.
  
  # Minimax algorithm with alpha-beta pruning for determining best move
  minimax <- function(board, depth, alpha, beta, maximizing_player) {
    # all valid locations on the board
    valid_locations <- get_valid_locations(board)
    # tells if the current board is terminal
    is_terminal_node <- is_terminal(board)
    
    # if the board is terminal or depth == 0
    # we score the win very high and a draw as 0
    result <- list(col=NULL, value=NULL)
    if (depth == 0 || is_terminal_node) {
      if (is_terminal_node) {
        if (winning_move(board, 2)) {
          result <- list(col = NULL, value = 1000000)
        } else if (winning_move(board, 1)) {
          result <- list(col = NULL, value = -1000000)
        } else {
          result <- list(col = NULL, value = 0)
        }
      # if depth is zero, we score the current board
      } else {
        result <- list(col = NULL, value=score_position(board, 2))
      }
    # if the current board is not terminal and we are maximizing
    } else if (maximizing_player) {
      value <- -Inf
      column <- valid_locations[1] # this will be the optimal column
      
      # for every valid column, we simulate dropping a piece with the help of a board copy
      # and run the minimax on it with decreased depth and switched player
      for (col in valid_locations) {
        row <- max(which(board[,col] == 0))
        board_copy <- board
        board_copy[row, col] <- 2
        new_score <- minimax(board_copy, depth-1, alpha, beta, FALSE)$value
        # if the score for this column is better than what we already have
        if (new_score > value) {
          value <- new_score
          column <- col
        }
        # alpha is the best option!
        alpha <- max(alpha, value)
        # if current alpha better than beta opponent move, prune branch
        if (alpha >= beta) {
          break
        }
      }
      result <- list(col=column, value=value)
    # this is for minimizing player
    } else {
      value <- Inf
      column <- valid_locations[1]
      
      for (col in valid_locations) {
        row <- max(which(board[,col] == 0))
        board_copy <- board
        board_copy[row, col] <- 1
        new_score <- minimax(board_copy, depth-1, alpha, beta, TRUE)$value
        
        if (new_score < value) {
          value <- new_score
          column <- col
        }
        beta <- min(beta, value)
        if (alpha >= beta) break
      }
      result <- list(col=column, value=value)
    }
    result
  }
  
  # Function to get all valid moves (columns that aren't full)
  get_valid_locations <- function(board) {
    which(board[1,] == 0)
  }
  
  # Function to check if game is over (win or draw)
  is_terminal <- function(board) {
    winning_move(board, 1) || winning_move(board, 2) || length(get_valid_locations(board)) == 0
  }
  
  # Execute minimax algorithm with depth 5 to find best move; 
  # can change it depending on how much depth wanted
  col <- minimax(board, 5, -Inf, Inf, TRUE)$col
  col
}



######################################
############## Main Game #############
######################################

# Main game loop
# This function simulates a Connect Four game where a human player competes against an AI.
# Arguments:
# player_1_human: Logical (TRUE/FALSE). If TRUE, the human player is Player 1.
# Otherwise, the AI is Player 1.
# Returns:
# A list containing the winner of the game ("Human", "AI", or "Draw!") and the
# total number of turns played.
play_connect_four <- function(player_1_human = TRUE) {
  board <- initialize_board() # Initialize the empty game board
  player_1_turn <- TRUE # Track whose turn it is (Player 1 starts)
  winner <- "undecided" # Track the game status
  n_turns <- 0 # Count the number of turns
  # play until someone wins or the board is filled up resulting in a draw
  while (winner == "undecided") {
    display_board(board) # Display the current board state
    n_turns <- n_turns + 1 # Increment the turn counter
    # Determine the current player
    if ((player_1_human & n_turns %% 2 == 1) |
        (!player_1_human & n_turns %% 2 == 0)) {
      current_player <- "Human"
    } else {
      current_player <- "AI"
      4
    }
    # and what piece they are using
    if (player_1_turn) {
      piece <- 1
    } else {
      piece <- 2
    }
    if (current_player == "Human") {
      # human's turn
      cat("Your turn player ", piece, " ! Choose a column (1-7): ")
      column <- as.integer(readline())
      if (column < 1 || column > 7 || board[1, column] != 0) {
        cat("Invalid column. Try again.\n")
        next
      }
      board <- drop_piece(board = board, column = column, player = piece)
      if (check_winner(board = board, player = piece)) {
        display_board(board)
        cat("Congratulations! You win!\n")
        winner <- "Human"
      }
    } else {
      # AI's turn
      cat("AI (Player ", piece,") is thinking...\n")
      Sys.sleep(1)
      column <- ai_connect4(board)
      board <- drop_piece(board = board, column = column, player = piece)
      if (check_winner(board = board, player = piece)) {
        display_board(board)
        cat("AI (Player ", piece,") wins! Better luck next time.\n")
        winner <- "AI"
      }
    }
    # Check for draw
    if (!any(board == 0)) {
      display_board(board)
      cat("It's a draw!\n")
      winner <- "Draw!"
    }
    # Alternate turns
    player_1_turn <- !player_1_turn
  }
  # Return the game results
  list(winner = winner,
       n_turns = n_turns)
}