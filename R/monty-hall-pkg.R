#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title
#' Select a random door.
#'
#' @description
#' select_door() simulates the contestant's first action in the Monty Hall
#' problem, choosing one of the three doors at random.
#'
#' @details
#' The contestant does not know what is behind each door and has an equal
#' chance of selecting door 1, 2, or 3.
#' @param ... no arguments are used by the function
#'
#' @return A single numeric value (1, 2, or 3) representing the door chosen.
#'
#' @examples
#' select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title Open a goat door.
#'
#' @description
#' open_goat_door() simulates the host's action of opening one of the doors
#' to reveal a goat.
#'
#' @details
#' If the contestant selected the car initially, the host randomly opens
#' one of the two remaining goat doors.
#' If the contestant selected a goat initially, the host opens the other
#' goat door that was not selected.
#'
#' @param
#' game: A character vector of length 3 created by create_game(),
#' containing two "goat" values and one "car".
#' a.pick: A numeric value (1, 2, or 3) indicating the contestant's
#'   chosen door.
#'
#' @return A single numeric value (1, 2, or 3) representing the door opened
#'   by the host.
#'
#' @examples
#' game <- create_game()
#' first.pick <- select_door()
#' open_goat_door(game, first.pick)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title Change or stay with chosen door.
#'
#' @description
#' change_door() simulates the contestant’s decision to either stay with
#' their original door or switch to the remaining unopened door.
#'
#' @details
#' If stay = TRUE, the contestant keeps their original pick.
#' If stay = FALSE, the contestant switches to the unopened door that is
#' neither the host’s opened door nor their original pick.
#'
#' @param
#' stay: Logical; TRUE if the contestant stays with their first pick,
#'   FALSE if they switch.
#' opened.door: A numeric value (1, 2, or 3) representing the door opened
#'   by the host to reveal a goat.
#' a.pick: A numeric value (1, 2, or 3) indicating the contestant's
#'   original chosen door.
#'
#' @return A single numeric value (1, 2, or 3) representing the door opened
#'   by the host.
#'
#' @examples
#' #' @examples
#' game <- create_game()
#' first.pick <- select_door()
#' opened.door <- open_goat_door(game, first.pick)
#' change_door(stay = FALSE, opened.door = opened.door, a.pick = first.pick)

#'
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3)

  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ]
  }

  return( final.pick )  # number between 1 and 3
}



#' @title Determine if the contestant won.
#'
#' @description
#' determine_winner() checks whether the contestant’s final door choice
#' contains the car or a goat.
#'
#' @details
#' The function evaluates the contestant's final door selection against
#' the hidden game setup.
#'
#' @param
#' final.pick: A numeric value (1, 2, or 3) representing the contestant’s final chosen door.
#' game: A character vector of length 3 created by create_game(), containing two "goat" values and one "car".
#'
#' @return A character string, either "WIN" if the contestant selected the car
#'   or "LOSE" if they selected a goat.
#'
#' @examples
#'   game <- create_game()
#'   final.pick <- 2
#'   determine_winner(final.pick, game)
#'
#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}





#' @title Play a single Monty Hall game.
#'
#' @description
#' play_game() simulates one full run of the Monty Hall problem with both
#' strategies: staying with the first choice and switching to the other door.
#'
#' @details
#'  The function runs all steps of the game in sequence:
#'   1. Create a new game: create_game()
#'   2. Contestant makes an initial choice: select_door()
#'   3. Host opens a goat door: open_goat_door()
#'   4. Contestant decides whether to stay or switch: change_door()
#'   5. Outcomes are determined for both strategies: determine_winner()
#'
#'   Results are returned in a data frame comparing the two strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return
#' A data frame with two columns:
#' strategy: character string ("stay" or "switch")
#' outcome: character string ("WIN" or "LOSE")
#'
#' @examples play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}



#' @title Simulate multiple Monty Hall games.

#' @description
#' play_n_games() runs repeated simulations of the Monty Hall problem
#' using both strategies (stay or switch) and summarizes the outcomes.
#'
#' @details
#' The function automates the process of playing the game multiple times.
#' For each trial, it calls play_game() to simulate one full game.
#' Results are collected in a list, combined into a single data frame,
#' and then summarized in a contingency table showing the proportion
#' of wins and losses for each strategy.
#'
#' @param n: Integer. The number of games to simulate (default = 100).
#'
#' @return
#' A data frame with two columns for all simulated games:
#'   - strategy: character string ("stay" or "switch")
#'   - outcome: character string ("WIN" or "LOSE")
#'
#'   A summary table of win/loss proportions by strategy is also printed
#'   to the console.
#'
#' @examples
#' Run 10 games
#' results <- play_n_games(10)

#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}
