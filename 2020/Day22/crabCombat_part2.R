#!/usr/bin/Rscript

#clear workspace
rm(list = ls())

# Libraries

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------

#Use this if you're running from the command line:
# mydata <- strsplit(readLines(file("stdin"))," ")

#Use this if you're opening this repo as a R project, using relative paths:
# mydata <- readLines('2020/Day22/input_test1.txt')
mydata <- readLines('2020/Day22/input.txt')

#------------------------------------------------------------------------------------------
#The Game
#------------------------------------------------------------------------------------------

#decks
player_idx <- which(grepl("Player", mydata, fixed = TRUE)==TRUE)

player1 <-as.numeric(mydata[(player_idx[1]+1):(player_idx[2]-2)])
player2 <-as.numeric(mydata[(player_idx[2]+1):(length(mydata))])
subgame = FALSE

#heavily inspired from:
# https://selbydavid.com/2020/12/06/advent-2020/#day22
recursive_combat <- function(player1, player2, subgame) {
  if (subgame & (max(player1) > max(player2))){
    return(player1)
  }
  
  played <- NULL
  
  while (length(player1) & length(player2)) {
    config <- paste(player1, player2, collapse = ',', sep = '|')
    if (config %in% played) return(player1)
    played <- c(played, config)
    
    if (player1[1] < length(player1) & player2[1] < length(player2)) {
      win1 <- all( recursive_combat(head(player1[-1], player1[1]),
                                    head(player2[-1], player2[1]),
                                    subgame = TRUE) > 0 )
    } else win1 <- player1[1] > player2[1]
    
    result1 <- c(player1[-1], player1[1][win1], player2[1][win1])
    result2 <- c(player2[-1], player2[1][!win1], player1[1][!win1])
    
    player1 <- result1
    player2 <- result2
  }
  
  if (!length(player1)) return(-player2)
  player1
}


# score
calculate_score <- function(result){
  return(sum(rev(result) * seq_along(result)))
}

print(calculate_score(recursive_combat(player1,player2,FALSE)))

