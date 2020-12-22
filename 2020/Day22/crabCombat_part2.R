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
mydata <- readLines('2020/Day22/input_test1.txt')

#------------------------------------------------------------------------------------------
#The Game
#------------------------------------------------------------------------------------------

#decks
player_idx <- which(grepl("Player", mydata, fixed = TRUE)==TRUE)

player1 <-as.numeric(mydata[(player_idx[1]+1):(player_idx[2]-2)])
player2 <-as.numeric(mydata[(player_idx[2]+1):(length(mydata))])

#rounds of the game
player1_wins <- function(player1,player2){
  prev_rounds <- list(player1, player2)
  while(length(player1)>0 && length(player2)>0){
    value_1 <- player1[1]
    value_2 <- player2[1]
    
    if(length(player1) >= value_1 && length(player2) >= value_2){
      
      result <- player1_wins(player1[1:value_1],player2[1:value_2])
      player1 <- result[[2]]
      player2 <- result[[3]]
      if (result[[1]]){
        player1 <- append(player1,value_1) 
        player1 <- append(player1,value_2) 
      }else{
        player2 <- append(player2,value_2) 
        player2 <- append(player2,value_1)
      }
      
    }else{
      if(value_1 > value_2){
        player1 <- append(player1,value_1) 
        player1 <- append(player1,value_2)
      }else{
        player2 <- append(player2,value_2) 
        player2 <- append(player2,value_1)
      }
    }
    
    if(length(player1)!=0 && length(player2)!=0){
      return(list(TRUE,player1,player2))
    }else{
      prev_rounds <- append(prev_rounds,list(player1, player2))
    }
    
    
  }
  
  if(length(player1)>length(player2)){
    return(list(TRUE,player1,player2))
  }else{return(list(FALSE,player1,player2))}
  
}


# score
calculate_score <- function(playerx){
  count = 0
  for(i in 1:length(playerx)){
    count = count+ playerx[i]*(length(playerx)-i)
  }
  return(count)
}



player1_wins(player1,player2)
calculate_score(player1_wins(player1,player2)[[2]]) #player 1
calculate_score(player1_wins(player1,player2)[[3]]) #player 2

