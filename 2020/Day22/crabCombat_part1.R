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

new_player1 <- player1
new_player2 <- player2

#rounds
rounds = 1
a = 1
while(rounds >=a){
  
  # current round and player deck status
  print(paste("-- Round",rounds,"--"),sep="")
  print(paste("Player 1's deck:",paste(player1,collapse=" ")))
  print(paste("Player 2's deck:",paste(player2,collapse=" ")))

  #play
  if(length(player1)!=0){
    print(paste("Player 1 plays:",paste(player1[1],collapse=" ")))
  }
  if(length(player2)!=0){
    print(paste("Player 2 plays:",paste(player2[1],collapse=" ")))
  }
  
  #   Player x wins the round! 
  if((length(player1)!=0 && length(player2)!=0)){
    if((player1[1] > player2[1])){
      print("Player 1 wins the round")
      
      new_player1 <- append(new_player1,player1[1])
      new_player1 <- append(new_player1,player2[1])
      
      new_player1 <- new_player1[-1]
      new_player2 <- new_player2[-1]
      
    }else if((player2[1] > player1[1])){
      print("Player 2 wins the round")
      new_player2 <- append(new_player2,player2[1])
      new_player2 <- append(new_player2,player1[1])
      
      new_player2 <- new_player2[-1]
      new_player1 <- new_player1[-1]
    }
  }
  
  
  
  if(length(player1)==0 || length(player2)==0){
    a=0
    rounds=rounds+1
    print("== Post-game results ==")
    print(paste("Player 1's deck:",paste(new_player1,collapse=" ")))
    print(paste("Player 2's deck:",paste(new_player2,collapse=" ")))
    break
    
  }else{
    a=1
    rounds=rounds+1
    player1 <- new_player1
    player2 <- new_player2
  }

}


# score
calculate_score <- function(playerx){
  if(length(playerx)!=0){
    playerx <- as.numeric(playerx)
    to_multiply <- seq(length(playerx),1,by=-1)
    score_ <- playerx*to_multiply
    return(sum(score_))
  }else{
    return(0)
  }
}

calculate_score(player1)
calculate_score(player2)


