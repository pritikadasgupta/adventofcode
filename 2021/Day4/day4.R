#!/usr/bin/env Rscript
# install.packages("reader")
library("reader")

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
#Use this if you're opening this repo as a R project, using relative paths:
mydata <- readLines("2021/Day4/input.txt")
mydata_order <- as.numeric(strsplit(readLines("2021/Day4/input.txt",n=1),",",fixed=TRUE)[[1]])

mydata_boards <- list()
for(i in seq(from=1,to=length(mydata),by=6)[1:(round(length(mydata)/6))]){
  mydata_boards <- c(mydata_boards,list(n.readLines("2021/Day4/input.txt",n=6,skip=i)))
}

example <- readLines("2021/Day4/example.txt")
example_order <- as.numeric(strsplit(readLines("2021/Day4/example.txt",n=1),",",fixed=TRUE)[[1]])
example_boards <- list()
for(i in seq(from=1,to=length(example),by=6)[1:(round(length(example)/6))]){
  example_boards <- c(example_boards,list(n.readLines("2021/Day4/example.txt",n=6,skip=i)))
}
#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------

bingo <- function(x,y){ #x=boards, y=order
  mat_list <- list()
  for(i in 1:length(x)){
    board <- vector()
    for(j in 1:length(x[[i]])){
      line <- as.numeric(scan(text=x[[i]][j],what=""))
      board <- append(board,line)
    }
    mat_list[[length(mat_list)+1]] <- matrix(board,ncol=5,nrow=5,byrow=TRUE)
  }
  
  #empty boards
  marked <- map(mat_list, ~ {.x[] = rep(0, length(.x));  .x})
  
  for(k in 1:length(y)){
    #mark boards
    for(board_num in 1:length(mat_list)){
      for(r in 1:nrow(mat_list[[board_num]])){
        for(c in 1:ncol(mat_list[[board_num]])){
          if(y[k]==mat_list[[board_num]][r,c]){
            marked[[board_num]][r,c] <- 1
          }
        }
      }
    }
    
    #check for wins
    for(board_num in 1:length(mat_list)){
      row_check <- which(rowSums(marked[[board_num]])==5)
      col_check <- which(colSums(marked[[board_num]])==5)
      if(length(row_check)!=0 | length(col_check)!=0){
        winning_num <- y[k]
        winning_sum <- sum(mat_list[[board_num]][marked[[board_num]]==0])
        k <- length(y)+1
        print(winning_num)
        return(winning_sum*winning_num)
      }
    }
    
    
  }
  
}
bingo(example_boards,example_order)
bingo(mydata_boards,mydata_order)

#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------
bingo_squid <- function(x,y){ #x=boards, y=order
  mat_list <- list()
  for(i in 1:length(x)){
    board <- vector()
    for(j in 1:length(x[[i]])){
      line <- as.numeric(scan(text=x[[i]][j],what=""))
      board <- append(board,line)
    }
    mat_list[[length(mat_list)+1]] <- matrix(board,ncol=5,nrow=5,byrow=TRUE)
  }
  
  #empty boards
  marked <- map(mat_list, ~ {.x[] = rep(0, length(.x));  .x})
  
  boards_won <- vector()
  for(k in 1:length(y)){
    #mark boards
    for(board_num in 1:length(mat_list)){
      for(r in 1:nrow(mat_list[[board_num]])){
        for(c in 1:ncol(mat_list[[board_num]])){
          if(y[k]==mat_list[[board_num]][r,c]){
            marked[[board_num]][r,c] <- 1
          }
        }
      }
    }
    
    #check for wins
    for(board_num in 1:length(mat_list)){
      row_check <- which(rowSums(marked[[board_num]])==5)
      col_check <- which(colSums(marked[[board_num]])==5)
      if(length(row_check)!=0 | length(col_check)!=0){

          if(!board_num %in% boards_won){
            boards_won <- append(boards_won,board_num)
          }
          
          if(length(boards_won)==length(mat_list)){
            winning_num <- y[k]
            winning_sum <- sum(mat_list[[board_num]][marked[[board_num]]==0])
            return(winning_sum*winning_num)
          }
      }
    }
  }
}
bingo_squid(example_boards,example_order)
bingo_squid(mydata_boards,mydata_order)

