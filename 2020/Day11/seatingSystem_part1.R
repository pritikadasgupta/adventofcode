#!/usr/bin/env Rscript

#clear workspace
rm(list = ls())

#Set working directory and load in data
# data1 <- as.numeric(readLines(file("stdin"))) #read in file
setwd("~/Documents/adventofcode1/adventofcode/2020/Day11")
data1 <- readLines("exercise11.input.txt") #read in file


#Functions
#-------------------------------------------------------------------------

# If a seatnum is empty (L) and there are no occupied seatnums adjacent to it, 
# the seatnum becomes occupied.

# If a seatnum is occupied (#) and four or more seatnums adjacent to it are also occupied, 
# the seatnum becomes empty.

# Otherwise, the seatnum's state does not change.

# Floor (.) never changes; seatnums don't move, and nobody sits on the floor.

main_layout_switcher <- function(seatnum,mylist,rownum){
  if(mylist[[rownum]][seatnum]=="L"){
    the_adjacency <- adjacency(seatnum,mylist,rownum)
    if(the_adjacency==0){
      return("#")
    }else{
      return("L")
    }
    
  }else if(mylist[[rownum]][seatnum]=="#"){
    the_adjacency <- adjacency(seatnum,mylist,rownum)
    if(the_adjacency>=4){
      return("L")
    }else if(the_adjacency<4){
      return("#")
    }
    
  }else if(mylist[[rownum]][seatnum]=="."){
      return(".")
  }
}

position_value <- function(x){
  if(x == "L"){
    return(0)
  }else if(x == "#"){
    return(1)
  }else if(x=="."){
    return(NA)
  }
}

adjacency <- function(seatnum,mylist,rownum){
  pos1 <- NA
  pos2 <- NA
  pos3 <- NA
  pos4 <- NA
  pos5 <- NA
  pos6 <- NA
  pos7 <- NA
  pos8 <- NA
  
  #top row, left
  if(rownum==1 && seatnum==1){
    
    x2 <- mylist[[rownum]][seatnum+1]
    x7 <- mylist[[rownum+1]][seatnum]
    x8 <- mylist[[rownum+1]][seatnum+1]
    
    pos2 <- position_value(x2)
    pos7 <- position_value(x7)
    pos8 <- position_value(x8)
  
    
    pos1 <- NA #left
    pos3 <- NA #above left
    pos4 <- NA #above 
    pos5 <- NA #above right
    pos6 <- NA #below left

    
    
  }else if(rownum==1 && seatnum==length(mylist[[rownum]])){#top row, right
    
    x1 <- mylist[[rownum]][seatnum-1]
    x6 <- mylist[[rownum+1]][seatnum-1]
    x7 <- mylist[[rownum+1]][seatnum]
    
    pos1 <- position_value(x1)
    pos6 <- position_value(x6)
    pos7 <- position_value(x7)

    pos2 <- NA #right
    pos3 <- NA #above left
    pos4 <- NA #above 
    pos5 <- NA #above right
    pos8 <- NA #below right
    
  }else if(rownum==length(mylist) && seatnum==1){#bottom row, left
    
    x2 <- mylist[[rownum]][seatnum+1]
    x4 <- mylist[[rownum-1]][seatnum]
    x5 <- mylist[[rownum-1]][seatnum+1]
    
    pos2 <- position_value(x2)
    pos5 <- position_value(x5)
    pos4 <- position_value(x4)
    
    pos1 <- NA #left
    pos3 <- NA #above left
    pos6 <- NA #below left
    pos7 <- NA #below
    pos8 <- NA #below right
    
  }else if(rownum==length(mylist) && seatnum==length(mylist[[rownum]])){ #bottom row, right
    
    x1 <- mylist[[rownum]][seatnum-1]
    x3 <- mylist[[rownum-1]][seatnum-1]
    x4 <- mylist[[rownum-1]][seatnum]
    
    pos1 <- position_value(x1)
    pos3 <- position_value(x3)
    pos4 <- position_value(x4)
    
    pos2 <- NA #right
    pos5 <- NA #above right
    pos6 <- NA #below left
    pos7 <- NA #below
    pos8 <- NA #below right
    
  }else if(seatnum==1){#left
    
    x2 <- mylist[[rownum]][seatnum+1]
    x4 <- mylist[[rownum-1]][seatnum]
    x5 <- mylist[[rownum-1]][seatnum+1]
    x7 <- mylist[[rownum+1]][seatnum]
    x8 <- mylist[[rownum+1]][seatnum+1]
    
    pos2 <- position_value(x2)
    pos4 <- position_value(x4)
    pos5 <- position_value(x5)
    pos8 <- position_value(x8)
    pos7 <- position_value(x7)
    
    pos1 <- NA #left
    pos3 <- NA #above left
    pos6 <- NA #below left
    
  }else if(seatnum==length(mylist[[rownum]])){ #right
    
    x1 <- mylist[[rownum]][seatnum-1]
    x3 <- mylist[[rownum-1]][seatnum-1]
    x4 <- mylist[[rownum-1]][seatnum]
    x6 <- mylist[[rownum+1]][seatnum-1]
    x7 <- mylist[[rownum+1]][seatnum]
    
    pos1 <- position_value(x1)
    pos3 <- position_value(x3)
    pos4 <- position_value(x4)
    pos6 <- position_value(x6)
    pos7 <- position_value(x7)
    
   
    pos2 <- NA #right
    pos5 <- NA #above right
    pos8 <- NA #below right
    
  }else if(rownum==1){ #top
    
    x1 <- mylist[[rownum]][seatnum-1]
    x2 <- mylist[[rownum]][seatnum+1]
    x6 <- mylist[[rownum+1]][seatnum-1]
    x7 <- mylist[[rownum+1]][seatnum]
    x8 <- mylist[[rownum+1]][seatnum+1]
    
    pos1 <- position_value(x1)
    pos2 <- position_value(x2)
    pos6 <- position_value(x6)
    pos7 <- position_value(x7)
    pos8 <- position_value(x8)
    
    pos3 <- NA
    pos4 <- NA
    pos5 <- NA
    
  }else if(rownum==length(mylist)){ #bottom
    
    x1 <- mylist[[rownum]][seatnum-1]
    x2 <- mylist[[rownum]][seatnum+1]
    x3 <- mylist[[rownum-1]][seatnum-1]
    x4 <- mylist[[rownum-1]][seatnum]
    x5 <- mylist[[rownum-1]][seatnum+1]
    
    
    pos1 <- position_value(x1)
    pos2 <- position_value(x2)
    pos3 <- position_value(x3)
    pos4 <- position_value(x4)
    pos5 <- position_value(x5)
    
    pos6 <- NA
    pos7 <- NA
    pos8 <- NA
    
    
  }else{
    x1 <- mylist[[rownum]][seatnum-1]
    x2 <- mylist[[rownum]][seatnum+1]
    x3 <- mylist[[rownum-1]][seatnum-1]
    x4 <- mylist[[rownum-1]][seatnum]
    x5 <- mylist[[rownum-1]][seatnum+1]
    x6 <- mylist[[rownum+1]][seatnum-1]
    x7 <- mylist[[rownum+1]][seatnum]
    x8 <- mylist[[rownum+1]][seatnum+1]
    
    
    pos1 <- position_value(x1)
    pos2 <- position_value(x2)
    pos3 <- position_value(x3)
    pos4 <- position_value(x4)
    pos5 <- position_value(x5)
    pos6 <- position_value(x6)
    pos7 <- position_value(x7)
    pos8 <- position_value(x8)

  }
  
  
  return(sum(pos1,pos2,pos3,pos4,pos5,pos6,pos7,pos8,na.rm=TRUE))

}

switch_layout <- function(mylist){
  # next_seatnum_layout <- mylist
  
  next_seatnum_rown <- length(mylist)
  next_seatnum_coln <- length(mylist[[1]])
  
  next_seatnum_layout <- vector(mode = "list", length = next_seatnum_rown)
  
  for(rown in 1:length(mylist)){
    next_seatnum_layout[[rown]] <- rep(NA,next_seatnum_coln)
  }
  
  
  for(rown in 1:length(mylist)){
    for(seatn in 1:length(mylist[[1]])){
      next_seatnum_layout[[rown]][[seatn]] <- main_layout_switcher(seatn,mylist,rown)
    }
  }
  return(next_seatnum_layout)
}

count_occ <- function(mylist){
  counts <-0
  for(rown in 1:length(mylist)){
    for(seatn in 1:length(mylist[[1]])){
      counts = counts+length(which(mylist[[rown]][[seatn]]=="#"))
    }
  }
  return(counts)
}

#Clean up data
#-------------------------------------------------------------------------

cur_seatnum_layout <- list()
for(k in 1:length(data1)){
  cur_seatnum_layout <- append(cur_seatnum_layout,strsplit(data1[k],""))
}

#-------------------------------------------------------------------------
#PART 1
#-------------------------------------------------------------------------
countsv <- vector()

#1st round
count1 = count_occ(cur_seatnum_layout)
nextlist <- switch_layout(cur_seatnum_layout)
count2 = count_occ(nextlist)
listswitches = 1

while((count2-count1)!=0){
  count1 = count2
  countsv <- append(countsv,count1)
  nextlist <- switch_layout(nextlist)
  count2 = count_occ(nextlist)
  listswitches = listswitches+1
  # print(nextlist)
  
}

#part 1 soln:
print(paste("Part 1:",countsv[length(countsv)]))

