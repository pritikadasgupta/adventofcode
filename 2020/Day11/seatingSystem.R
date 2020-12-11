#!/usr/bin/env Rscript

#clear workspace
rm(list = ls())

#Libraries


#Set working directory and load in data
# data1 <- as.numeric(readLines(file("stdin"))) #read in file
setwd("~/Documents/adventofcode1/adventofcode/2020/Day11")
data1 <- readLines("exercise11.input.txt") #read in file


#Functions
#-------------------------------------------------------------------------

# If a seatnum is empty (L) and there are no occupied seatnums adjacent to it, the seatnum becomes occupied.
# If a seatnum is occupied (#) and four or more seatnums adjacent to it are also occupied, the seatnum becomes empty.
#   Otherwise, the seatnum's state does not change.

# Floor (.) never changes; seatnums don't move, and nobody sits on the floor.

main_layout_switcher <- function(seatnum,mylist,rownum){
  if(mylist[[rownum]][seatnum]=="L"){
    
    if(adjacency(seatnum,mylist,rownum)==0){
      mylist[[rownum]][seatnum]="#"
      return("#")
    }else{
      return("L")
    }
    
  }else if(mylist[[rownum]][seatnum]=="#"){
    
    if(adjacency(seatnum,mylist,rownum)>=4){
      mylist[[rownum]][seatnum]="L"
      print(adjacency(seatnum,mylist,rownum))
      return("L")
    }else if(adjacency(seatnum,mylist,rownum)<4){
      return("#")
    }else{
      return("#")
    }
    
  }else if(mylist[[rownum]][seatnum]=="."){
      return(".")
  }
}


adjacency <- function(seatnum,mylist,rownum){
  # pos1 <- 0 #left
  # pos2 <- 0 #right
  # pos3 <- 0 #above left
  # pos4 <- 0 #above 
  # pos5 <- 0 #above right
  # pos6 <- 0 #below left
  # pos7 <- 0 #below
  # pos8 <- 0 #below right
  
  #top row, left
  if(rownum==1 && seatnum==1){
    
    x2 <- mylist[[rownum]][seatnum+1]
    x7 <- mylist[[rownum+1]][seatnum]
    x8 <- mylist[[rownum+1]][seatnum+1]
    
    pos2 <- ifelse(x2!="#",0,1)
    pos7 <- ifelse(x7!="#",0,1)
    pos8 <- ifelse(x8!="#",0,1)
    
    pos1 <- 0 #left
    pos3 <- 0 #above left
    pos4 <- 0 #above 
    pos5 <- 0 #above right
    pos6 <- 0 #below left

    
    
  }else if(rownum==1 && seatnum==length(mylist[[rownum]])){#top row, right
    
    x1 <- mylist[[rownum]][seatnum-1]
    x6 <- mylist[[rownum+1]][seatnum-1]
    x7 <- mylist[[rownum+1]][seatnum]
    
    pos1 <- ifelse(x1!="#",0,1)
    pos6 <- ifelse(x6!="#",0,1)
    pos7 <- ifelse(x7!="#",0,1)

    pos2 <- 0 #right
    pos3 <- 0 #above left
    pos4 <- 0 #above 
    pos5 <- 0 #above right
    pos8 <- 0 #below right
    
  }else if(rownum==length(mylist) && seatnum==1){#bottom row, left
    
    x2 <- mylist[[rownum]][seatnum+1]
    x4 <- mylist[[rownum-1]][seatnum]
    x5 <- mylist[[rownum-1]][seatnum+1]
    
    pos2 <- ifelse(x2!="#",0,1)
    pos5 <- ifelse(x5!="#",0,1)
    pos4 <- ifelse(x4!="#",0,1)
    
    pos1 <- 0 #left
    pos3 <- 0 #above left
    pos6 <- 0 #below left
    pos7 <- 0 #below
    pos8 <- 0 #below right
    
  }else if(rownum==length(mylist) && seatnum==length(mylist[[rownum]])){
    
    x1 <- mylist[[rownum]][seatnum-1]
    x3 <- mylist[[rownum-1]][seatnum-1]
    x4 <- mylist[[rownum-1]][seatnum]
    
    pos1 <- ifelse(x1!="#",0,1)
    pos3 <- ifelse(x3!="#",0,1)
    pos4 <- ifelse(x4!="#",0,1)
    
    pos2 <- 0 #right
    pos5 <- 0 #above right
    pos6 <- 0 #below left
    pos7 <- 0 #below
    pos8 <- 0 #below right
    
  }else if(seatnum==1){#left
    
    x2 <- mylist[[rownum]][seatnum+1]
    x4 <- mylist[[rownum-1]][seatnum]
    x5 <- mylist[[rownum-1]][seatnum+1]
    x7 <- mylist[[rownum+1]][seatnum]
    x8 <- mylist[[rownum+1]][seatnum+1]
    
    pos2 <- ifelse(x2!="#",0,1)
    pos4 <- ifelse(x4!="#",0,1)
    pos5 <- ifelse(x5!="#",0,1)
    pos8 <- ifelse(x8!="#",0,1)
    pos7 <- ifelse(x7!="#",0,1)
    
    pos1 <- 0 #left
    pos3 <- 0 #above left
    pos6 <- 0 #below left
    
  }else if(seatnum==length(mylist[[rownum]])){ #right
    
    x1 <- mylist[[rownum]][seatnum-1]
    x3 <- mylist[[rownum-1]][seatnum-1]
    x4 <- mylist[[rownum-1]][seatnum]
    x6 <- mylist[[rownum+1]][seatnum-1]
    x7 <- mylist[[rownum+1]][seatnum]
    
    pos1 <- ifelse(x1!="#",0,1)
    pos3 <- ifelse(x3!="#",0,1)
    pos4 <- ifelse(x4!="#",0,1)
    pos6 <- ifelse(x6!="#",0,1)
    pos7 <- ifelse(x7!="#",0,1)
    
   
    pos2 <- 0 #right
    pos5 <- 0 #above right
    pos8 <- 0 #below right
    
  }else if(rownum==1){ #top
    
    x1 <- mylist[[rownum]][seatnum-1]
    x2 <- mylist[[rownum]][seatnum+1]
    x6 <- mylist[[rownum+1]][seatnum-1]
    x7 <- mylist[[rownum+1]][seatnum]
    x8 <- mylist[[rownum+1]][seatnum+1]
    
    pos1 <- ifelse(x1!="#",0,1)
    pos2 <- ifelse(x2!="#",0,1)
    pos6 <- ifelse(x6!="#",0,1)
    pos7 <- ifelse(x7!="#",0,1)
    pos8 <- ifelse(x8!="#",0,1)
    
    pos3 <- 0
    pos4 <- 0
    pos5 <- 0
    
  }else if(rownum==length(mylist)){ #bottom
    
    x1 <- mylist[[rownum]][seatnum-1]
    x2 <- mylist[[rownum]][seatnum+1]
    x3 <- mylist[[rownum-1]][seatnum-1]
    x4 <- mylist[[rownum-1]][seatnum]
    x5 <- mylist[[rownum-1]][seatnum+1]
    
    
    pos1 <- ifelse(x1!="#",0,1)
    pos2 <- ifelse(x2!="#",0,1)
    pos3 <- ifelse(x3!="#",0,1)
    pos4 <- ifelse(x4!="#",0,1)
    pos5 <- ifelse(x5!="#",0,1)
    
    pos6 <- 0
    pos7 <- 0
    pos8 <- 0
    
  }else{
    x1 <- mylist[[rownum]][seatnum-1]
    x2 <- mylist[[rownum+1]][seatnum+1]
    x3 <- mylist[[rownum-1]][seatnum-1]
    x4 <- mylist[[rownum-1]][seatnum]
    x5 <- mylist[[rownum-1]][seatnum+1]
    x6 <- mylist[[rownum+1]][seatnum-1]
    x7 <- mylist[[rownum+1]][seatnum]
    x8 <- mylist[[rownum+1]][seatnum+1]
    
    
    pos1 <- ifelse(x1!="#",0,1)
    pos2 <- ifelse(x2!="#",0,1)
    pos3 <- ifelse(x3!="#",0,1)
    pos4 <- ifelse(x4!="#",0,1)
    pos5 <- ifelse(x5!="#",0,1)
    pos6 <- ifelse(x6!="#",0,1)
    pos7 <- ifelse(x7!="#",0,1)
    pos8 <- ifelse(x8!="#",0,1)
  }
  
  
  return(sum(pos1,pos2,pos3,pos4,pos5,pos6,pos7,pos8,na.rm=TRUE))

}

switch_layout <- function(mylist){
  next_seatnum_layout <- mylist
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
counts <- vector()

#1st round
count2 = count_occ(cur_seatnum_layout)
nextlist <- switch_layout(cur_seatnum_layout)
listswitches = 1

# #second rounds
# count2 = count_occ(nextlist)
# nextlist <- switch_layout(nextlist)
# 
# count3 = count_occ(nextlist)
# nextlist <- switch_layout(nextlist)
# 
# 
# count4 = count_occ(nextlist)
# nextlist <- switch_layout(nextlist)
# 
# 
# count5 = count_occ(nextlist)
# nextlist <- switch_layout(nextlist)

while(listswitches<100){
  count1 = count2
  counts <- append(counts,count1)
  nextlist <- switch_layout(nextlist)
  count2 = count_occ(nextlist)
  listswitches = listswitches+1
}

counts
