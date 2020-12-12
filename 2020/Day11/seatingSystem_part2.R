#!/usr/bin/env Rscript

#clear workspace
rm(list = ls())

#Set working directory and load in data
# data1 <- as.numeric(readLines(file("stdin"))) #read in file
setwd("~/Documents/adventofcode1/adventofcode/2020/Day11")
data1 <- readLines("exercise11.testinput.txt") #read in file


#Functions
#-------------------------------------------------------------------------


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
    if(the_adjacency>=5){
      return("L")
    }else if(the_adjacency<5){
      return("#")
    }
    
  }else if(mylist[[rownum]][seatnum]=="."){
      return(".")
  }
}

position_value <- function(x){
  if(length(x)==0){
    return(NA)
  }else if(x == "L"){
    return(0)
  }else if(x == "#"){
    return(1)
  }else if(x=="."){
    return(NA)
  }
}

adjacency <- function(seatnum,mylist,rownum){
  pos1_value <- 0
  pos2_value <- 0
  pos3_value <- 0
  pos4_value <- 0
  pos5_value <- 0
  pos6_value <- 0
  pos7_value <- 0
  pos8_value <- 0
  
  rownum_perm <- rownum
  seatnum_perm <- seatnum
  times = 0
    #position1:
    while(times < length(mylist)){
      seatnum <- max(2,seatnum-1)
      rownum <- max(1,rownum)
      pos1 <- position_value(mylist[[rownum]][seatnum-1])
      pos1_value <- position_value(mylist[[rownum]][seatnum-1])
      
      if(is.na(pos1_value)){
        times=times+1
      } else if(pos1_value=="#"){
        times = length(mylist)
      }else{
        times = times+1
      }
      
      if(rownum==1 && seatnum==1){
        pos1 <- NA #left
        pos1_value <- NA #left
      }else if(rownum==length(mylist) && seatnum==1){#bottom row, left
        pos1 <- NA #left
        pos1_value <- NA #left
      }else if(seatnum==1){#left
        pos1 <- NA #left
        pos1_value <- NA #left
      }
    }
  
  
    
    
  rownum <- rownum_perm
  seatnum <- seatnum_perm
  times = 0 
    #position2:
  while(times < length(mylist)){
      rownum <- max(1,rownum)
      seatnum <- min(length(mylist[[rownum]])-1,seatnum+1)
      pos2 <- position_value(mylist[[rownum]][seatnum+1])
      pos2_value <- position_value(mylist[[rownum]][seatnum+1])
      
      if(is.na(pos2_value)){
        times=times+1
      } else if(pos2_value=="#"){
        times = length(mylist)
      }else{
        times = times+1
      }
      
      if(rownum==1 && seatnum==length(mylist[[rownum]])){#top row, right
        pos2 <- NA #right
        pos2_value <- NA #right
      }else if(rownum==length(mylist) && seatnum==length(mylist[[rownum]])){ #bottom row, right
        pos2 <- NA #right
        pos2_value <- NA #right
      }else if(seatnum==length(mylist[[rownum]])){ #right
        pos2 <- NA #right
        pos2_value <- NA #right
      }
    }
    
  rownum <- rownum_perm
  seatnum <- seatnum_perm
  times = 0
    #position3:
  while(times < length(mylist)){
      seatnum <- max(2,seatnum-1)
      rownum <- max(2,rownum-1)
      pos3 <- position_value(mylist[[rownum-1]][seatnum-1])
      pos3_value <- position_value(mylist[[rownum-1]][seatnum-1])
      if(is.na(pos3_value)){
        times=times+1
      } else if(pos3_value=="#"){
        times = length(mylist)
      }else{
        times = times+1
      }
      
      if(rownum==1 && seatnum==1){
        pos3 <- NA #above left
        pos3_value <- NA #above left
      }else if(rownum==1 && seatnum==length(mylist[[rownum]])){
        pos3 <- NA #above left
        pos3_value <- NA #above left
      }else if(rownum==length(mylist) && seatnum==1){
        pos3 <- NA #above left
        pos3_value <- NA #above left
      }else if(seatnum==1){
        pos3 <- NA #above left
        pos3_value <- NA #above left
      }else if(rownum==1){
        pos3 <- NA #above left
        pos3_value <- NA #above left
      }
      
    }
    
  rownum <- rownum_perm
  seatnum <- seatnum_perm
  times=0
    #position4:
  while(times < length(mylist)){
      
      seatnum <- max(1,seatnum)
      rownum <- max(2,rownum-1)
      pos4 <- position_value(mylist[[rownum-1]][seatnum])
      pos4_value <- position_value(mylist[[rownum-1]][seatnum])
      if(is.na(pos4_value)){
        times=times+1
      } else if(pos4_value=="#"){
        times = length(mylist)
      }else{
        times = times+1
      }
      
      if(rownum==1 && seatnum==1){
        
        pos4 <- NA #above 
        pos4_value <- NA #above 
        
        
      }else if(rownum==1 && seatnum==length(mylist[[rownum]])){#top row, right
        
        
        pos4 <- NA #above 
        pos4_value <- NA #above 
        
      }else if(rownum==1){ #top
        
        pos4 <- NA #above 
        pos4_value <- NA #above 
        
      }
      
    }
    
  rownum <- rownum_perm
  seatnum <- seatnum_perm
  times = 0
    #position5:
  while(times < length(mylist)){
      seatnum <- min(seatnum+1,length(mylist[[rownum]])-1)
      rownum <- max(2,rownum-1)
      pos5 <- position_value(mylist[[rownum-1]][seatnum+1])
      pos5_value <- position_value(mylist[[rownum-1]][seatnum+1])
      if(is.na(pos5_value)){
        times=times+1
      } else if(pos5_value=="#"){
        times = length(mylist)
      }else{
        times = times+1
      }
      if(rownum==1 && seatnum==1){
        pos5 <- NA #above right
        pos5_value <- NA #above right
      }else if(rownum==1 && seatnum==length(mylist[[rownum]])){#top row, right
        pos5 <- NA #above right
        pos5_value <- NA #above right
        
      }else if(rownum==length(mylist) && seatnum==length(mylist[[rownum]])){ #bottom row, right
        pos5 <- NA #above right
        pos5_value <- NA #above right
        
      }else if(seatnum==length(mylist[[rownum]])){ #right
        pos5 <- NA #above right
        pos5_value <- NA #above right
        
      }else if(rownum==1){ #top
        pos5 <- NA #above right
        pos5_value <- NA #above right
        
      }
    }
    
  rownum <- rownum_perm
  seatnum <- seatnum_perm
  times = 0
    #position6:
  while(times < length(mylist)){
      seatnum <- max(2,seatnum-1)
      rownum <- min(length(mylist)-1,rownum+1)
      pos6 <- position_value(mylist[[rownum+1]][seatnum-1])
      pos6_value <- position_value(mylist[[rownum+1]][seatnum-1])
      if(is.na(pos6_value)){
        times=times+1
      } else if(pos6_value=="#"){
        times = length(mylist)
      }else{
        times = times+1
      }
      
      if(rownum==1 && seatnum==1){
        
        pos6 <- NA #below left
        
        pos6_value <- NA #below left
        
        
        
      }else if(rownum==length(mylist) && seatnum==1){#bottom row, left
        pos6 <- NA #below left
        
        pos6_value <- NA #below left
        
      }else if(rownum==length(mylist) && seatnum==length(mylist[[rownum]])){ #bottom row, right
        pos6 <- NA #below left
        
        pos6_value <- NA #below left
        
      }else if(seatnum==1){#left
        pos6 <- NA #below left
        
        pos6_value <- NA #below left
        
      }else if(rownum==length(mylist)){ #bottom
        pos6 <- NA #below left
        
        pos6_value <- NA #below left
      }
    }
    
  rownum <- rownum_perm
  seatnum <- seatnum_perm
  times = 0
    #position7:
  while(times < length(mylist)){
      
      seatnum <- max(1,seatnum)
      rownum <- min(length(mylist)-1,rownum+1)
      pos7 <- position_value(mylist[[rownum+1]][seatnum])
      pos7_value <- position_value(mylist[[rownum+1]][seatnum])
      if(is.na(pos7_value)){
        times=times+1
      } else if(pos7_value=="#"){
        times = length(mylist)
      }else{
        times = times+1
      }
      if(rownum==length(mylist) && seatnum==1){#bottom row, left
        pos7 <- NA #below
        pos7_value <- NA #below
        
      }else if(rownum==length(mylist) && seatnum==length(mylist[[rownum]])){ #bottom row, right
        pos7 <- NA #below
        pos7_value <- NA #below
        
      }else if(rownum==length(mylist)){ #bottom
        pos7 <- NA #below
        pos7_value <- NA #below
      }
    }
    
  rownum <- rownum_perm
  seatnum <- seatnum_perm
  times = 0
  while(times < length(mylist)){
      
      seatnum <- min(length(mylist[[rownum]])-1,seatnum+1)
      rownum <- min(length(mylist)-1,rownum+1)
      pos8 <- position_value(mylist[[rownum+1]][seatnum+1])
      pos8_value <- position_value(mylist[[rownum+1]][seatnum+1])
      if(is.na(pos8_value)){
        times=times+1
      } else if(pos8_value=="#"){
        times = length(mylist)
      }else{
        times = times+1
      }
      if(rownum==1 && seatnum==length(mylist[[rownum]])){#top row, right
        pos8 <- NA #below right
        pos8_value <- NA #below right
        
      }else if(rownum==length(mylist) && seatnum==1){#bottom row, left
        pos8 <- NA #below right
        pos8_value <- NA #below right
        
      }else if(rownum==length(mylist) && seatnum==length(mylist[[rownum]])){ #bottom row, right
        pos8 <- NA #below right
        pos8_value <- NA #below right
        
      }else if(seatnum==length(mylist[[rownum]])){ #right
        pos8 <- NA #below right
        pos8_value <- NA #below right
        
      }else if(rownum==length(mylist)){ #bottom
        pos8 <- NA #below right
        pos8_value <- NA #below right
      }
  }
  
  
  
  

  return(sum(pos1,pos2,pos3,pos4,pos5,pos6,pos7,pos8,na.rm=TRUE))

}

switch_layout <- function(mylist){
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
#PART 2
#-------------------------------------------------------------------------
countsv <- vector()

#1st round
count1 = count_occ(cur_seatnum_layout)
nextlist <- switch_layout(cur_seatnum_layout)

mylist <- nextlist
rownum=1
seatnum=1

count2 = count_occ(nextlist)
listswitches = 1

# while((count2-count1)!=0){
while(listswitches < 20){
  count1 = count2
  countsv <- append(countsv,count1)
  nextlist <- switch_layout(nextlist)
  count2 = count_occ(nextlist)
  listswitches = listswitches+1
  # print(nextlist)
  
}

#part 2 soln:
print(paste("Part 2:",countsv[length(countsv)]))

