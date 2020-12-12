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
  if (!is.null(x) && 
      length(x) == 1 && 
      !is.na(x)) { 
    if(x == "L"){
      return(0)
    }else if(x == "#"){
      return(1)
    }else if(x=="."){
      return(NA)
    }
  }else{
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
      
      x <- tryCatch(expr = {                      # Specifying expression
        mylist[[rownum]][seatnum-1]
        message("Everything was fine.")
      },error = function(e){          # Specifying error message
        message("There was an error message.")
      },warning = function(w){        # Specifying warning message
        message("There was a warning message.")
      },finally = {                   # Specifying final message
        message("tryCatch is finished.")
      }
      )
      
      pos1 <- position_value(x)
      seatnum <- seatnum-1
      rownum <- rownum
      
      x <- tryCatch(expr = {                      # Specifying expression
        mylist[[rownum]][seatnum-1]
        message("Everything was fine.")
      },error = function(e){          # Specifying error message
        message("There was an error message.")
      },warning = function(w){        # Specifying warning message
        message("There was a warning message.")
      },finally = {                   # Specifying final message
        message("tryCatch is finished.")
      }
      )
      
      pos1_value <- position_value(x)
      
      if(is.na(pos1_value)){
        times=times+1
      } else if(pos1_value=="#"){
        pos1=pos1_value
        times = length(mylist)
      }else{
        times = times+1
      }
    }

  rownum <- rownum_perm
  seatnum <- seatnum_perm
  times = 0 
    #position2:
  while(times < length(mylist)){
    
    x <- tryCatch(expr = {                      # Specifying expression
      mylist[[rownum]][seatnum+1]
      message("Everything was fine.")
    },error = function(e){          # Specifying error message
      message("There was an error message.")
    },warning = function(w){        # Specifying warning message
      message("There was a warning message.")
    },finally = {                   # Specifying final message
      message("tryCatch is finished.")
    }
    )
    pos2 <- position_value(x)
    
    
      rownum <- rownum
      seatnum <- seatnum+1
      
      x <- tryCatch(expr = {                      # Specifying expression
        mylist[[rownum]][seatnum+1]
        message("Everything was fine.")
      },error = function(e){          # Specifying error message
        message("There was an error message.")
      },warning = function(w){        # Specifying warning message
        message("There was a warning message.")
      },finally = {                   # Specifying final message
        message("tryCatch is finished.")
      }
      )
      
      pos2_value <- position_value(x)
      
      if(is.na(pos2_value)){
        times=times+1
      } else if(pos2_value=="#"){
        times = length(mylist)
        pos2=pos2_value
      }else{
        times = times+1
      }
      
      
    }
    
  rownum <- rownum_perm
  seatnum <- seatnum_perm
  times = 0
    #position3:
  while(times < length(mylist)){
    
    x <- tryCatch(expr = {                      # Specifying expression
        mylist[[rownum-1]][seatnum-1]
        message("Everything was fine.")
      },error = function(e){          # Specifying error message
        message("There was an error message.")
      },warning = function(w){        # Specifying warning message
        message("There was a warning message.")
      },finally = {                   # Specifying final message
        message("tryCatch is finished.")
      }
    )
    
    # x <- tryCatch(mylist[[rownum-1]][seatnum-1],finally="hi")
    pos3 <- position_value(x)
      seatnum <- seatnum-1
      rownum <- rownum-1
      
      x <- tryCatch(expr = {                      # Specifying expression
        mylist[[rownum-1]][seatnum-1]
        message("Everything was fine.")
      },error = function(e){          # Specifying error message
        message("There was an error message.")
      },warning = function(w){        # Specifying warning message
        message("There was a warning message.")
      },finally = {                   # Specifying final message
        message("tryCatch is finished.")
      }
      )
    
      pos3_value <- position_value(x)
      if(is.na(pos3_value)){
        times=times+1
      } else if(pos3_value=="#"){
        times = length(mylist)
        pos3=pos3_value
      }else{
        times = times+1
      }
      
      
      
    }
    
  rownum <- rownum_perm
  seatnum <- seatnum_perm
  times=0
    #position4:
  while(times < length(mylist)){
    x <- tryCatch(expr = {                      # Specifying expression
      mylist[[rownum-1]][seatnum]
      message("Everything was fine.")
    },error = function(e){          # Specifying error message
      message("There was an error message.")
    },warning = function(w){        # Specifying warning message
      message("There was a warning message.")
    },finally = {                   # Specifying final message
      message("tryCatch is finished.")
    }
    )
    pos4 <- position_value(x)
      seatnum <- seatnum
      rownum <- rownum-1
      
      x <- tryCatch(expr = {                      # Specifying expression
        mylist[[rownum-1]][seatnum]
        message("Everything was fine.")
      },error = function(e){          # Specifying error message
        message("There was an error message.")
      },warning = function(w){        # Specifying warning message
        message("There was a warning message.")
      },finally = {                   # Specifying final message
        message("tryCatch is finished.")
      }
      )
      pos4_value <- position_value(x)
      if(is.na(pos4_value)){
        times=times+1
      } else if(pos4_value=="#"){
        times = length(mylist)
        pos4=pos4_value
      }else{
        times = times+1
      }
      
      
      
    }
    
  rownum <- rownum_perm
  seatnum <- seatnum_perm
  times = 0
    #position5:
  while(times < length(mylist)){
    
    x <- tryCatch(expr = {                      # Specifying expression
      mylist[[rownum-1]][seatnum+1]
      message("Everything was fine.")
    },error = function(e){          # Specifying error message
      message("There was an error message.")
    },warning = function(w){        # Specifying warning message
      message("There was a warning message.")
    },finally = {                   # Specifying final message
      message("tryCatch is finished.")
    }
    )
    pos5 <- position_value(x)
      seatnum <- seatnum+1
      rownum <- rownum-1
      x <- tryCatch(expr = {                      # Specifying expression
        mylist[[rownum-1]][seatnum+1]
        message("Everything was fine.")
      },error = function(e){          # Specifying error message
        message("There was an error message.")
      },warning = function(w){        # Specifying warning message
        message("There was a warning message.")
      },finally = {                   # Specifying final message
        message("tryCatch is finished.")
      }
      )
      pos5_value <- position_value(x)
      if(is.na(pos5_value)){
        times=times+1
      } else if(pos5_value=="#"){
        times = length(mylist)
        pos5=pos5_value
      }else{
        times = times+1
      }
      
    }
    
  rownum <- rownum_perm
  seatnum <- seatnum_perm
  times = 0
    #position6:
  while(times < length(mylist)){
    
    x <- tryCatch(expr = {                      # Specifying expression
      mylist[[rownum+1]][seatnum-1]
      message("Everything was fine.")
    },error = function(e){          # Specifying error message
      message("There was an error message.")
    },warning = function(w){        # Specifying warning message
      message("There was a warning message.")
    },finally = {                   # Specifying final message
      message("tryCatch is finished.")
    }
    )
    
    pos6 <- position_value(x)
      seatnum <- seatnum-1
      rownum <- rownum+1
      x <- tryCatch(expr = {                      # Specifying expression
        mylist[[rownum+1]][seatnum-1]
        message("Everything was fine.")
      },error = function(e){          # Specifying error message
        message("There was an error message.")
      },warning = function(w){        # Specifying warning message
        message("There was a warning message.")
      },finally = {                   # Specifying final message
        message("tryCatch is finished.")
      }
      )
      pos6_value <- position_value(x)
      if(is.na(pos6_value)){
        times=times+1
      } else if(pos6_value=="#"){
        pos6 = pos6_value
        times = length(mylist)
      }else{
        times = times+1
      }
    }
    
  rownum <- rownum_perm
  seatnum <- seatnum_perm
  times = 0
    #position7:
  while(times < length(mylist)){
    x <- tryCatch(expr = {                      # Specifying expression
      mylist[[rownum+1]][seatnum]
      message("Everything was fine.")
    },error = function(e){          # Specifying error message
      message("There was an error message.")
    },warning = function(w){        # Specifying warning message
      message("There was a warning message.")
    },finally = {                   # Specifying final message
      message("tryCatch is finished.")
    }
    )
    
    pos7 <- position_value(x)
      seatnum <- seatnum
      rownum <- rownum+1
      x <- tryCatch(expr = {                      # Specifying expression
        mylist[[rownum+1]][seatnum]
        message("Everything was fine.")
      },error = function(e){          # Specifying error message
        message("There was an error message.")
      },warning = function(w){        # Specifying warning message
        message("There was a warning message.")
      },finally = {                   # Specifying final message
        message("tryCatch is finished.")
      }
      )
      pos7_value <- position_value(x)
      if(is.na(pos7_value)){
        times=times+1
      } else if(pos7_value=="#"){
        pos7=pos7_value
        times = length(mylist)
      }else{
        times = times+1
      }
      
    }
    
  rownum <- rownum_perm
  seatnum <- seatnum_perm
  times = 0
  while(times < length(mylist)){
    x <- tryCatch(expr = {                      # Specifying expression
      mylist[[rownum+1]][seatnum+1]
      message("Everything was fine.")
    },error = function(e){          # Specifying error message
      message("There was an error message.")
    },warning = function(w){        # Specifying warning message
      message("There was a warning message.")
    },finally = {                   # Specifying final message
      message("tryCatch is finished.")
    }
    )
    pos8 <- position_value(x)
      seatnum <- seatnum+1
      rownum <- rownum+1
      x <- tryCatch(expr = {                      # Specifying expression
        mylist[[rownum+1]][seatnum+1]
        message("Everything was fine.")
      },error = function(e){          # Specifying error message
        message("There was an error message.")
      },warning = function(w){        # Specifying warning message
        message("There was a warning message.")
      },finally = {                   # Specifying final message
        message("tryCatch is finished.")
      }
      )
      pos8_value <- position_value(x)
      if(is.na(pos8_value)){
        times=times+1
      } else if(pos8_value=="#"){
        pos8=pos8_value
        times = length(mylist)
      }else{
        times = times+1
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

# mylist <- nextlist
mylist <- cur_seatnum_layout
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

