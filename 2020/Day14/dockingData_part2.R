#!/usr/bin/Rscript

#clear workspace
rm(list = ls())

# Libraries
library(data.table)

#Load Data
#Use this if you're running from the command line:
# mydata <- read.table(file("stdin"), sep = '=', strip.white = TRUE,col.names = c('key', 'value'))


#Use this if you're opening this repo as a R project, using relative paths:
mydata <- read.table('2020/Day14/input.txt', sep = '=', strip.white = TRUE,col.names = c('key', 'value'))

#-------------------------------------------------------------------------
#Clean up data and Functions
#-------------------------------------------------------------------------

convert <- function(x){ #converts to 36 bits
  x_bit32 <- rev(as.character(intToBits(x)))
  x_bit36 <- c(rep(0, 4), as.integer(x_bit32))
  x_new <- paste(x_bit36, collapse = '')
  return(x_new)
}


binary_to_integer <- function(x) {
  x_char <- paste(x)
  x_int <- suppressWarnings(as.integer(strsplit(x_char, '')[[1]]))
  return(sum(x_int * 2^rev(seq_along(x_int) - 1)))
}

mask <- function(mask_, x) {
  mask_ <- suppressWarnings(as.integer(strsplit(mask_, '')[[1]]))
  x <- as.integer(strsplit(x, '')[[1]])
  x[!is.na(mask_)] <- mask_[!is.na(mask_)]
  paste(x, collapse = '')
}

MyIntToBit <- function(x, dig) {
  i <- 0L
  string <- numeric(dig)
  while (x > 0) {
    string[dig - i] <- x %% 2L
    x <- x %/% 2L
    i <- i + 1L
  }
  string
}



mask_idx <- which(mydata[,1]=="mask")
#create new variables
mydata$value_masked <- ""
mydata$value_converted <- ""
mydata$value_int <- ""
mydata$mem_value <- ""
not_unique_mem <- vector()

for(i in 1:nrow(mydata)){
  if(mydata[i,1]=="mask"){
    mymask <- mydata[i,2]
    j=i+1
    while(mydata[j,1]!="mask" && j<(nrow(mydata)+1)){
      
      memvalues <- suppressWarnings(as.integer(strsplit(mydata$key[j],"")[[1]]))
      mem_justnumbers <- memvalues[!is.na(memvalues)]
      cur_mem_value <- as.integer(paste(mem_justnumbers,collapse=""))
      mydata$mem_value[j] <- cur_mem_value
      
      converted_value_x <- suppressWarnings(convert(cur_mem_value))
      masked_value <- mask(mymask,converted_value_x)
      
      mask_2 <- suppressWarnings((strsplit(mymask, '')[[1]]))
      x2 <- strsplit(converted_value_x, '')[[1]]
      
      for(d in 1:length(mask_2)){
        if(mask_2[d]=="X"){
          x2[d]="X"
        }else if(mask_2[d]=="0"){
          x2[d]=as.character(x2[d]) #unchanged
        }else if(mask_2[d]=="1"){
          x2[d]="1"
        }
      }
      
      masked_val <- paste(x2, collapse = '')
      
      #next find combos
      mask_1 <- strsplit(mymask, '')[[1]]
      Xtimes <- which(mask_1=="X")
      all_x <- list()
      n <- length(Xtimes)
      
      #attempt 1: Error: vector memory exhausted (limit reached?)
      # Xtimes_solns <- lapply(0:(2^n-1), FUN=function(x) head(as.integer(intToBits(x)),n))
      
      #attempt 2: takes too long
      # l <- rep(list(0:1), n)
      # Xtimes_solns <- expand.grid(l)
      
      #attempt 3:
      # Xtimes_solns <- lapply(0:(2^n - 1), function(x) MyIntToBit(x,n))
      
      #attempt using data.table
      # Xtimes_solns <- do.call(CJ, replicate(n, 0:1, FALSE))
        
      library(iterpc)
      I = iterpc(2, n, label=c(0,1), ordered=T, replace=T)
      
      for(a in 1:(2^n)){
        x1 <- x2
        cur_soln <- getnext(I)
        for(b in 1:length(cur_soln)){
          x1[Xtimes[b]] = as.character(cur_soln[b])
        }
        all_x <- append(all_x,paste(x1,collapse=''))
      }
      
      rm(I)
      
      converted_int <- list()
      for(masked_value in 1:length(all_x)){
        converted_int <- append(converted_int,binary_to_integer(all_x[masked_value][[1]]))
      }
      
      mydata$value_masked[j] <- converted_value_x
      mydata$value_converted[j] <- list(all_x)
      mydata$value_int[j] <- list(converted_int)
      j=j+1
    }
  }
}


#-------------------------------------------------------------------------
#More data wrangling and cleaning (or not cleaning)
#-------------------------------------------------------------------------


myvalues <- vector()
mymemvalues <- vector()

for(i in 1:nrow(mydata)){
  if(mydata[i,1]!="mask"){
    mems <- mydata$value_int[[i]]
    for(j in mems){
      myvalues <- append(myvalues,mydata$value[[i]])
      mymemvalues <- append(mymemvalues,j)
    }
  }
}

mydf <- as.data.frame(cbind(myvalues,mymemvalues))
mydf$convertedvalues <- 0
for(i in 1:nrow(mydf)){
  mydf$convertedvalues[i] <- mydf$myvalues[i]
}

all_mem_values <- as.character(unique(mydf$mymemvalues))
sorted_mem_values <- sort(as.character(unique(mydf$mymemvalues)))
mydf$to_sum <- 0
overwrite <- vector()
#fill it in first
for(a in 1:nrow(mydf)){
  for(b in 1:length(sorted_mem_values)){
    val_a <- as.character(mydf$mymemvalues[a])
    val_b <- as.character(sorted_mem_values[b])
    if(val_a == val_b){
      mydf$to_sum[a] <- as.character(mydf$myvalues[a])
    }
  }
}

for(a in 1:nrow(mydf)){
  for(b in 1:length(sorted_mem_values)){
    val_a <- as.character(mydf$mymemvalues[a])
    val_b <- as.character(sorted_mem_values[b])
    if(val_a == val_b){
      if(val_b %in% overwrite){
        myidx <- which(mydf$mymemvalues == val_b)
        for(b in 1:(length(myidx)-1)){
          mydf$to_sum[myidx[b]] <- "0"
        }
      }
      # mydf$to_sum[a] <- as.character(mydf$myvalues[a])
      overwrite <- append(overwrite,val_b)
    }
  }
}


#-------------------------------------------------------------------------
#Part 2 Answer:
#-------------------------------------------------------------------------

address_sum <- sum(as.numeric(mydf$to_sum),na.rm=TRUE)

part2 <- paste(sum(address_sum,na.rm=TRUE))
print(part2)

