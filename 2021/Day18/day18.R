#!/usr/bin/env Rscript

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
#Use this if you're opening this repo as a R project, using relative paths:
mydata <- as.character(readLines("2021/Day18/input.txt"))
example <- as.character(readLines("2021/Day18/example.txt"))
anotherexample <- as.character(readLines("2021/Day18/anotherexample.txt"))

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------

add_snailfish <- function(x,y){
  x <- paste0(c("[",x,",",y,"]"),collapse="")
  x <- reduce_snailfish(x)
  return(x)
}

separate_chars <- function(x){
  i <- 1
  chars <- strsplit(x,"")[[1]]
  chars_new <- c()
  while(i <=(length(chars)-1)){
    first <- suppressWarnings(as.numeric(chars[i]))
    second <- suppressWarnings(as.numeric(chars[i+1]))
    
    if(!is.na(first) & !is.na(second)){
      chars_new <- c(chars_new,paste0(chars[i],chars[i+1],collapse=""))
      i <- i+2
    }else{
      chars_new <- c(chars_new,chars[i])
      i <- i+1
    }
  }
  chars_new <- c(chars_new,chars[i])
  return(chars_new)
}

reduce_snailfish <- function(x){
  chars <- separate_chars(x)
  
  repeat{
    fifth_bracket <- which(cumsum(chars=="[") - cumsum(chars=="]")==5)[1]
    all_pos <- grep("\\d",chars)
    
    if(!is.na(fifth_bracket)){
      chars <- explode(chars)
      next
    }
    
    all_numbers <- as.numeric(chars[all_pos])
    first_10_number <- all_pos[all_numbers >= 10][1]
    
    if(!is.na(first_10_number)){
      chars <- split_snailfish(chars)
      next
    }
    
    break
  }
  
  return(paste0(chars,collapse=""))
}


# If any pair is nested inside four pairs, the leftmost such pair explodes.
explode <- function(chars){
  fifth_bracket <- which(cumsum(chars=="[") - cumsum(chars=="]")==5)[1]
  before_bracket <- which(grep("\\d",chars) > fifth_bracket)[1] - 1
  all_pos <- grep("\\d",chars)
  current_pos <- all_pos[before_bracket+(0:3)]
  
  if(before_bracket==0){
    current_pos <- c(NA,current_pos)
  }
  
  if(!is.na(current_pos[1])){
    chars[current_pos[1]] <- as.numeric(chars[current_pos[1]]) + as.numeric(chars[current_pos[2]])
  }
  
  if(!is.na(current_pos[4])){
    chars[current_pos[4]] <- as.numeric(chars[current_pos[3]]) + as.numeric(chars[current_pos[4]])
  }
  
  chars <- c(chars[1:(fifth_bracket-1)],"0",chars[(fifth_bracket+5):length(chars)])
  return(chars)
}
# example2 <- strsplit("[[[[[9,8],1],2],3],4]","")[[1]]
# explode(example2)



# If any regular number is 10 or greater, the leftmost such regular number splits.
split_snailfish <- function(chars){
  all_pos <- grep("\\d",chars)
  all_numbers <- as.numeric(chars[all_pos])
  first_10_number <- all_pos[all_numbers >= 10][1]
  
  cur_num <- all_numbers[all_numbers >=10][1]
  chars <- c(chars[1:(first_10_number-1)],"[",floor(cur_num/2),",",ceiling(cur_num/2),"]",chars[(first_10_number+1):length(chars)])
  return(chars)
}

# example3 <- separate_chars("[[[[0,7],4],[15,[0,13]]],[1,1]]")
# split_snailfish(example3)

# The magnitude of a pair is 3 times the magnitude of its left element plus 2 times the magnitude of its right element.
magnitude <- function(x,y){
  return(3*x + 2*y)
}


part1 <- function(input){
  i <- 1
  current <- input[i]
  while(i <= (length(input)-1)){
    current <- add_snailfish(current,input[i+1])
    i <- i+1
    print(current)
  }
  print(current)
  current <- gsub("\\[","magnitude(",current)
  current <- gsub("\\]",")",current)
  
  result <- eval(parse(text=current))
  
  return(result)
}


# x <- "[[[[4,3],4],4],[7,[[8,4],9]]]"
# y <- "[1,1]"
# add_snailfish(x,y)


# x<- "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]"
# y <- "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
# add_snailfish(x,y)

part1(anotherexample)
part1(example)
part1(mydata)


#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------

part2 <- function(input){
  mag_list <- c()
  for(i in 1:length(input)){
    for(j in 1:length(input)){
      if(i!=j){
        current <- add_snailfish(input[i],input[j])
        current <- gsub("\\[","magnitude(",current)
        current <- gsub("\\]",")",current)
        mag_list <- c(mag_list,eval(parse(text=current)))
      }
    }
  }
  result <- max(mag_list)
  return(result)
}

part2(example)
part2(mydata)

