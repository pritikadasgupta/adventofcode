#!/usr/bin/Rscript

#clear workspace
rm(list = ls())

#Set working directory and load in data
# con <- file("stdin", open = "r")
data1 <- readLines(con) #read in file
# close(con)


#-------------------------------------------------------------------------
#Clean up data 
#-------------------------------------------------------------------------

directions <- vector()
for (datum in data1) {
  directions <- append(directions,strsplit(datum,"")[[1]][1])
}

values <- vector()
for(datum in data1){
  x <- strsplit(datum,"")[[1]]
  x <- as.numeric(x[2:length(x)])
  x <- paste(c(x),collapse="")
  
  values <- append(values,as.numeric(x))
}

df <- as.data.frame(cbind(directions,values))

#-------------------------------------------------------------------------
#Part 1: Setup
#-------------------------------------------------------------------------


#store cardinal directions in a list
direction_list <- vector(mode="list", length=4)
names(direction_list) <- c("N","S","E","W")
direction_list[[1]] <- c(0,1) #north
direction_list[[2]] <- c(0,-1) #south
direction_list[[3]] <- c(1,0) #east
direction_list[[4]] <- c(-1,0) #west


#store rotational directions in a list
rotation_list <- vector(mode="list", length=2)
names(rotation_list) <- c("R","L")
rotation_list[[1]] <- c(1,-1) #right
rotation_list[[2]] <- c(-1,1) #left

update <- function(a,b,part,x,y,x_,y_){
  if((a %in% names(direction_list)) && (part==TRUE)){
    x = x+ ((direction_list[[a]][1])*b)
    y = y+ ((direction_list[[a]][2])*b)
  }
  
  if((a %in% names(direction_list)) && (part==FALSE)){
    x_ = x_+ ((direction_list[[a]][1])*b)
    y_ = y_+ ((direction_list[[a]][2])*b)
  }
  
  if(a %in% names(rotation_list)){
    for(i in 1:floor(b/90)){
      rotate <- rotation_list[[a]]
      d <- x_
      e <- y_
      x_ = (e * rotate[1])
      y_ = (d * rotate[2])
    }
  }
  
  if(a=="F"){
    x = x + (x_*b)
    y = y + (y_*b)
  }
  
  myresults <- c(x,y,x_,y_)
  return(myresults)
}

#-------------------------------------------------------------------------
#Part 1: Initialization and Results
#-------------------------------------------------------------------------

x=0
y=0
x_=1 #start in the east
y_=0  #start in the east
end_result <- update(as.character(df$directions[1]),as.numeric(df$values[1]),part=TRUE,x,y,x_,y_)
print(end_result)
for(j in 2:nrow(df)){
  x=as.numeric(end_result[1])
  y=as.numeric(end_result[2])
  x_=as.numeric(end_result[3])
  y_=as.numeric(end_result[4])
  a <- as.character(df$directions[j])
  b <- as.numeric(df$values[j])
  part=TRUE
  end_result <- update(a,b,part,x,y,x_,y_)
  print(end_result)
}


distance = abs(end_result[1]-0) + abs(end_result[2]-0)
print(paste("Part 1:",distance))

#-------------------------------------------------------------------------
#Part 2: Initialization and Results
#-------------------------------------------------------------------------
x=0
y=0
x_=10 #start in the east
y_=1  #start in the east
end_result <- update(as.character(df$directions[1]),as.numeric(df$values[1]),part=FALSE,x,y,x_,y_)
for(j in 2:nrow(df)){
  x=as.numeric(end_result[1])
  y=as.numeric(end_result[2])
  x_=as.numeric(end_result[3])
  y_=as.numeric(end_result[4])
  a <- as.character(df$directions[j])
  b <- as.numeric(df$values[j])
  part=FALSE
  end_result <- update(a,b,part,x,y,x_,y_)
  print(end_result)
}


distance = abs(end_result[1]-0) + abs(end_result[2]-0)
print(paste("Part 2:",distance))
