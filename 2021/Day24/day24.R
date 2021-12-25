#!/usr/bin/env Rscript
options(scipen = 100)
#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
mydata <- read.delim("2021/Day24/input.txt",sep=" ",header=FALSE,col.names = c("ops","var","val"))
#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------
parse_alu = function(x) {
  x[x$ops=="inp","call"] <- paste(x$var[x$ops=="inp"],"<- w_vals")
  x[x$ops=="mul","call"] <- paste(x$var[x$ops=="mul"],"<-",x$var[x$ops=="mul"],"*",x$val[x$ops=="mul"])
  x[x$ops=="add","call"] <- paste(x$var[x$ops=="add"],"<-",x$var[x$ops=="add"],"+",x$val[x$ops=="add"])
  x[x$ops=="div","call"] <- paste(x$var[x$ops=="div"],"<-",x$var[x$ops=="div"],"/",x$val[x$ops=="div"])
  x[x$ops=="mod","call"] <- paste(x$var[x$ops=="mod"],"<-",x$var[x$ops=="mod"],"%%",x$val[x$ops=="mod"])
  x[x$ops=="eql","call"] <- paste(x$var[x$ops=="eql"],"<- as.integer(",x$var[x$ops=="eql"],"==",x$val[x$ops=="eql"],")")
  return(x)
}

parsed_data <- parse_alu(mydata)
w <- x <- z <- y <- 0
w_vals <- vals <- 9:1

nrow(parsed_data)

for(i in 1:nrow(parsed_data)){
  print(i)
  eval(parse(text=paste(parsed_data$call[i])))
  if(parsed_data$ops[i]=="inp" && i!=1){
    vals <- vals[!duplicated(z)]
    z <- z[!duplicated(z)]
    vals <- c(t(outer(vals,w_vals,paste0)))
    z <- rep(z, each=9)
    w <- rep(w_vals,times=length(z)/9)
  }
}
vals[z==0][1]



#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------
w <- x <- z <- y <- 0
w_vals <- vals <- 1:9

for(i in 1:nrow(parsed_data)){
  print(i)
  eval(parse(text=paste(parsed_data$call[i])))
  if(parsed_data$ops[i]=="inp" && i!=1){
    vals <- vals[!duplicated(z)]
    z <- z[!duplicated(z)]
    vals <- c(t(outer(vals,w_vals,paste0)))
    z <- rep(z, each=9)
    w <- rep(w_vals,times=length(z)/9)
  }
}
vals[z==0][1]
