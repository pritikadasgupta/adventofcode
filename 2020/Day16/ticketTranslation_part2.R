#!/usr/bin/Rscript

#clear workspace
rm(list = ls())

# Libraries
library(reshape2)
library(ggplot2)

# You collect the rules for ticket fields, 
# the numbers on your ticket, 
# and the numbers on other nearby tickets for the same train service 

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------

#Use this if you're running from the command line:
# mydata <- read.table(file("stdin"), sep = '=', strip.white = TRUE,col.names = c('key', 'value'))

#Use this if you're opening this repo as a R project, using relative paths:
mydata <- readLines('2020/Day16/input.txt')

#------------------------------------------------------------------------------------------
#Functions
#------------------------------------------------------------------------------------------

#create rule parser
rule_parser <- function(rule){
  ruletype <- strsplit(rule,":")[[1]][1]
  rulerange <- strsplit(rule,":")[[1]][2]
  rulerange1 <- strsplit(rulerange,"or")[[1]][1]
  rulerange2 <- strsplit(rulerange,"or")[[1]][2]
  rulerange1_start <- as.numeric(strsplit(rulerange1,"-")[[1]][1])
  rulerange1_end <- as.numeric(strsplit(rulerange1,"-")[[1]][2])
  rulerange2_start <- as.numeric(strsplit(rulerange2,"-")[[1]][1])
  rulerange2_end <- as.numeric(strsplit(rulerange2,"-")[[1]][2])
  
  valid_num <- vector()
  for(i in rulerange1_start:rulerange1_end){
    valid_num <- append(valid_num,i)
  }
  for(j in rulerange2_start:rulerange2_end){
    valid_num <- append(valid_num,j)
  }
  return(valid_num)
}

#------------------------------------------------------------------------------------------
#Clean Data
#------------------------------------------------------------------------------------------
yourticket_idx <- which(mydata == "your ticket:")
rules <- mydata[1:(yourticket_idx-2)]
myticket <- as.numeric(strsplit(mydata[yourticket_idx+1],",")[[1]])

nearbyticket_idx <- which(mydata == "nearby tickets:")

nearbyticket <- vector()
for(i in (nearbyticket_idx+1):(length(mydata))){
  nearbyticket <- append(nearbyticket,as.numeric(strsplit(mydata[i],",")[[1]]))
}

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------
valid_num <- vector()
for(i in 1:length(rules)){
  valid_num <- append(valid_num,rule_parser(rules[i]))
}

invalid_num <- nearbyticket[!nearbyticket %in% valid_num]
valid_tickets <- nearbyticket[nearbyticket %in% valid_num]
print(paste("Part 1:",sum(invalid_num)))

#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------
#get list of nearby tickets
n <- length((nearbyticket_idx+1):(length(mydata)))
nearbyticket <- vector(mode = "list", length = n)
j=1
for(i in (nearbyticket_idx+1):(length(mydata))){
  nearbyticket[[j]] <- as.numeric(strsplit(mydata[i],",")[[1]])
  j=j+1
}
nearbyticket2 <- nearbyticket

#remove the invalid tickets entirely
k=1
nt_length <- length(nearbyticket2)
while(k<=nt_length && k!=0){
  if(sum(nearbyticket2[[k]] %in% valid_num) <length(nearbyticket[[1]])){
    nearbyticket2 <- nearbyticket2[-k]  
    k=1
  }else{
    k=k+1
    nt_length <- length(nearbyticket2)
  }
}


track_r <- vector()
track_k <- vector()
track_b <- vector()
track_v <- vector()
track_value <- vector()
for(k in 1:length(nearbyticket2)){
  for(b in 1:length(nearbyticket2[[k]])){
    for(r in 1:length(rules)){
      track_k <- append(track_k,k)
      track_b <- append(track_b,b)
      track_r <- append(track_r,r)
      track_value <- append(track_value,nearbyticket2[[k]][b])
      if(nearbyticket2[[k]][b] %in% rule_parser(rules[r])){
        track_v <- append(track_v,"V")
      }else{
        track_v <- append(track_v,"IV")
      }
    }
  }
}

mydf <- as.data.frame(cbind(track_value,track_k,track_b,track_r,track_v))
mycolumns <- 1:length(nearbyticket2[[1]])
mylist <- vector(mode = "list", length = length(mycolumns))
for(num in mycolumns){
  smalldf <- subset(mydf,mydf$track_b==num)
  smalldf <- subset(mydf,mydf$track_b==num)
  rules_impossible <- vector()
  for(d in 1:nrow(smalldf)){
    for(r in 1:lengths(rule_parser(rules[r]))){
      if(smalldf$track_v[d]=="IV"){
        rules_impossible <- append(rules_impossible,as.numeric(smalldf$track_r[d]))
      }
    }
  }
  rules_impossible1 <- unique(rules_impossible)
  mylist[[num]] <- as.numeric(rules_impossible1)
}



fakeData <- rep(1, size=length(mycolumns) * length(mycolumns))
my_mat <- matrix(fakeData,ncol = length(mycolumns), nrow = length(mycolumns))

for(column in 1:length(mylist)){
  check <- mylist[[column]]
  for(row in check){
    
    my_mat[row,column] <- 0
  }
}

#Graphics: https://rpubs.com/lgadar/matrix-visualizations
A <- my_mat
longData<-melt(A)
longData<-longData[longData$value!=0,]

ggplot(longData, aes(x = Var2, y = Var1)) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient(low="#63ACBE", high="#601A4A") +
  labs(x="rules", y="positions", title="Positions vs. Rules Matrix") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))


#figure out positions which only satisfy one rule
my_mat2 <- my_mat
rules_ <- vector()
rules_location_ <- vector()
row = 1
rownames(my_mat2) <- c(1:20)
colnames(my_mat2) <- c(1:20)
while(length(my_mat2) >1 && row <=nrow(my_mat2)){
# for(row in 1:nrow(my_mat2)){
  if(sum(my_mat2[row,])==1){
    cur <- which(my_mat2[row,]==1)
    print(paste("position",rownames(my_mat2)[row],"is at:",cur))
    
    rules_ <- append(rules_,rownames(my_mat2)[row])
    rules_location_ <- append(rules_location_,cur)
    
    my_mat2<- my_mat2[-row,-cur]
    row <-1
  }else{
    row=row+1
  }
  
}

x <- myticket[as.numeric(rules_)]
y <- startsWith(my_results$V2,"departure")

my_results <- as.data.frame(cbind(rules_,rules[as.numeric(rules_)],rules_location_))
my_results2 <- subset(my_results,startsWith(my_results$V2,"departure"))
part2 <- prod(x[y])
print(paste("Part 2:",prod(as.numeric(my_results2$rules_location_))))
