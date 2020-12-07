#!/usr/bin/env Rscript
#clear workspace
rm(list = ls())
#Libraries
library(tidyverse)
library(igraph)

#Set working directory and load in data
# data1 <- readLines(file("stdin")) #read in file
setwd("~/Documents/adventofcode1/adventofcode/2020/Day7")
data1 <- readLines("exercise7.input.txt") #read in file

#-------------------------------------------------------------------------
#Clean up data and make an edge list with weights for the rules
#-------------------------------------------------------------------------
bags <- vector()
to_bag <- vector()
to_bag_num <- vector()
for(a in data1){
  a1 <- strsplit(a,"contain")[[1]] #split on contain
  a1_2 <- strsplit(a1[2]," bags, ")[[1]]
  a1_2 <- unlist(strsplit(a1_2," bag, "))
  numbers <- list()
  for(i in data1){
    numbers <- append(numbers,unique(as.numeric(unlist(strsplit(gsub("[^0-9]", "", unlist(i)), "")))))
  }
  numbers1 <- unlist(numbers)
  bad_numbers <- seq(min(numbers1),max(numbers1),1)
  bad_words <- c(" ","","bag.","bags.","bag","bags")
  bad_library <- c(bad_numbers,bad_words)
  
  isEmpty <- function(x) {
    return(length(x)==0)
  }
  
  #for each element in the second half of the statement
  for(x in a1_2){
    bags <- append(bags,strsplit(a1[1]," bags ")[[1]])
    
    numberofbags <- unique(as.numeric(unlist(strsplit(gsub("[^0-9]", "", unlist(x)), ""))))
    
    numberofbags <- append(numberofbags,0)[1]
    
    to_bag_num <- append(to_bag_num,numberofbags)
    list_words <- strsplit(x," ")[[1]]
    type <- list_words[!list_words %in% bad_library]
    to_bag <- append(to_bag,paste(type,collapse=" "))
  }
}

df <- as.data.frame(cbind(bags,to_bag_num,to_bag))

#-------------------------------------------------------------------------
#Part 1:
#-------------------------------------------------------------------------
# How many bag colors can eventually contain at least one shiny gold bag?

mat <- as.matrix(df)

net1<-graph.data.frame(mat, directed=F)
# V(net1) #prints the list of vertices (bags)
# E(net1) #prints the list of edges (relationships)
# degree(net1) #print the number of edges per vertex (relationships per bag)
# V(net1)$color<-ifelse(V(net1)$name=='shiny gold',"gold","black") 
# 
# for(j in 1:length(E(net1)$weight)){
#   E(net1)$weight[j] <- df$to_bag_num[j]
# }
# 
# plot(net1, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=1.5,
# vertex.color=c( "pink", "gold")[1+(V(net1)$color=="gold")] ) 
# 
# plot(net1, vertex.size=0, vertex.label=NA, edge.arrow.size=0 )
# 
# plot(net1, layout=layout_randomly)

#delete all bags which have a 0 relationship with another bag
mat_nozeros <- as.matrix(df[which(df$to_bag_num!=0),c(1,3)])
net2<-graph.data.frame(mat_nozeros, directed=F)
plot(net2)

# 
# 
# unique_list_bags <- unique(df$bags)
# for(bag in unique_list_bags){
#   # which(df$to_bag==bag)
#   which(df$bags==bag)
#   
#   
#   
# }


df_nozeros<- df[which(df$to_bag_num!=0),]
unique_bags <- unique(c(df_nozeros$bags,df_nozeros$to_bag))

df_nodes <- as.data.frame(cbind(seq(1,length(unique_bags)),unique_bags))
colnames(df_nodes) <- c("Id","Label")

write.csv(df_nodes, "nodes.csv",row.names=FALSE) #the nodes file tells Gephi all the possible nodes in a network

df_edges <- df_nozeros[,c(1,2,3)]
df_edges$Type = "Directed"
df_edges$Source = ""
df_edges$Target = ""
df_edges$Weight = df_edges$to_bag_num

for(k in 1:nrow(df_edges)){
  df_edges$Source[k] <- df_nodes$Id[which(df_nodes$Label==df_edges$bags[k])]
  df_edges$Target[k] <- df_nodes$Id[which(df_nodes$Label==df_edges$to_bag[k])]
}

df_edges <- df_edges[,c(4,5,6,7)]
write.csv(df_edges, "edges.csv",row.names=FALSE)

retrieve_bag <- function(bag){
  for (x in 1:length(data)) {
    data_container <- data[which(data[,3]==bag),]
    newbag <- data_container[1,1]
    bag_count = bag_count+nrow(data_container)
    retrieve_bag(newbag)
  }
}

retrieve("shiny gold")







