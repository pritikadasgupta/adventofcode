#!/usr/bin/env Rscript
#clear workspace
# rm(list = ls())
#Libraries
library(tidyverse)
library(igraph)

#Set working directory and load in data
data1 <- readLines(file("stdin")) #read in file

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

# mat <- as.matrix(df)
# 
# net1<-graph.data.frame(mat, directed=F)
# 
# #delete all bags which have a 0 relationship with another bag
# mat_nozeros <- as.matrix(df[which(df$to_bag_num!=0),c(1,3)])
# net2<-graph.data.frame(mat_nozeros, directed=F)
# plot(net2)
# 
# df_nozeros<- df[which(df$to_bag_num!=0),]
# unique_bags <- unique(c(df_nozeros$bags,df_nozeros$to_bag))
# 
# df_nodes <- as.data.frame(cbind(seq(1,length(unique_bags)),unique_bags))
# colnames(df_nodes) <- c("Id","Label")
# 
# write.csv(df_nodes, "nodes.csv",row.names=FALSE) #the nodes file tells Gephi all the possible nodes in a network
# 
# df_edges <- df_nozeros[,c(1,2,3)]
# df_edges$Type = "Directed"
# df_edges$Source = ""
# df_edges$Target = ""
# df_edges$Weight = df_edges$to_bag_num
# 
# for(k in 1:nrow(df_edges)){
#   df_edges$Source[k] <- df_nodes$Id[which(df_nodes$Label==df_edges$bags[k])]
#   df_edges$Target[k] <- df_nodes$Id[which(df_nodes$Label==df_edges$to_bag[k])]
# }
# 
# df_edges <- df_edges[,c(4,5,6,7)]
# write.csv(df_edges, "edges.csv",row.names=FALSE)

net3 <- graph_from_edgelist(as.matrix(df[,c(1,3)]), directed = TRUE)
# tkplot(net3)

list_bags <- all_simple_paths(net3, "shiny gold", to = V(net3),mode="in")
vector_bags <- unique(unlist(list_bags))

#PART 1 ANSWER:
part1 <- length(vector_bags)-1
print(paste("Part 1:",part1))

#-------------------------------------------------------------------------
#Part 2:
#-------------------------------------------------------------------------
# How many individual bags are required inside your single shiny gold bag?

all_paths <- all_simple_paths(net3, "shiny gold", to = V(net3),mode="out")

counts <- vector()
for(i in 1:length(all_paths)){
  counts <- append(counts,length(all_paths[[i]]))
}

longest_counts <- which(counts == max(counts))
longest_paths <- vector()
for(j in 1:length(longest_counts)){
  longest_paths <- append(longest_paths,all_paths[j])
}

thepath <-longest_paths[[length(longest_paths)]]

thepath_count <- vector()
for(k in 2:length(thepath)){
  color <- thepath[k]$name
  prevcolor <-thepath[k-1]$name 
  
  newdf <- subset(df, bags==prevcolor & to_bag==color)
  thepath_count <- append(thepath_count,as.numeric(newdf$to_bag_num[1]))
}

sum(thepath_count)





