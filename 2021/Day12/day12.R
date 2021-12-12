#!/usr/bin/env Rscript

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
#Use this if you're opening this repo as a R project, using relative paths:
mydata <- as.character(readLines("2021/Day12/input.txt"))
example1 <- as.character(readLines("2021/Day12/example1.txt"))
example2 <- as.character(readLines("2021/Day12/example2.txt"))

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------

# path_traversal <- function(mat,current_node,unique_nodes,paths){
#   if(current_node=="end"){
#     paths <- paths + 1
#     # print(paths)
#   }
#   
#   routes <- sum(mat[current_node,]==1) 
#   if(routes==0){
#     return(paths)
#   }else{
#     next_cave <- unique_nodes[mat[current_node,]==1]
#     for(i in 1:length(next_cave)){
#       next_node <- next_cave[i]
#       current_node_num <- which.max(unique_nodes == current_node)
#       first_letter <- strsplit(current_node,"")[[1]][1]
#       if(first_letter %in% letters){
#         paths <- path_traversal(mat[,-current_node_num],next_node,unique_nodes[-current_node_num],paths)
#       }else{
#         paths <- path_traversal(mat,next_node,unique_nodes,paths)
#       }
#     }
#   }
# }

x <- example1
df <- as.data.frame(matrix(0, ncol=2,nrow=0))
for(i in 1:length(x)){
  y <- strsplit(x[i],"-")[[1]]
  df <- rbind(df,y)
}
colnames(df) <- c("from","to")

# unique_nodes <- unique(c(df$from,df$to))
# num_nodes <- length(unique_nodes)
# 
# mat <- matrix(0,nrow=num_nodes, ncol=num_nodes)
# rownames(mat) <- unique_nodes
# colnames(mat) <- unique_nodes
# 
# for(i in 1:nrow(df)){
#   mat[df$from[i],df$to[i]] <- 1
#   mat[df$to[i],df$from[i]] <- 1
# }
# 
# current_node <- "start"
# paths <- 0





#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------











