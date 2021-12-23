#!/usr/bin/env Rscript

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
parse_data <- function(file) {
  df <- str_match(readLines(file),"(on|off) x=(-?\\d+)\\.\\.(-?\\d+),y=(-?\\d+)\\.\\.(-?\\d+),z=(-?\\d+)\\.\\.(-?\\d+)")
  df <- df[,-c(1)]
  return(df)
}

mydata <- parse_data("2021/Day22/input.txt")
example1 <- parse_data("2021/Day22/example1.txt")
example2 <- parse_data("2021/Day22/example2.txt")
#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------

# part1 <- function(x){
#   df <- data.frame(matrix(0,ncol=7,nrow=length(x)))
#   colnames(df) <- c("light","xmin","xmax","ymin","ymax","zmin","zmax")
#   for(i in 1:length(x)){
#     x_digits <- as.numeric(unlist(regmatches(x[i], gregexpr("[[:digit:]]+", x[i]))))
#     df[i,c(2:7)] <- x_digits
#     df[i,1] <- strsplit(x[i]," ")[[1]][1]
#   }
#   
#   #remove those outside of range
#   
#   df <- df[which(df$xmin <= 50 & df$xmin >=-50 &
#                    df$xmax <= 50 & df$xmax >=-50 &
#                    df$ymin <= 50 & df$ymin >=-50 &
#                    df$ymax <= 50 & df$ymax >=-50 &
#                    df$zmin <= 50 & df$zmin >=-50 &
#                    df$zmax <= 50 & df$zmax >=-50),]
# 
#   cubes <- list()
#   
#   for(i in 1:nrow(df)){
#     for(x in df$xmin[i]:df$xmax[i]){
#       for(y in df$ymin[i]:df$ymax[i]){
#         for(z in df$zmin[i]:df$zmax[i]){
#           cur_cube <- as.character(c(x,y,z))
#           operation <- df$light[i]
#           
#           if(operation=="on"){
#             if(!list(cur_cube) %in% cubes){
#               cubes[[length(cubes)+1]] <- cur_cube
#             }
#           }else if (operation=="off"){
#             if(list(cur_cube) %in% cubes){
#               cubes <- cubes[-match(list(cur_cube),cubes)]
#             }
#           }
#           
#         }
#       }
#     }
#     
#   }
#   return(length(unique(cubes)))
# }


part1 <- function(x) {
  cubes <- array(0, dim=c(101,101,101))
  
  for(i in 1:nrow(x)) {
    light <- ifelse(x[i,1] == "on", 1, 0)
    
    xs <- intersect(seq(x[i,2], x[i,3]), -50:50) + 51
    ys <- intersect(seq(x[i,4], x[i,5]), -50:50) + 51
    zs <- intersect(seq(x[i,6], x[i,7]), -50:50) + 51

    for(m in xs) {
      for(n in ys) {
        cubes[m,n,zs] <- light
      }
    }
  }
  
  return(sum(cubes))
}



part1(example1)
part1(example2)
part1(mydata)


#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------
library(tictoc)
tic()

df <- as.data.frame(mydata)
colnames(df) <- c("type","x1", "x2", "y1", "y2", "z1", "z2")
cols.num <- c("x1", "x2", "y1", "y2", "z1", "z2")
df[cols.num] <- sapply(df[cols.num],as.numeric)
df$x2 <- df$x2 + 1
df$y2 <- df$y2 + 1
df$z2 <- df$z2 + 1

x_pts <- sort(unique(c(df$x1, df$x2)))
y_pts <- sort(unique(c(df$y1, df$y2)))
z_pts <- sort(unique(c(df$z1, df$z2)))

arr <- array(0, c(length(x_pts)-1, length(y_pts)-1, length(z_pts)-1))

for (i in seq_len(nrow(df))) {
  x_range <- seq( which(df$x1[i] == x_pts), which(df$x2[i] == x_pts)-1 )
  y_range <- seq( which(df$y1[i] == y_pts), which(df$y2[i] == y_pts)-1 )
  z_range <- seq( which(df$z1[i] == z_pts), which(df$z2[i] == z_pts)-1 )
  
  cur_val <- ifelse(d$type[i] == "on", 1, 0)
  
  for (x in x_range) {
    for(y in y_range) {
      for(z in z_range) {
        arr[x,y,z] <- cur_val
      }
    }
  }
}

z = which(arr==1, arr.ind=TRUE) %>%
  as_tibble() %>%
  setNames(c("xi","yi","zi")) %>% 
  mutate(
    xd <- x_pts[xi+1] - x_pts[xi],
    yd <- y_pts[yi+1] - y_pts[yi],
    zd <- z_pts[zi+1] - z_pts[zi],
    area <- xd * yd * zd
  )

options(scipen = 99)
sum(z$area)

toc()

