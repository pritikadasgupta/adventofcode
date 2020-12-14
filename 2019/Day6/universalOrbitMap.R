#!/usr/bin/Rscript

#clear workspace
rm(list = ls())

library(tidyverse)
library(igraph)
library(magrittr)

#Load Data
#Use this if you're running from the command line:
# mydata <- readLines(file("stdin")) #read in file

#Use this if you're opening this repo as a R project, using relative paths:
mydata <- readLines("2019/Day6/input")

#-------------------------------------------------------------------------
#Clean up data
#-------------------------------------------------------------------------
df <- t(as.data.frame(strsplit(mydata,')')))
rownames(df) <- NULL

mygraph <- graph_from_data_frame(df)

distance_table(mygraph)

part1 <- sum(extract2(distance_table(mygraph),"res"))

paste("Part 1:",part1)


part2 <- distances(mygraph, v = V(mygraph)[["YOU"]], 
                    to = V(mygraph)[["SAN"]]) - 2

paste("Part 2:",part2)

# plot(mygraph)

