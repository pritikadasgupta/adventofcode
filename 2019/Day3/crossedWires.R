#!/usr/bin/env Rscript
#clear workspace
rm(list = ls())
#Libraries
# library(tidyverse)
# library(igraph)

#Set working directory and load in data
# data1 <- as.numeric(readLines(file("stdin"))) #read in file
setwd("~/Documents/adventofcode1/adventofcode/2019/Day3")
data1 <- readLines("exercise3.input.txt") #read in file


#Functions
#-------------------------------------------------------------------------


#Clean up data
#-------------------------------------------------------------------------

main_data <- as.character(strsplit(data1[1],",")[[1]])
as.character(strsplit(data1[2],",")[[1]])


#-------------------------------------------------------------------------
#PART 1
#-------------------------------------------------------------------------
