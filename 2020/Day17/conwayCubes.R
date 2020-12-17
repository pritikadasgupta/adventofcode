#!/usr/bin/Rscript

#clear workspace
rm(list = ls())

# Libraries

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------

#Use this if you're running from the command line:
# mydata <- read.table(file("stdin"), sep = '=', strip.white = TRUE,col.names = c('key', 'value'))

#Use this if you're opening this repo as a R project, using relative paths:
mydata <- readLines('2020/Day17/input_test1.txt')

#------------------------------------------------------------------------------------------
#Functions
#------------------------------------------------------------------------------------------
