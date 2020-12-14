#!/usr/bin/Rscript

#clear workspace
rm(list = ls())

library(tidyverse)
library(igraph)
library(magrittr)

#Load Data
#Use this if you're running from the command line:
# mydata <- scan(file("stdin"), numeric(), sep = ",") #read in file

#Use this if you're opening this repo as a R project, using relative paths:
mydata <- scan("2019/Day7/input", numeric(), sep = ",")

#-------------------------------------------------------------------------
#Clean up data
#-------------------------------------------------------------------------
