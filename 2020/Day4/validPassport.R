#-------------------
#clear workspace
rm(list = ls())

#Libraries
library(tidyverse)

#Set working directory and load In Data
# set(wd)
data1 <- readLines(file("stdin")) #read in file

#-------------------
#Clean up dataframe
#-------------------
data2 <- paste(data1,collapse = "\n") #concatenate everything into one string
data3 <- strsplit(data2,"\n\n")[[1]] #find where there are two new lines to separate btwn blanks
data4 <- gsub("\n"," ",data3) #get rid of the \n
#note: prob could do it cleaner with magrittr? 

#-------------------
#PART 1
#-------------------
validFields = c("byr","iyr","eyr","hgt","hcl","ecl","pid") #required passport fields
# optionalField = c("cid")

validPassports = 0 #initialize num of valid passports
x_valid = vector() #initialize a vector of valid passports
for (x in data4) {
  validFieldValues = 0
  for (y in validFields){
    if(grepl(as.character(y), as.character(x), fixed = TRUE)){
      validFieldValues = validFieldValues+1
    }
  }
  
  if(validFieldValues==length(validFields)){
    validPassports = validPassports+1
    x_valid <- append(x_valid,x)
  }
  
}

print(paste("Part 1:",validPassports))

#-------------------
#PART 2
#-------------------

#-------------------
#Create functions for validating each passport field
#-------------------

# byr (Birth Year) - four digits; at least 1920 and at most 2002.
checkbyr <- function(x){
  if(x >=1920 && x <=2002){
    return(TRUE)
  }else{return(FALSE)}
}

# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
checkiyr <- function(x){
  if(x >=2010 && x <=2020){
    return(TRUE)
  }else{return(FALSE)}
}

# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
checkeyr <- function(x){
  if(x >=2020 && x <=2030){
    return(TRUE)
  }else{return(FALSE)}
}

# hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
# If in, the number must be at least 59 and at most 76.

checkhgt <- function(x){
  if(grepl("cm", as.character(x), fixed = TRUE)){
    xnum <- as.numeric(str_remove_all(x, "cm")) #get numeric measurement
    if(xnum >=150 && xnum <=193){
      return(TRUE)
    }else{return(FALSE)}
  }else if(grepl("in", as.character(x), fixed = TRUE)){
    xnum <- as.numeric(str_remove_all(x, "in")) #get numeric measurement
    if(xnum >=59 && xnum <=76){
      return(TRUE)
    }else{return(FALSE)}
  }else{
    return(FALSE)
  }

}

# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
checkhcl <- function(x){
  if(str_detect(x, "^#[a-f0-9]{6}$")){
    return(TRUE)
  }else{return(FALSE)}
}

# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
checkecl <- function(x){
  validFieldValues = 0
  for (y in c("amb","blu","brn","gry","grn","hzl","oth")) {
    if(grepl(as.character(y), as.character(x), fixed = TRUE)){
      validFieldValues = validFieldValues+1
    }
  }
  if(validFieldValues==1){
    return(TRUE)
  }else{return(FALSE)}
}


# pid (Passport ID) - a nine-digit number, including leading zeroes.
checkpid <- function(x){
  if(str_detect(x, "^[0-9]{9}$")){
    return(TRUE)
  }else{return(FALSE)}
}



validPassports = 0 #initialize num of valid passports
for (x in x_valid) {
  x_fields <- strsplit(x,split=" ")[[1]] #separate out all the fields
  
  validFields_num = 0
  for(x_fields_ in x_fields){
    currentField <- strsplit(x_fields_,split=":")[[1]][1]
    currentValue <- strsplit(x_fields_,split=":")[[1]][2]
    
    if(currentField=="byr"){
      
      validFields_num = validFields_num + checkbyr(currentValue)
    }else if(currentField=="iyr"){
      validFields_num = validFields_num + checkiyr(currentValue)
    }else if(currentField=="eyr"){
      validFields_num = validFields_num + checkeyr(currentValue)
    }else if(currentField=="hgt"){
      validFields_num = validFields_num + checkhgt(currentValue)
    }else if(currentField=="hcl"){
      validFields_num = validFields_num + checkhcl(currentValue)
    }else if(currentField=="ecl"){
      validFields_num = validFields_num + checkecl(currentValue)
    }else if(currentField=="pid"){
      validFields_num = validFields_num + checkpid(currentValue)
    }
  }
  if(as.numeric(validFields_num)==7){
    validPassports = validPassports+1
  }
}


print(paste("Part 2:",validPassports))
