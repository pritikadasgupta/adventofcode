#!/usr/bin/Rscript

#clear workspace
rm(list = ls())

# Libraries

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------

#Use this if you're running from the command line:
# mydata <- readLines(file("stdin"))

#Use this if you're opening this repo as a R project, using relative paths:
# mydata <- strsplit(readLines('2020/Day19/input_test1.txt')," ")
mydata <- readLines('2020/Day21/input.txt')

#------------------------------------------------------------------------------------------
#Functions
#------------------------------------------------------------------------------------------
parser <- function(x){
  ingredients <- strsplit(x,"contains")[[1]][1]
  allergens <- strsplit(x,"contains")[[1]][2]
  
  ingredients <- strsplit(ingredients," ")[[1]]
  for(i in 1:length(ingredients)){
    if(ingredients[i]=="("){
      ingredients <- ingredients[-i]
    }
  }
  
  allergens <- strsplit(allergens," ")[[1]]
  allergens <- allergens[2:length(allergens)]
  new_allergens <- vector()
  for(i in 1:length(allergens)){
    new_allergens <- append(new_allergens,strsplit(allergens[i],",")[[1]])
  }
  
  new_allergens2 <- vector()
  for(i in 1:length(new_allergens)){
    new_allergens2 <- append(new_allergens2,strsplit(new_allergens[i],")")[[1]])
  }
  
  ingredients_df <- vector()
  allergens_df <- vector()
  for(i in 1:length(ingredients)){
    for(j in 1:length(new_allergens2)){
      ingredients_df <- append(ingredients_df,ingredients[i])
      allergens_df <- append(allergens_df,new_allergens2[j])
    }
  }
  
  df <- as.data.frame(cbind(ingredients_df,allergens_df))
  
  return(df)
}

#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------
mydf <- data.frame(ingredients_df=character(),allergens_df=character())
for(x in mydata){
  mydf <- rbind(mydf,parser(x))
}

unique_ingredients <- unique(mydf$ingredients_df)
unique_allergens <- unique(mydf$allergens_df)

mylist <- vector(mode = "list", length = length(unique_allergens))
k=1
for(allergen in unique_allergens){
  mylist[[k]] <- unique_ingredients
  k=k+1
}
names(mylist) <- unique_allergens


for(i in 1:length(mydata)){
  curdf <- parser(mydata[i])
  curr_allergens <- unique(curdf$allergens_df)
  curr_ingredients <- unique(curdf$ingredients_df)
  for (allergen in curr_allergens){
    mylist[allergen][[1]] <- intersect(mylist[allergen][[1]],curr_ingredients)
  }
}

outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

possible_allergen <- unique(unlist(mylist))
non_allergen <- outersect(unique_ingredients,possible_allergen)


total = 0
for (x in mydata){
  curdf2 <- parser(x)
  curr_ingredients <- unique(curdf2$ingredients_df)
  for(ingredient in curr_ingredients){
    if(ingredient %in% non_allergen){
      total=total+1
    }
  }
  
}

print(total)


#part 2:
found <- mylist
a=1
#for singletons
# for(a in 1:length(unique_allergens)){
while(a <=length(unique_allergens)){
  allergen <- unique_allergens[a]
  ingredients_ <- mylist[[allergen]]
  if(length(ingredients_)==1){
    found[[allergen]] <- ingredients_
    mylist[[allergen]] <- NULL

    unique_allergens <- unique_allergens[!unique_allergens %in% allergen]
    
    for(allergen2 in unique_allergens){
      mylist[[allergen2]] <- mylist[[allergen2]][!mylist[[allergen2]] %in% ingredients_]
    }
    a=1
  }else{
    a = a+1
  }
}



df <- as.data.frame(found)
names_df <- sort(colnames(df))

to_paste <- paste(found[names_df[1]])
for(i in 2:length(names_df)){
  to_paste <- paste(to_paste,",",found[names_df[i]],sep="")
}

print(to_paste)

# nfnfk,nbgklf,clvr,fttbhdr,qjxxpr,hdsm,sjhds,xchzh
