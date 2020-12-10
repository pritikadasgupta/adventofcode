#!/usr/bin/Rscript

#Set working directory and load in data
con <- file("stdin", open = "r")
data1 <- readLines(con) #read in file
close(con)

#-------------------------------------------------------------------------
#Clean up data 
#-------------------------------------------------------------------------
data_sorted <- sort(as.numeric(data1)) #change to numeric first and sort
data_jolts <- c(0, data_sorted, max(data_sorted) + 3)

#functions (used in part 2)
combinations <- function(x){
  return(sum(choose(x-1, 0:2)))
} 


#-------------------------------------------------------------------------
#Part 1:
#-------------------------------------------------------------------------
# What is the number of 1-jolt differences multiplied by the number of 3-jolt differences?

#I can take the "prod" of the frequency ("table") of the differences ("diff") in the data_jolts vector.
prod(table(diff(data_jolts)))
print(paste("Part 1:",prod(table(diff(data_jolts)))))

#-------------------------------------------------------------------------
#Part 2:
#-------------------------------------------------------------------------
# What is the total number of distinct ways you can arrange the adapters 
# to connect the charging outlet to your device?

data_jolts_diff <- diff(data_jolts)

df1 <- as.data.frame(unclass(rle(data_jolts_diff)))

df2 <- subset(df1, values == 1)

df3 <- transform(df2, combinations_ = sapply(lengths, combinations))

print(paste("Part 2:",prod(df3$combinations_)))

