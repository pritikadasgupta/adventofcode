#!/usr/bin/Rscript

con <- file("stdin", open = "r")
data1 <- readLines(con) #read in file
close(con)

data_sorted <- sort(as.numeric(data1)) #change to numeric first and sort
data_jolts <- c(0, data_sorted, max(data_sorted) + 3)

combinations <- function(x){
  return(sum(choose(x-1, 0:2)))
} 

prod(table(diff(data_jolts)))
print(paste("Part 1:",prod(table(diff(data_jolts)))))


data_jolts_diff <- diff(data_jolts)

df1 <- as.data.frame(unclass(rle(data_jolts_diff)))

df2 <- subset(df1, values == 1)

df3 <- transform(df2, combinations_ = sapply(lengths, combinations))

print(paste("Part 2:",prod(df3$combinations_)))
