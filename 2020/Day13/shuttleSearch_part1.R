#!/usr/bin/Rscript

#clear workspace
rm(list = ls())

#Load Data
#Use this if you're running from the command line:
# mydata <- readLines(file("stdin")) #read in file

#Use this if you're opening this repo as a R project, using relative paths:
mydata <- readLines("2020/Day13/input")

data1 <- mydata[1]
data2 <- mydata[2]

#-------------------------------------------------------------------------
#Clean up bus data 
#-------------------------------------------------------------------------
data1_numeric <- as.numeric(data1) #change your time to numeric
data2_split <- strsplit(data2,",")[[1]] #get all buses
time_buses <- seq(1,data1_numeric+50,by=1)

#get all bus types
bus_type <- vector()
for(datum in data2_split){
  if(datum!="x"){
    bus_type <- append(bus_type,as.numeric(datum))
  }
}

# numberofcolumns <- length(bus_type)
numberofrows <- length(time_buses)

#create data frame for bus types and times
df <- data.frame(matrix(ncol = 0, nrow = numberofrows))
df[,1] <- time_buses
colnames(df)[1] <- "time"
df2 <- df
for(j in 1:length(bus_type)){
  start <- as.numeric(bus_type[j])
  end <- as.numeric(nrow(df))
  time_list <- as.vector(seq(start,end,by=start))
  bus_list <- as.vector(rep(1,length(time_list)))
  df_combine <- cbind(time_list,bus_list)
  colnames(df_combine) <- c("time",as.character(bus_type[j]))
  df2 <- merge(df2,df_combine,by="time",all=TRUE)
}

#-------------------------------------------------------------------------
#Clean up your data
#-------------------------------------------------------------------------

mytime <- seq(data1_numeric,data1_numeric+50,by=1)
mytime_available <- rep(1,length(mytime))

mydf <- cbind(mytime,mytime_available)
colnames(mydf) <- c("time","me")
df2 <- merge(df2,mydf,by="time",all=TRUE)

finaldf <- subset(df2, time>=data1_numeric)
#-------------------------------------------------------------------------
#Part 1:
#-------------------------------------------------------------------------

finaldf$bustotake <- rowSums(finaldf[,c(2:length(bus_type)+1)],na.rm=TRUE)

answers <- vector()
for(i in 1:nrow(finaldf)){
  if(finaldf$bustotake[i] == 1){
    myvector <- finaldf[i,]
    myidx <- which(finaldf[i,c(2:length(bus_type)+1)]==1)
    bus_number <- bus_type[myidx+1]
    answer <- bus_number*(finaldf$time[i]-data1_numeric)
    answers <- append(answers,answer)
}}

print(paste("Part 1:",answers[1]))

