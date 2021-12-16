#!/usr/bin/env Rscript

#------------------------------------------------------------------------------------------
#Load Data
#------------------------------------------------------------------------------------------
mydata <- strsplit(scan("2021/Day16/input.txt",""),"")[[1]]
example1 <- strsplit("D2FE28","")[[1]]
example2 <- strsplit("38006F45291200","")[[1]]
example3 <- strsplit("EE00D40C823060","")[[1]]
example4 <- strsplit("8A004A801A8002F478","")[[1]]
example5 <- strsplit("620080001611562C8802118E34","")[[1]]
example6 <- strsplit("C0015000016115A2E0802F182340","")[[1]]
example7 <- strsplit("A0016C880162017C3686B18A3D4780","")[[1]]
binary <- read.table("2021/Day16/binary.txt",header=FALSE,sep="",colClasses= 'character')
#------------------------------------------------------------------------------------------
#Part 1
#------------------------------------------------------------------------------------------

to_bits <- function(x){
  binary_data <- c()
  for(i in 1:length(x)){
    binary_data <- c(binary_data,binary[which(x[i] == binary$V1),3])
  }
  all_bits <- as.numeric(strsplit(paste0(binary_data,collapse=""),"")[[1]])
  return(all_bits)
}

bin_to_int = function(x) {
  sum(x*2^(seq(length(x)-1,0)))
}

literal_value = function(x) {
  binary_number <- c()
  repeat {
    if (length(x) < 5) break
    current <- x[1:5]
    x <- x[-(1:5)]
    binary_number <- c(binary_number, current[-1])
    if (current[1] == 0) break
  }
  
  list(x = x, num = bin_to_int(binary_number))
}

parse_packet = function(x, packet_version_sum = 0) {
  if(length(x) == 0) return(list(x=x, packet_version_sum = packet_version_sum))
  packet_version <- bin_to_int(x[1:3])
  packet_type <- bin_to_int(x[4:6])
  x <- x[-(1:6)]
  packet_version_sum <- packet_version_sum + packet_version
  if (packet_type == 4) {
    z <- literal_value(x)
    x <- z$x
  } else {
    length_type_id <- x[1]
    x <- x[-1]
    if (length_type_id == 0) {
      total_length <- bin_to_int(x[1:15])
      x <- x[-(1:15)]
      remaining <- x[-(1:total_length)]
      x <- x[1:total_length]
      while(length(x > 0)) {
        y <- parse_packet(x, packet_version_sum = packet_version_sum)
        x <- y$x
        packet_version_sum <- y$packet_version_sum
      }
      x <- remaining
    } else {
      num_subpackets = bin_to_int(x[1:11])
      x <- x[-(1:11)]
      for(i in 1:num_subpackets) {
        y <- parse_packet(x, packet_version_sum = packet_version_sum)
        x <- y$x
        packet_version_sum <- y$packet_version_sum
      }
    } 
  }
  return(list(x=x, packet_version_sum = packet_version_sum))
}

parse_packet(to_bits(mydata), packet_version_sum = 0)[2]


#------------------------------------------------------------------------------------------
#Part 2
#------------------------------------------------------------------------------------------

operation_list = list(
  "0" = sum,
  "1" = prod,
  "2" = min,
  "3" = max,
  "5" = function(x, y) {as.integer(x > y)},
  "6" = function(x, y) {as.integer(x < y)},
  "7" = function(x, y) {as.integer(x == y)}
)

parse_packet2 = function(x) {
  packet_version <- bin_to_int(x[1:3])
  packet_type <- bin_to_int(x[4:6])
  x <- x[-(1:6)]
  nums <- c()
  if (packet_type == 4) {
    return(literal_value(x))
  } else {
    length_type_id <- x[1]
    x <- x[-1]
    if (length_type_id == 0) {
      total_length <- bin_to_int(x[1:15])
      x <- x[-(1:15)]
      remaining <- x[-(1:total_length)]
      x <- x[1:total_length]
      nums = c()
      while(length(x > 0)) {
        y <- parse_packet2(x)
        x <- y$x
        nums <- c(nums, y$num)
      }
      x <- remaining
    } else {
      num_subpackets <- bin_to_int(x[1:11])
      x <- x[-(1:11)]
      for(i in 1:num_subpackets) {
        y <- parse_packet2(x)
        x <- y$x
        nums <- c(nums, y$num)
      }
    } 
  }
  function_to_run = operation_list[[as.character(packet_type)]]
  return(list(x=x, num=do.call(function_to_run, as.list(nums))))
}


parse_packet2(to_bits(mydata))[2]

