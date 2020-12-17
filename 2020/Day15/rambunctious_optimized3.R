library(purrr)
starting <- c(1,0,18,10,19,6)
possible_nums <- rep(c(0), 30000000)
update_info <- function(num, num_list, position) {
  
  num_list[num + 1] <- position
  
  num_list
  
}
for (i in 1:(length(starting))) {
  
  possible_nums <- update_info(starting[i], possible_nums, i)
  
}
last_num <- starting[length(starting)]
last_num_match_pos <- 0
for (i in (length(starting)+1):30000000) {
  
  if (last_num_match_pos == 0) {
    
    last_num <- 0
    
    last_num_match_pos <- possible_nums[1]
    
    possible_nums[1] <- i
    
  }
  
  else {
    
    last_num <- (i-1) - last_num_match_pos
    
    last_num_match_pos <- possible_nums[last_num+1]
    
    possible_nums[last_num+1] <- i
    
    
    
  }
  
}
last_num
