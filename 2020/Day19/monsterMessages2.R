
#clear workspace
rm(list = ls())

library(tidyverse)

file = "2020/Day19/input_test1.txt"

f = function(file, fix = FALSE, n = 10) {
  d = read_file(file) %>%
    str_trim() %>%
    str_split("\n") %>%
    .[[1]]
  
  i = which(d == "")
  
  msgs  = d[(i+1):length(d)]
  
  rules = d[1:(i-1)] %>%
    tibble(rules = .) %>%
    separate(rules, c("id", "rule"), sep = ": ") %>%
    mutate(
      rule = ifelse(
        grepl("[0-9]", rule),
        paste0("( ", rule, " )"),
        rule
      ) %>%
        str_remove_all('"')
    )
  
  if (fix) {
    rules = rules %>%
      mutate(
        rule = case_when(
          # 8: 42 | 42 8 => implies 1 or more 42
          id == "8"  ~ "(( 42 )+)",
          
          # 11: 42 31 | 42 11 31 => 42 31 | 42 42 31 31 | 42 42 42 31 31 31 | etc.
          id == "11" ~ paste0("( 42 ){", 1:n, "}( 31 ){", 1:n, "}") %>%
            paste(collapse = " | ") %>% paste0("(", ., ")"),
          # otherwise
          TRUE       ~ rule
        )
      )
  }
  
  repeat {
    
    sub = !grepl(" [0-9]+ ", rules$rule)
    if (sum(!sub) == 0) {
      break
    }
    
    done = rules[sub,]
    rules = rules[!sub,]
    
    for(i in seq_len(nrow(done))) {
      id = done$id[i]
      sub = done$rule[i]
      
      repeat {
        prev = rules
        rules = rules %>% mutate(
          rule = str_replace_all(
            rule,
            paste0(" ", done$id[i], " "),
            paste0(" ", done$rule[i], " ")
          )
        )
        if (identical(rules, prev))
          break
      }
    }
    
    #print(rules$rule)
  }
  final = rules$rule[rules$id == 0]
  final_rule = paste0("^(", str_remove_all(final, " "), ")$")
  print(final_rule)
  
  msgs[ grepl(final_rule, msgs) ]
}

f("2020/Day19/input_test1.txt") %>% length()

f("2020/Day19/input.txt") %>% length()


## Part 2

f("2020/Day19/input.txt", fix=TRUE) %>% length()
