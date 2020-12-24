linked_cups <- function(circle, moves = 10, terms = 9) {
  ncups <- length(circle)
  stopifnot(terms <= ncups)
  # Linked list where each index is the label of the preceding cup.
  lnklst <- c(circle[-1], circle[1])[order(circle)]
  current <- circle[1]
  for (iter in 1:moves) {
    # Pick the 3 next cups.
    grab1 <- lnklst[current]
    grab2 <- lnklst[grab1]
    grab3 <- lnklst[grab2]
    # Choose destination.
    dest <- 1 + (current - 1:4 - 1) %% ncups
    dest <- dest[which.min(dest %in% c(grab1, grab2, grab3))]
    # Lift out the 3 cups.
    lnklst[current] <- lnklst[grab3]
    # Slot in the 3 cups.
    lnklst[grab3] <- lnklst[dest]
    lnklst[dest] <- grab1
    # Move clockwise by 1.
    current <- lnklst[current]
  }
  
  # Convert back into a vector of labels for output.
  cups <- integer(terms)
  cups[1] <- 1
  for (i in 2:terms) cups[i] <- lnklst[cups[i-1]]
  cups
}


input <- "158937462"
input <- as.numeric(strsplit(input,"")[[1]])

result2 <- linked_cups(c(input, 10:1e6), 1e7, terms = 3)
prod(result2)