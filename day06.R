library(tidyverse)
library(stringr)
input <- read_file("inputs/day06.txt")
chars
# Part 1
is_unique <- function(x, i, n) {
  x1 <- x[(i - n + 1) : i]
  if (length(x1) == length(unique(x1)))
    TRUE
  else
    FALSE
}
chars <- str_split(input, "") %>% as_vector()
for (i in 4:length(chars)) {
  if (is_unique(chars, i, 4)) {
    results_1 <- i
    break
  }
}

# Part 2

for (i in 14:length(chars)) {
  if (is_unique(chars, i, 14)) {
    results_2 <- i
    break
  }
}