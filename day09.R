library(tidyverse)
input <- read_delim("inputs/day09.txt", delim = " ", col_names = c("direction", "step_num"))

# Part 1
steps <- input %>% 
  uncount(step_num) %>% 
  mutate(
    step_h_x = case_when(
      direction == "R" ~  1,
      direction == "L" ~ -1,
      TRUE ~ 0
    ),
    step_h_y = case_when(
      direction == "U" ~  1,
      direction == "D" ~ -1,
      TRUE ~ 0
    ),
    h_x = cumsum(step_h_x),
    h_y = cumsum(step_h_y),
    t_x = 0,
    t_y = 0
  )

step_t <- function(t_x, t_y, h_x_new, h_y_new){
  dist <- sqrt((h_x_new - t_x)^2 + (h_y_new - t_y)^2)
  t_x_new <- case_when(
    dist < 2 ~ t_x,
    dist == 2 ~ (t_x + h_x_new) / 2,
    h_x_new > t_x ~ t_x + 1,
    TRUE ~ t_x - 1
  )
  t_y_new <- case_when(
    dist < 2 ~ t_y,
    dist == 2 ~ (t_y + h_y_new) / 2,
    h_y_new > t_y ~ t_y + 1,
    TRUE ~ t_y - 1
  )
  c(t_x_new, t_y_new) %>% as.integer()
}

for (i in 2:nrow(steps)){
  new_t <- step_t(steps$t_x[i - 1], steps$t_y[i - 1], steps$h_x[i], steps$h_y[i])
  steps$t_x[i] <- new_t[1]
  steps$t_y[i] <- new_t[2]
}

steps %>%
  distinct(t_x, t_y) %>% 
  nrow()

# Part 2
steps2 <- steps
for (j in (ncol(steps) + 1):(ncol(steps) + 16)){
  steps2[j] <- 0
}

for(i in 2:nrow(steps2)) {
  for (j in seq(from = ncol(steps) + 1, to = ncol(steps) + 16, by = 2)){
    new_t <- step_t(steps2[[i - 1, j]], steps2[[i - 1, j + 1]], steps2[[i, j - 2]], steps2[[i, j - 1]])
    steps2[[i, j]] <- new_t[1]
    steps2[[i, j + 1]] <- new_t[2]
  }
}
  
steps2 %>%
  rename(t_x_last = ...22, t_y_last = ...23) %>% 
  distinct(t_x_last, t_y_last) %>% 
  nrow()