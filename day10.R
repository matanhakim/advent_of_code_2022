library(tidyverse)
library(stringi)
library(R.utils)
input <- read_lines("inputs/day10.txt") %>% 
  as_tibble() %>% 
  separate(value, into = c("op", "add_x"), sep = " ", fill = "right") %>% 
  mutate(add_x = as.integer(add_x)) %>% 
  replace_na(list(add_x = 0))

# Part 1
results_1 <- input %>% 
  mutate(
    cycle_num = if_else(op == "noop", 1, 2),
    cycle_tot = cumsum(cycle_num),
    x_end = cumsum(add_x) + 1,
    x_during = lag(x_end)
  )

get_sig_strength <- function(data, cycle){
  data %>% 
    filter(cycle_tot >= cycle) %>% 
    slice(1) %>% 
    pull(x_during) * cycle
}
vec_cycles <- seq(from = 20, to = 220, by = 40)
map_dbl(vec_cycles, ~ get_sig_strength(results_1, .x)) %>% 
  sum()

# Part 2
cycles <- results_1
counter <-0
for (i in 2:nrow(results_1)) {
  if (results_1$op[i] == "addx") {
    cycles <- cycles %>% 
      add_row(op = "addx", .before = i + counter)
    counter <- counter + 1
  }
}
cycles <- cycles %>% 
  fill(x_end) %>% 
  mutate(
    x_during = lag(x_end, default = 1),
    cycle_tot = 1:nrow(cycles),
    cycle_id = 0:(nrow(cycles) - 1),
    pixel = if_else(cycle_id %% 40 >= x_during - 1 & cycle_id %% 40 <= x_during + 1, "#", ".")
  )

cycles %>%
  pull(pixel) %>% 
  insert(seq(from = 41, to = 201, by = 40), "\n") %>% 
  str_flatten() %>% 
  writeLines()
