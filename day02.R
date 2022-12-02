library(tidyverse)
input <- read_delim(
  "inputs/day02.txt",
  delim = " ",
  col_names = c("opp", "player")
  )

# Prt 1
results <- input %>% 
  mutate(
    res_opp = case_when(
      opp == "A" ~ 1,
      opp == "B" ~ 2,
      opp == "C" ~ 3
    ),
    res_player = case_when(
      player == "X" ~ 1,
      player == "Y" ~ 2,
      player == "Z" ~ 3,
    ),
    result = case_when(
      res_opp == res_player ~ 3, # Draw
      (res_player - res_opp == 1) | (res_player - res_opp == -2) ~ 6, # Win
      TRUE ~ 0 # Lose
    ),
    result = res_player + result
  )

result <- results %>% 
  pull(result) %>% 
  sum()

# Part 2

results_2 <- results %>% 
  mutate(
    res_player_2 = case_when(
      player == "Y" ~ res_opp + 3, # Draw
      player == "X" & opp == "A" ~ 3, # Lose with scissors
      player == "X" ~ res_opp - 1, # Lose w/o scissors
      player == "Z" & opp == "C" ~ 1 + 6, # Win with rock
      player == "Z" ~ res_opp + 1 + 6 # Win w/o rock
    )
  ) %>% 
  pull(res_player_2) %>% 
  sum()
