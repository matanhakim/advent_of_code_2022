library(tidyverse)
library(stringi)
crates <- readLines("inputs/day05.txt", n = 8) %>% 
  as_tibble() %>% 
  separate(col = value, into = str_c("x", 1:9), sep = 1:8 * 4) %>% 
  mutate(across(everything(), ~ str_sub(.x, 2, 2))) %>% 
  map(., str_flatten) %>% 
  map(., stri_reverse) %>% 
  map(., str_trim)

moves <- read_delim("inputs/day05.txt", delim = " ", skip = 9, col_names = FALSE) %>% 
  select(move = 2, from = 4, to = 6)

# Part 1

move_crates <- function(crates, move, from, to){
  crates[[to]] <- str_c(
    crates[[to]],
    stri_reverse(str_sub(crates[[from]], start = - move))
  )
  crates[[from]] <- str_sub(crates[[from]], end = - move - 1)
  
  crates
}

crates_final <- crates
for(i in 1:nrow(moves)) {
  crates_final <- move_crates(crates_final, moves[[i, 1]], moves[[i, 2]], moves[[i, 3]])
}

results_1 <- crates_final %>%
  map_chr(~ str_sub(.x, start = -1)) %>% 
  str_flatten()

# Part 2

move_crates_2 <- function(crates, move, from, to){
  crates[[to]] <- str_c(
    crates[[to]],
    str_sub(crates[[from]], start = - move)
  )
  crates[[from]] <- str_sub(crates[[from]], end = - move - 1)
  
  crates
}

crates_final_2 <- crates
for(i in 1:nrow(moves)) {
  crates_final_2 <- move_crates_2(crates_final_2, moves[[i, 1]], moves[[i, 2]], moves[[i, 3]])
}

results_2 <- crates_final_2 %>%
  map_chr(~ str_sub(.x, start = -1)) %>% 
  str_flatten()