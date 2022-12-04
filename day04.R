library(tidyverse)
input <- read_delim("inputs/day04.txt", delim = " ", col_names = "pairs")

# Part 1

results_1 <- input %>% 
  separate(pairs, into = c("min1", "max1", "min2", "max2"), sep = "[,-]") %>% 
  mutate(
    across(everything(), as.numeric),
    is_contained = 
  ((min1 >= min2 & min1 <= max2) &
                    (max1 >= min2 & max1 <= max2)) | 
                   ((min2 >= min1 & min2 <= max1) &
                    (max2 >= min1 & max2 <= max1))
  ) %>%
  pull(is_contained) %>% 
  sum()

# Part 2

results_2 <- input %>% 
  separate(pairs, into = c("min1", "max1", "min2", "max2"), sep = "[,-]") %>% 
  mutate(
    across(everything(), as.numeric),
    is_contained = 
      ((min1 >= min2 & min1 <= max2) |
         (max1 >= min2 & max1 <= max2)) | 
      ((min2 >= min1 & min2 <= max1) |
         (max2 >= min1 & max2 <= max1))
  ) %>%
  pull(is_contained) %>% 
  sum()
