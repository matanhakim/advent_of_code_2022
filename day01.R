calories <- readLines("inputs/day01.txt")
calories <- as.numeric(calories)

# Part one
max_cal <- 0
cal_count <- 0
for (i in calories){
  if (is.na(i)){
    if (cal_count > max_cal){
      max_cal <- cal_count
    }
    cal_count <- 0
  } else {
    cal_count <- cal_count + i
  }
}

# Part two
library(tidyverse)
input <- tibble(calories) %>% 
  mutate(elf = cumsum(is.na(calories)))

output <- input %>% 
  drop_na(calories) %>% 
  group_by(elf) %>% 
  summarise(calories = sum(calories)) %>% 
  arrange(desc(calories)) %>% 
  slice_head(n = 3) %>% 
  pull() %>% 
  sum()
