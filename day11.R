library(tidyverse)
library(janitor)
input <- read_lines("inputs/day11.txt")
monkeys <- tibble(x = input) %>% 
  separate(x, sep = ":", into = c("col", "value")) %>% 
  mutate(id = cumsum(col == "")) %>% 
  filter(col != "", !str_detect(col, "Monkey ")) %>% 
  pivot_wider(id, names_from = col, values_from = value) %>% 
  clean_names() %>% 
  mutate(
    starting_items = str_split(starting_items, ","),
    starting_items = map(starting_items, str_trim),
    starting_items = map(starting_items, as.numeric),
    test_num = str_extract(test, "(?<=divisible by ).*$"),
    if_true = str_extract(if_true, "(?<= throw to monkey ).*$"),
    if_false = str_extract(if_false, "(?<= throw to monkey ).*$"),
    across(c(if_true, if_false, test_num), as.numeric)
  )

# Part 1
monkeys <- monkeys %>% 
  mutate(items_inspected = 0, items = starting_items)
  
for (rounds in 1:20){
  for (i in 1:nrow(monkeys)){
    if (is_empty(monkeys$items[[i]]) )
      next
    for(item in 1:length(monkeys$items[[i]])){
      old <- monkeys$items[[i]][item]
      eval(parse(text = monkeys$operation[i]))
      new <- new %/% 3
      test_result <- new %% monkeys$test_num[i] == 0
      if (test_result)
        monkeys$items[[monkeys$if_true[i] + 1]] <- c(monkeys$items[[monkeys$if_true[i] + 1]], new)
      else 
        monkeys$items[[monkeys$if_false[i] + 1]] <- c(monkeys$items[[monkeys$if_false[i] + 1]], new)
      monkeys$items_inspected[i] <- monkeys$items_inspected[i] + 1
    }
    monkeys$items[[i]] <- monkeys$items[[i]][FALSE]
  }
}
monkeys %>% 
  arrange(desc(items_inspected)) %>% 
  slice_head(n = 2) %>% 
  pull(items_inspected) %>% 
  prod()

# part 2

monkeys <- monkeys %>% 
  mutate(items_inspected = 0, items = starting_items)

mod_stop <- monkeys %>% pull(test_num) %>% prod()

for (rounds in 1:10000){
  for (i in 1:nrow(monkeys)){
    if (is_empty(monkeys$items[[i]]) )
      next
    for(item in 1:length(monkeys$items[[i]])){
      old <- monkeys$items[[i]][item]
      eval(parse(text = monkeys$operation[i]))
      if (new >= mod_stop)
        new <- new %% mod_stop
      test_result <- new / monkeys$test_num[i] == new %/% monkeys$test_num[i]
      if (test_result)
        monkeys$items[[monkeys$if_true[i] + 1]] <- c(monkeys$items[[monkeys$if_true[i] + 1]], new)
      else 
        monkeys$items[[monkeys$if_false[i] + 1]] <- c(monkeys$items[[monkeys$if_false[i] + 1]], new)
      monkeys$items_inspected[i] <- monkeys$items_inspected[i] + 1
    }
    monkeys$items[[i]] <- monkeys$items[[i]][FALSE]
  }
}
monkeys %>% 
  arrange(desc(items_inspected)) %>% 
  slice_head(n = 2) %>% 
  pull(items_inspected) %>% 
  prod()
