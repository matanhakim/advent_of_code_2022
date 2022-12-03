library(tidyverse)
input <- read_delim("inputs/day03.txt", delim = " ", col_names = "items")

# Part 1
conv_df <- tibble(
  letter = c(letters, LETTERS),
  result = 1:52
)

results_1 <- input %>% 
  mutate(
    items_num = str_length(items),
    comp_1 = str_sub(items, end = items_num / 2),
    comp_2 = str_sub(items, start = 1 + items_num / 2),
    spec_item = str_extract(comp_1, str_c("[", comp_2, "]"))
  ) %>% 
  left_join(conv_df, by = c("spec_item" = "letter")) %>% 
  pull(result) %>% 
  sum()

# Part 2

find_badge <- function(data){
  data %>% 
    pivot_wider(values_from = items, names_from = items) %>% 
    rename(x1 = 1, x2 = 2, x3 = 3) %>% 
    mutate(
      result = str_flatten(
        as_vector(
          str_extract_all(x1, str_c("[", x2, "]")) 
        )
      ),
      result = str_extract(result, str_c("[", x3, "]"))
    ) %>% 
    pull(result)
}

results_2 <- input %>% 
  mutate(group = rep(1:(nrow(.) / 3), each = 3)) %>% 
  group_by(group) %>% 
  nest() %>% 
  mutate(letter = map_chr(data, find_badge)) %>% 
  left_join(conv_df, by = "letter") %>% 
  pull(result) %>% 
  sum()