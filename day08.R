library(tidyverse)
input <- read_table("inputs/day08.txt", col_names = FALSE)
input_width <- input[[1,1]] %>% str_length()
input <- input %>% 
  separate(X1, into = str_c("x", 1:input_width), sep = 1:(input_width - 1)) %>% 
  mutate(across(everything(), as.numeric)) %>% 
  as.matrix()

# Part 1
is_visible <- function(mat, i, j){
  if (any(i == 1, i == nrow(mat), j == 1, j == ncol(mat)))
    return(TRUE)
  up <- mat[1:(i - 1), j]
  down <- mat[(i + 1):nrow(mat), j]
  left <- mat[i, 1:(j - 1)]
  right <- mat[i, (j + 1):ncol(mat)]
  any(mat[i, j] > max(up), mat[i, j] > max(down),
      mat[i, j] > max(left), mat[i, j] > max(right))
}

results_1 <- matrix(NA, nrow = nrow(input), ncol = ncol(input))
for (i in 1:nrow(input)) {
  for (j in 1:ncol(input)) {
    results_1[i, j] <- is_visible(input, i, j)
  }
}
results_1 %>% sum %>% print()

# Part 2
scene_length <- function(x, y){
  for (i in 1:length(y)){
    if (x <= y[i])
      break
  }
  i
}
scenic_score <- function(mat, i, j){
  up <- scene_length(mat[i, j], rev(mat[1:(i - 1), j]))
  down <- scene_length(mat[i, j], mat[(i + 1):nrow(mat), j])
  left <- scene_length(mat[i, j], rev(mat[i, 1:(j - 1)]))
  right <- scene_length(mat[i, j], mat[i, (j + 1):ncol(mat)])
  up * down * left * right
}

results_2 <- matrix(0, nrow = nrow(input), ncol = ncol(input))
for (i in 2:(nrow(input) - 1)) {
  for (j in 2:(ncol(input) - 1)) {
    results_2[i, j] <- scenic_score(input, i, j)
  }
}
results_2 %>% max() %>% print()