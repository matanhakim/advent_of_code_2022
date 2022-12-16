library(tidyverse)
input <- readLines("inputs/day12.txt")
mat <- input %>% 
  strsplit("") %>% 
  as_vector() %>% 
  matrix(nrow = length(input), ncol = nchar(input[1]), byrow = TRUE)
start_point <- which(mat == "S", arr.ind = TRUE)
end_point <- which(mat == "E", arr.ind = TRUE)
mat <- match(mat, letters) %>% matrix(nrow = length(input), ncol = nchar(input[1]))
mat[start_point] <- 1
mat[end_point] <- 26

# Part 1
check_step <- function(i, j, steps){
  up <- FALSE
  down <- FALSE
  left <- FALSE
  right <- FALSE
  if (i > 1)
    up <- all(steps[i - 1, j] > 0, mat[i - 1, j] >= mat[i, j] - 1)
  if (i < nrow(mat))
    down <- all(steps[i + 1, j] > 0, mat[i + 1, j] >= mat[i, j] - 1)
  if (j > 1)
    left <- all(steps[i, j - 1] > 0, mat[i, j - 1]  >= mat[i, j] - 1)
  if (j < ncol(mat))
    right <- all(steps[i, j + 1] > 0, mat[i, j + 1] >= mat[i, j] - 1)
  self <- steps[i, j] > 0
  any(up, down, left, right, self)
}

steps <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
steps[start_point] <- 1
counter <- 0
while(steps[end_point] == 0) {
  counter <- counter + 1
  does_step <- rep(FALSE, ncol(mat) * nrow(mat))
  z <- 1
  for (j in 1:ncol(mat)) {
    for (i in 1:nrow(mat)) {
      if (check_step(i, j, steps)) {
        does_step[z] <- TRUE
      }
      z <- z + 1
    }
  }
  steps[does_step] <- steps[does_step] + 1
}
counter

# Part 2
check_step_up <- function(i, j, steps){
  up <- FALSE
  down <- FALSE
  left <- FALSE
  right <- FALSE
  if (i > 1)
    up <- all(steps[i - 1, j] > 0, mat[i - 1, j] <= mat[i, j] + 1)
  if (i < nrow(mat))
    down <- all(steps[i + 1, j] > 0, mat[i + 1, j] <= mat[i, j] + 1)
  if (j > 1)
    left <- all(steps[i, j - 1] > 0, mat[i, j - 1]  <= mat[i, j] + 1)
  if (j < ncol(mat))
    right <- all(steps[i, j + 1] > 0, mat[i, j + 1] <= mat[i, j] + 1)
  self <- steps[i, j] > 0
  any(up, down, left, right, self)
}

steps_2 <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
steps_2[end_point] <- 1
a_points <- which(mat == 1)
counter <- 0
while(sum(steps_2[a_points]) == 0) {
  counter <- counter + 1
  does_step <- rep(FALSE, ncol(mat) * nrow(mat))
  z <- 1
  for (j in 1:ncol(mat)) {
    for (i in 1:nrow(mat)) {
      if (check_step_up(i, j, steps_2)) {
        does_step[z] <- TRUE
      }
      z <- z + 1
    }
  }
  steps_2[does_step] <- steps_2[does_step] + 1
}
counter