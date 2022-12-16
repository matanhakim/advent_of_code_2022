library(tidyverse)
input <- read_lines("inputs/day14.txt")

split_xy <- function(xy_vec){
  str_split(xy_vec, pattern = ",") %>%
    map(as.numeric) %>%
    map(~ matrix(.x, nrow = 1, ncol = 2))
}

rocks <- input %>% 
  str_split(" -> ") %>% 
  map(split_xy)

# Part 1 --------------------------
cave <- matrix(FALSE, nrow = 1000, ncol = 200)

draw_line <- function(prev_point, next_point, cave){
  differ <- next_point - prev_point
  differ_size <- differ %>% sum()
  if (differ[1, 1] != 0) {
    for (i in 0:differ_size){
      cave[prev_point[1,1] + i, prev_point[1,2]] <- TRUE
    }
  } else {
    for (j in 0:differ_size){
      cave[prev_point[1,1], prev_point[1,2] + j] <- TRUE
    }
  }
  cave
}

# Build cave
for (x in 1:length(rocks)){
  for (y in 1:(length(rocks[[x]]) - 1)){
    cave <- draw_line(rocks[[x]][[y]], rocks[[x]][[y + 1]], cave)
  }
}

# Drop sand

drop_sand <- function(cave, source_x){
  sand <- matrix(c(source_x, 0), nrow = 1, ncol = 2)
  down <- matrix(0:1, nrow = 1, ncol = 2)
  left <- matrix(-1:0, nrow = 1, ncol = 2)
  right <- matrix(1:0, nrow = 1, ncol = 2)
  is_blocked <- FALSE
  while(!is_blocked){
    if (sand[1, 2] == 200){ # constant fall
      break
    }
    if (!cave[sand[1, 1], sand[1, 2] + 1]){ # open down
      sand <- sand + down
    } else {
      if (!cave[sand[1, 1] - 1, sand[1, 2] + 1]) { # open left
        sand <- sand + down + left
      } else {
        if (!cave[sand[1, 1] + 1, sand[1, 2] + 1]) { # open right
          sand <- sand + down + right
        } else { # blocked
          cave[sand] <- TRUE
          is_blocked <- TRUE
        }
      }
    }
  }
  cave
}

counter <- 0
last_cave_size <- 0
while (last_cave_size != sum(cave)) {
  last_cave_size <- sum(cave)
  cave <- drop_sand(cave, 500)
  counter <- counter + 1
}

counter - 1

# Part 2 ------------------------
floor_height <- 0
for (x in 1:length(rocks)){
  for (y in 1:length(rocks[[x]])){
    floor_height <- max(floor_height, rocks[[x]][[y]][1, 2])
  }
}
floor_height <- floor_height + 2

# Build cave
cave <- matrix(FALSE, nrow = 1000, ncol = 200)
for (x in 1:length(rocks)){
  for (y in 1:(length(rocks[[x]]) - 1)){
    cave <- draw_line(rocks[[x]][[y]], rocks[[x]][[y + 1]], cave)
  }
}
cave_side <- matrix(FALSE, nrow = 1000, ncol = 200)
cave <- rbind(cave_side, cave, cave_side)
cave[, floor_height] <- TRUE

# Drop sand
counter <- 0
while(!all(cave[1499, 1], cave[1500, 1], cave[1501, 1])){
  cave <- drop_sand(cave, 1500)
  counter <- counter + 1
}
counter + 1