calories <- readLines("inputs/day01.txt")
calories <- as.numeric(calories)

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