setwd("C:/Users/marla/OneDrive/Desktop/Advent of Code 2022/Code-Advent/Day 1")
library(dplyr)

# Read in data
dat <- read.table("input.txt"
                  , header = FALSE
                  , sep = ""
                  , dec = "."
                  , blank.lines.skip = FALSE)
names(dat) <- 'calories'

# Figure out which records belong to individual elves
dat$elf <- NA
dat$elf[1] <- 1

for(i in 2:nrow(dat)){
  dat[i, 'elf'] <- ifelse(is.na(dat[i, 'calories']), dat[i - 1, 'elf'] + 1, dat[i - 1, 'elf'])
  
}

dat$calories <- ifelse(is.na(dat$calories), 0, dat$calories)

# Aggregate info
dat_agg <- dat %>% 
  group_by(elf) %>% 
  summarize(calories = sum(calories)) %>% 
  ungroup %>% 
  mutate(max_cal = max(calories)) %>% 
  filter(calories == max_cal)

dat_agg$calories # Puzzle 1 answer -- 65912

dat_rank <- dat %>% 
  group_by(elf) %>% 
  summarize(calories = sum(calories)) %>% 
  ungroup %>% 
  arrange(desc(calories))

sum(dat_rank[1:3,'calories']) # Puzzle 2 answer -- 195625
