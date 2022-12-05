# wd <- getwd()
# setwd(paste0(wd,"/Day 4"))

library(dplyr)
library(stringr)

# Read in data
dat <- read.table("input.txt"
                  , header = FALSE
                  , sep = ""
                  , dec = "."
                  , blank.lines.skip = FALSE)
names(dat) <- c('assignments')

##############################################################################
# Puzzle 1
##############################################################################

# Split assignments into 2 columns
for(i in 1:nrow(dat)){
  dat$assignment_1[i] <- strsplit(dat$assignments, split = ",")[[i]][1]
  dat$assignment_2[i] <- strsplit(dat$assignments, split = ",")[[i]][2]
}

# Split assignments end points into columns
for(i in 1:nrow(dat)){
  dat$assignment_1_min[i] <- strsplit(dat$assignment_1, split = "-")[[i]][1]
  dat$assignment_1_max[i] <- strsplit(dat$assignment_1, split = "-")[[i]][2]
  
  dat$assignment_2_min[i] <- strsplit(dat$assignment_2, split = "-")[[i]][1]
  dat$assignment_2_max[i] <- strsplit(dat$assignment_2, split = "-")[[i]][2]
}

# Convert factors to numeric
dat$assignment_1_min <- as.numeric(dat$assignment_1_min)
dat$assignment_2_min <- as.numeric(dat$assignment_2_min)
dat$assignment_1_max <- as.numeric(dat$assignment_1_max)
dat$assignment_2_max <- as.numeric(dat$assignment_2_max)

# Apply logic
dat <- dat %>% 
  mutate(contained_1_in_2 = ifelse(assignment_1_min >= assignment_2_min & assignment_1_max <= assignment_2_max, 1, 0)
         , contained_2_in_1 = ifelse(assignment_2_min >= assignment_1_min & assignment_2_max <= assignment_1_max, 1, 0)
         , contained = ifelse(contained_1_in_2 + contained_2_in_1 == 0, 0, 1)
         )

sum(dat$contained) # 513 -- Puzzle 1 answer

##############################################################################
# Puzzle 2
##############################################################################

dat <- dat %>% 
  # overlap doesn't occur if the max of one assignment is strictly less than the min of the other
  mutate(overlap = ifelse(assignment_1_max < assignment_2_min | assignment_2_max < assignment_1_min, 0, 1)
  )

sum(dat$overlap) # 878 -- Puzzle 2 answer