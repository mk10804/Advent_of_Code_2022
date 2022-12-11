# wd <- getwd()
# setwd(paste0(wd,"/Day 10"))

library(dplyr)
library(reshape2)

# Read in data
dat <- read.table("input.txt"
                  , header = FALSE
                  , sep = ""
                  , numerals = "no.loss"
                  , fill = TRUE)

##############################################################################
# Puzzle 1
##############################################################################

x <- 1
cycle <- 0

dat_cycle <- data.frame(matrix(NA,0,3))
names(dat_cycle) <- c("cycle_num","x_during","x_after")

for(i in 1:nrow(dat)){
  if(dat[i,"V1"] == "noop"){
    
    cycle <- cycle + 1
    dat_cycle <- rbind(dat_cycle,
                       c(cycle, x, x))
  }else{
    
    cycle <- cycle + 1
    dat_cycle <- rbind(dat_cycle,
                       c(cycle, x, x))
    
    cycle <- cycle + 1
    dat_cycle <- rbind(dat_cycle,
                       c(cycle, x, x + dat[i,"V2"]))
    x <- x + dat[i,"V2"]

  }
}

names(dat_cycle) <- c("cycle_num","x_during","x_after")

dat_cycle$signal_strength = dat_cycle$cycle_num * dat_cycle$x_during

interesting_cycles <- seq(from = 20, to = 220, by = 40)

dat_cycle %>% 
  filter(cycle_num %in% interesting_cycles) %>%
  select(signal_strength) %>% 
  summarize(sum(signal_strength))

# 16480 -- Puzzle 1 answer

##############################################################################
# Puzzle 2
##############################################################################

num_row <- 6
num_col <- 40
dat_cycle$crt_col <- rep(1:num_col,num_row)
dat_cycle$crt_row <- sort(rep(1:num_row, num_col))

dat_cycle$left_point <- dat_cycle$x_during
dat_cycle$right_point <- dat_cycle$x_during + 2

dat_cycle$pixel <- ifelse(dat_cycle$crt_col >= dat_cycle$left_point & dat_cycle$crt_col <= dat_cycle$right_point,"#",".")

dat_pic <- dat_cycle %>% select(crt_col, crt_row, pixel)
dat_pic <- dcast(dat_pic, crt_row ~ crt_col, value.var = 'pixel')
View(dat_pic)
# PLEFULPB