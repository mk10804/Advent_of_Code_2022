# wd <- getwd()
# setwd(paste0(wd,"/Day 13"))

library(dplyr)
library(stringr)

# Read in data
dat <- read.table("input.txt"
                  , header = FALSE
                  , sep = "")

##############################################################################
# Puzzle 1
##############################################################################
pairs <- nrow(dat) / 2

dat_pairs <- dat %>% 
  mutate(p = sort(rep(1:pairs,2)))

in_order <- c()

for(i in 1:pairs){
  df <- dat_pairs %>% 
    filter(p == i)
  
  df_x <- strsplit(df[1,1],"") %>% unlist
  df_y <- strsplit(df[2,1],"") %>% unlist
  p <- df$p[1]
  
  in_order <- NA
  
  v <- 1
  
  repeat{
    # If x = y = [, move to the next value
    if(df_x[v] == "[" & df_y[v] == "["){
      v <- v + 1
    }
  }
  
}

gsub("\\[|\\]|,", "", df_l)
gsub("\\[|\\]|,", "", df_r)
