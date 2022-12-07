# wd <- getwd()
# setwd(paste0(wd,"/Day 6"))

library(dplyr)
library(stringr)

# Read in data
dat <- read.table("input.txt"
                  , header = FALSE
                  , sep = ""
                  , dec = ".")

##############################################################################
# Puzzle 1
##############################################################################

i <- 1
repeat{
  if(length(unique(strsplit(substr(dat, i, i + 3),"")[[1]])) == 4){
    print(i + 3)
    break
  }else{
    i <- i + 1
  }
}
# 1109 -- Puzzle 1 Answer

##############################################################################
# Puzzle 2
##############################################################################

i <- 1
repeat{
  if(length(unique(strsplit(substr(dat, i, i + 13),"")[[1]])) == 14){
    print(i + 13)
    break
  }else{
    i <- i + 1
  }
}
# 3965
