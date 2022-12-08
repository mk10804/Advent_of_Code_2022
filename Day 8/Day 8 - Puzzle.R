# wd <- getwd()
# setwd(paste0(wd,"/Day 8"))

library(dplyr)
library(stringr)

# Read in data
dat <- read.table("input.txt"
                  , header = FALSE
                  , sep = ""
                  , numerals = "no.loss")

##############################################################################
# Puzzle 1
##############################################################################

dat_split <- data.frame()
for(i in 1:nrow(dat)){
  
  dat_split <- rbind(dat_split,
                     as.numeric(strsplit(as.character(dat[i,]),"")[[1]]))
}

dat_vis <- data.frame(matrix(0, nrow=nrow(dat_split), ncol=ncol(dat_split)))


for(r in 1:nrow(dat_split)){
  for(c in 1:ncol(dat_split)){
    
    # If on top row or bottom row or leftmost col or rightmost col, then visible
    if(r == 1 | r == nrow(dat_split) | c == 1 | c == ncol(dat_split)){
      dat_vis[r,c] <- 1
    }else{
    
      # L R U D
      check_val <- c(0,0,0,0)
      
      # Check values to left of cell
      check_val[1] <- ifelse(dat_split[r,c] > max(dat_split[r, 1:(c-1)] %>% unlist)
                             , 1
                             , 0)
      
      # Check values to right of cell
      check_val[2] <- ifelse(dat_split[r,c] > max(dat_split[r, (c+1):ncol(dat_split)] %>% unlist)
                             , 1
                             , 0)
      
      # Check values above cell
      check_val[3] <- ifelse(dat_split[r,c] > max(dat_split[1:(r-1), c] %>% unlist)
                             , 1
                             , 0)
      
      # Check values below cell
      check_val[4] <- ifelse(dat_split[r,c] > max(dat_split[(r+1):nrow(dat_split), c] %>% unlist)
                             , 1
                             , 0)
      
      dat_vis[r,c] <- ifelse(sum(check_val) > 0, 1, 0)


    }
  }
}
sum(dat_vis)    
# 1840 -- Puzzle 1 answer

##############################################################################
# Puzzle 2
##############################################################################

dat_score <- data.frame(matrix(0, nrow=nrow(dat_split), ncol=ncol(dat_split)))

# r <- 5
# c <- 40

for(r in 1:nrow(dat_split)){
  for(c in 1:ncol(dat_split)){
    
    # If on top row or bottom row or leftmost col or rightmost col, then visible
    if(r == 1 | r == nrow(dat_split) | c == 1 | c == ncol(dat_split)){
      
      dat_score[r,c] <- 0

    }else{
      
      # L R U D
      check_val <- c(0,0,0,0)
      
      # Check values to the left of the cell
      i <- 1
      repeat{
        if(c-i < 1){
          # We've reached an edge
          break
        }else if (dat_split[r,c-i] >= dat_split[r,c]){
          # Found a tree that is the same height or higher than the tree in question
          # Stop looking at this point
          check_val[1] <- check_val[1] + 1
          break
        }else{
          # Keep looking
          check_val[1] <- check_val[1] + 1
          i <- i + 1
        }
      }
      
      #REPEAT Check values to the right of the cell
      i <- 1
      repeat{
        if(c+i > ncol(dat_split)){
          # We've reached an edge
          break
        }else if (dat_split[r,c+i] >= dat_split[r,c]){
          # Found a tree that is the same height or higher than the tree in question
          # Stop looking at this point
          check_val[2] <- check_val[2] + 1
          break
        }else{
          check_val[2] <- check_val[2] + 1
          i <- i + 1
        }
      }
      
      #REPEAT Check values above the cell
      i <- 1
      repeat{
        if(r-i < 1){
          # We've reached an edge
          break
        }else if (dat_split[r-i,c] >= dat_split[r,c]){
          # Found a tree that is the same height or higher than the tree in question
          # Stop looking at this point
          check_val[3] <- check_val[3] + 1
          break
        }else{
          check_val[3] <- check_val[3] + 1
          i <- i + 1
        }
      }
      
      #REPEAT Check values below the cell
      i <- 1
      repeat{
        if(r+i > ncol(dat_split)){
          # We've reached an edge
          break
        }else if (dat_split[r+i,c] >= dat_split[r,c]){
          # Found a tree that is the same height or higher
          # than the tree in question
          check_val[4] <- check_val[4] + 1
          break
        }else{
          check_val[4] <- check_val[4] + 1
          i <- i + 1
        }
      }

      dat_score[r,c] <- check_val[1] * check_val[2] * check_val[3] * check_val[4]
    }
  }
}

max(dat_score)
# 405769 - TOO LOW - Puzzle 2 answer