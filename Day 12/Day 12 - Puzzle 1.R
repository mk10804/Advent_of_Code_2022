# wd <- getwd()
# setwd(paste0(wd,"/Day 12"))

library(dplyr)
library(stringr)

# Read in data
dat <- read.table("input.txt"
                  , header = FALSE
                  , sep = "")

##############################################################################
# Puzzle 1
##############################################################################

dat_grid <- data.frame(matrix(NA,0,nchar(dat[1,])))
for(i in 1:nrow(dat)){
  dat_grid <- rbind(dat_grid
                    , str_split(dat[i,],"") %>% unlist)
}


# Find starting coords
x_S <- NA
y_S <- NA
for(r in 1:nrow(dat_grid)){
  for(c in 1:ncol(dat_grid)){
    if(dat_grid[r,c] == "S"){
      x_S <- c
      y_S <- r
    }else{
      # Do nothing
    }
  }
}

# Find ending coords
x_E <- NA
y_E <- NA
for(r in 1:nrow(dat_grid)){
  for(c in 1:ncol(dat_grid)){
    if(dat_grid[r,c] == "E"){
      x_E <- c
      y_E <- r
    }else{
      # Do nothing
    }
  }
}

# Initialize dat_path
dat_path <- data.frame(matrix(NA,0,5))
dat_path[1,] <- data.frame(x_S, y_S, x_S, y_S,"")
names(dat_path) <- c("start_x","start_y","end_x","end_y","path")

dat_path$end_coords <- paste(dat_path$end_x, dat_path$end_y)

# Function to check validity of adjacent cells
check_moves <- function(x,y,xs,ys){
  
  out_d <- NULL
  out_x <- NULL
  out_y <- NULL
  
  for(d in c("U","D","L","R")){
    xd <- ifelse(d == "U" | d == "D", 0, ifelse(d == "L", -1, 1)) # Columns
    yd <- ifelse(d == "L" | d == "R", 0, ifelse(d == "U", -1, 1)) # Rows
    
    # Check if we've reached a boundary of the grid
    x_OOB <- ifelse(d == "L" & x+xd < 1, TRUE
                    , ifelse(d == "R" & x+xd > ncol(dat_grid), TRUE, FALSE))
    
    y_OOB <- ifelse(d == "U" & y+yd < 1, TRUE
                    , ifelse(d == "D" & y+yd > nrow(dat_grid), TRUE, FALSE))
    
    # Check if the next cell is no more than 1 higher
    start_height <- ifelse(x == x_S & y == y_S, 1, match(dat_grid[y,x], letters))
    
    compare_height <- ifelse(x+xd == x_E & y+yd == y_E, 26
                    , ifelse(any(end_coords == paste(x+xd, y+yd)), 999
                    , ifelse(length(dat_grid[y+yd,x+xd]) == 0,999, match(dat_grid[y+yd,x+xd], letters))))
    
    next_OOB <- ifelse(compare_height - start_height <= 1,FALSE,TRUE)
    
    # Check if...
    # x_OOB == FALSE
    # y_OOB == FALSE
    # next_OOB == FALSE
    # then return the value
    if(x_OOB == FALSE & y_OOB == FALSE & next_OOB == FALSE){
      out_d <- c(out_d, d)
      out_x <- c(out_x, x+xd)
      out_y <- c(out_y, y+yd)
    }else{
      # Do nothing
    }
  }
  
  return(list("direction" = out_d
              , "x_coords" = out_x
              , "y_coords" = out_y))
}

# List all paths!
move_num <- 0

end_coords <- dat_path %>% 
  select(end_coords) %>% 
  unique

repeat{
  
  # Get latest set of moves
  df <- dat_path %>% 
    filter(nchar(path) == move_num)
  
  for(i in 1:nrow(df)){
    if(df[i,"end_x"] == x_E & df[i,"end_y"] == y_E){
      
      break
    
      }else{
      
      possible_moves <- check_moves(df[i,"end_x"], df[i,"end_y"], df[i,"start_x"], df[i,"start_y"])
      
      if(length(possible_moves$direction) == 0){
        #do nothing
      }else{
        for(v in 1:length(possible_moves$direction)){
          sx <- df[i,"start_x"]
          sy <- df[i,"start_y"]
          ex <- possible_moves$x_coords[v]
          ey <- possible_moves$y_coords[v]
          p <- paste0(df[i,"path"], possible_moves$direction[v], collapse="")
          ec <- paste(ex, ey)
          
          dat_path <- rbind(dat_path,
                            data.frame("start_x" = sx
                                       ,"start_y" = sy
                                       , "end_x" = ex
                                       , "end_y" = ey
                                       , "path" = p
                                       , "end_coords" = ec))
          
          end_coords <- rbind(end_coords,ec)
        }
      }
    }
  }
  print(move_num)
  move_num <- move_num + 1
}

dat_path %>% 
  filter(end_x == x_E
         , end_y == y_E) %>% 
  unique %>% 
  mutate(n_moves = nchar(path)) %>% 
  summarize(min(n_moves))
# 468 - Puzzle 1 answer

##############################################################################
# Puzzle 2
##############################################################################