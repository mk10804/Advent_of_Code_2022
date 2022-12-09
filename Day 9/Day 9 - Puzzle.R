# wd <- getwd()
# setwd(paste0(wd,"/Day 9"))

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

# Function to check how to adjust tail's coordinates
find_moves <- function(hx, hy, tx, ty){
  
  x_dist <- abs(hx-tx)
  y_dist <- abs(hy-ty)
  
  if(x_dist > 1 | y_dist > 1){
    x_move <- ifelse(hx-tx > 0, 1,
                     ifelse(hx-tx < 0, -1, 0))
    
    y_move <- ifelse(hy-ty > 0, 1,
                     ifelse(hy-ty < 0, -1, 0))
  }else{
    x_move <- 0
    y_move <- 0
  }
  
  return(c(x_move, y_move)) # To be added onto Tails' coordinates
}

# Make list of coordinates
dat_coords <- as.data.frame(matrix(0,1,4))
names(dat_coords) <- c("head_x","head_y","tail_x","tail_y")

for(i in 1:nrow(dat)){
  for(moves in 1:dat$V2[i]){
    curr_hx <- dat_coords[nrow(dat_coords),"head_x"]
    curr_hy <- dat_coords[nrow(dat_coords),"head_y"]
    curr_tx <- dat_coords[nrow(dat_coords),"tail_x"]
    curr_ty <- dat_coords[nrow(dat_coords),"tail_y"]
    
    # Get new head coordinates
    # Up
    if(dat[i,"V1"] == "U"){
      new_hx <- curr_hx
      new_hy <- curr_hy + 1
      
      # Down
    }else if(dat[i,"V1"] == "D"){
      new_hx <- curr_hx
      new_hy <- curr_hy - 1
      
      # Left
    }else if(dat[i,"V1"] == "L"){
      new_hx <- curr_hx - 1
      new_hy <- curr_hy
      
      # Right
    }else{
      new_hx <- curr_hx + 1
      new_hy <- curr_hy
    }
    
    # Get how tail coordinates should move
    tail_adj <- find_moves(new_hx, new_hy, curr_tx, curr_ty)
    
    new_tx <- curr_tx + tail_adj[1]
    new_ty <- curr_ty + tail_adj[2]
    
    dat_coords <- rbind(dat_coords,
                        c("head_x" = new_hx
                          , "head_y" = new_hy
                          , "tail_x" = new_tx
                          , "tail_y" = new_ty))
  }
}

dat_coords %>% 
  select(tail_x, tail_y) %>% 
  unique %>% 
  nrow()
# 5513 - Puzzle 1 answer

##############################################################################
# Puzzle 2
##############################################################################

dat_coords_10 <- as.data.frame(matrix(0,1,20))
names(dat_coords_10) <- c("knot_0_x" # Head
                          ,"knot_0_y" # Head
                          , 'knot_1_x'
                          , 'knot_1_y'
                          , 'knot_2_x'
                          , 'knot_2_y'
                          , 'knot_3_x'
                          , 'knot_3_y'
                          , 'knot_4_x'
                          , 'knot_4_y'
                          , 'knot_5_x'
                          , 'knot_5_y'
                          , 'knot_6_x'
                          , 'knot_6_y'
                          , 'knot_7_x'
                          , 'knot_7_y'
                          , 'knot_8_x'
                          , 'knot_8_y'
                          , 'knot_9_x' # Tail
                          , 'knot_9_y') # Tail

for(i in 1:nrow(dat)){
  for(moves in 1:dat$V2[i]){
    
    # Before looping through each knot, add a row that's the same as latest row
    # This new added row will be adjusted in the loop below
    dat_coords_10 <- rbind(dat_coords_10, dat_coords_10[nrow(dat_coords_10),])
    rownames(dat_coords_10) <- NULL
    
    for(k in 0:8){ #8? 9?
      
      curr_hx <- dat_coords_10[nrow(dat_coords_10),paste0("knot_",k,"_x")]
      curr_hy <- dat_coords_10[nrow(dat_coords_10),paste0("knot_",k,"_y")]
      curr_tx <- dat_coords_10[nrow(dat_coords_10),paste0("knot_",k+1,"_x")]
      curr_ty <- dat_coords_10[nrow(dat_coords_10),paste0("knot_",k+1,"_y")]
      
      # Get new head coordinates
      # Up
      if(dat[i,"V1"] == "U" & k == 0){
        new_hx <- curr_hx
        new_hy <- curr_hy + 1
        
        # Down
      }else if(dat[i,"V1"] == "D" & k == 0){
        new_hx <- curr_hx
        new_hy <- curr_hy - 1
        
        # Left
      }else if(dat[i,"V1"] == "L" & k == 0){
        new_hx <- curr_hx - 1
        new_hy <- curr_hy
        
        # Right
      }else if(dat[i,"V1"] == "R" & k == 0){
        new_hx <- curr_hx + 1
        new_hy <- curr_hy
        
      }else{
        # Is this right?
        new_hx <- curr_hx
        new_hy <- curr_hy
      }
      
      # Get how tail coordinates should move
      # tail_adj <- find_moves(new_hx, new_hy, curr_tx, curr_ty, dat[i,"V1"])
      tail_adj <- find_moves(new_hx, new_hy, curr_tx, curr_ty)
      
      new_tx <- curr_tx + tail_adj[1]
      new_ty <- curr_ty + tail_adj[2]
      
      # Update coordinates 
      dat_coords_10[nrow(dat_coords_10), paste0("knot_",k,"_x")] <- new_hx
      dat_coords_10[nrow(dat_coords_10), paste0("knot_",k,"_y")] <- new_hy
      dat_coords_10[nrow(dat_coords_10), paste0("knot_",k+1,"_x")] <- new_tx
      dat_coords_10[nrow(dat_coords_10), paste0("knot_",k+1,"_y")] <- new_ty
      
    }
  }
}

dat_coords_10 %>% 
  select(knot_9_x, knot_9_y) %>% 
  unique %>% 
  nrow()
# 2427 - Puzzle 2 answer