# wd <- getwd()
# setwd(paste0(wd,"/Day 14"))

library(dplyr)
library(stringr)

# Read in data
dat <- read.table("input.txt"
                  , header = FALSE
                  , fill = TRUE)

##############################################################################
# Puzzle 2
##############################################################################

# Keep odd numbered columns
dat <- dat[,seq(1,ncol(dat),2)]

dat_coords <- data.frame(matrix(NA,0,3))
names(dat_coords) <- c("line","x","y")

# Parse data
for(r in 1:nrow(dat)){
  for(c in 1:ncol(dat)){
    
    if(dat[r,c] == ""){
      #Do nothing
    }else{
      x <- str_split(dat[r,c],",")[[1]][1] %>% as.numeric
      y <- str_split(dat[r,c],",")[[1]][2] %>% as.numeric
      line <- r
      dat_coords <- rbind(dat_coords
                          , data.frame(line,x,y))
    }
  }
}

dat_coords$y <- dat_coords$y + 1 # Top row starts at index = 0, not 1

# Functions
new_coords <- function(x1,x2,y1,y2){
  if(x1 == x2){
    d <- "UD"
    x_d <- 0
    y_d <- y2 - y1
  }else{
    d <- "LR"
    x_d <- x2 - x1
    y_d <- 0
  }
  
  new_x <- ifelse(d == "UD", x1,
                  ifelse(x_d > 0, x1 + 1, x1 - 1))
  
  new_y <- ifelse(d == "LR", y1,
                  ifelse(y_d > 0, y1 + 1, y1 - 1))
  
  return(data.frame(new_x, new_y))
}

find_moves <- function(x1,x2,y1,y2){
  if(x1 == x2){
    moves <- abs(y2 - y1)
  }else{
    moves <- abs(x2 - x1)
  }
  return(moves)
}


### Chart the path
n_row <- max(dat_coords$y) + 1
n_col <- max(dat_coords$x) - min(dat_coords$x) + 1
dat_path <- data.frame(matrix(".",n_row, n_col))
names(dat_path) <- paste0("V",min(dat_coords$x):max(dat_coords$x))

for(l in unique(dat_coords$line)){
  coords <- dat_coords %>% 
    filter(line == l)
  
  for(i in 1:(nrow(coords) - 1)){
    # Number of moves to go between row i and row (i+1) in coords
    m <- find_moves(coords[i,"x"], coords[i+1,"x"], coords[i,"y"], coords[i+1,"y"])
    
    x1 <- coords[i,"x"]
    y1 <- coords[i,"y"]
    dat_path[y1,paste0("V",x1)] <- "#"
    
    for(j in 1:m){
      
      nc <- new_coords(x1, coords[i+1,"x"], y1, coords[i+1,"y"])
      dat_path[nc$new_y, paste0("V",nc$new_x)] <- "#"
      
      x1 <- nc$new_x
      y1 <- nc$new_y
      
    }
  }
}

# Add floor
dat_path <- rbind(dat_path
                  , rep("#",ncol(dat_path)))

### Drop in sand!
sand_dir <- function(x,y,lb,rb){ #x and y are current location of sand
  if(dat_path[y+1,paste0("V",x)] == "."){
    # Move down into that space
    x <- x
    y <- y + 1
    at_rest <- FALSE
    done <- FALSE
    add_left <- FALSE
    add_right <- FALSE
    left <- lb
    right <- rb
    
  }else if(x - 1 < lb){
    x <- 500
    y <- 1
    at_rest <- FALSE
    done <- FALSE
    add_left <- TRUE
    add_right <- FALSE
    left <- lb - 1
    right <- rb
    
  }else if(x + 1 > rb){
    x <- 500
    y <- 1
    at_rest <- FALSE
    done <- FALSE
    add_left <- FALSE
    add_right <- TRUE
    left <- lb
    right <- rb + 1
    
  }else if((dat_path[y+1,paste0("V",x)] == "#" | dat_path[y+1,paste0("V",x)] == "o") & dat_path[y+1,paste0("V",x-1)] == "."){
    # Check down and left
    x <- x - 1
    y <- y + 1
    at_rest <- FALSE
    done <- FALSE
    add_left <- FALSE
    add_right <- FALSE
    left <- lb
    right <- rb
    
  }else if((dat_path[y+1,paste0("V",x)] == "#" | dat_path[y+1,paste0("V",x)] == "o") & dat_path[y+1,paste0("V",x+1)] == "."){
    # Check down and right
    x <- x + 1
    y <- y + 1
    at_rest <- FALSE
    done <- FALSE
    add_left <- FALSE
    add_right <- FALSE
    left <- lb
    right <- rb
    
  # }else if((dat_path[y+1,paste0("V",x)] == "#" | dat_path[y+1,paste0("V",x)] == "o") &
  #          (dat_path[y+1,paste0("V",x+1)] == "#" | dat_path[y+1,paste0("V",x+1)] == "o") &
  #          (dat_path[y+1,paste0("V",x-1)] == "#" | dat_path[y+1,paste0("V",x-1)] == "o") &
  #          y == 1 & x == 500
  # ){
  }else if(dat_path[y,paste0("V",x)] == "o"){
    x <- x
    y <- y
    at_rest <- FALSE
    done <- TRUE
    add_left <- FALSE
    add_right <- FALSE
    left <- lb
    right <- rb
    
  }else{
    x <- x
    y <- y
    at_rest <- TRUE
    done <- FALSE
    add_left <- FALSE
    add_right <- FALSE
    left <- lb
    right <- rb
  }
  return(data.frame(x,y,at_rest,done,add_left,add_right,left,right))
}


### Drop in sand!
curr_sand_x <- 500
curr_sand_y <- 1
left_boundary <- min(dat_coords$x)
right_boundary <- max(dat_coords$x)
stop <- FALSE

i <- 0

repeat{
  
  sd <- sand_dir(curr_sand_x, curr_sand_y, left_boundary, right_boundary)
  if(sd$done == TRUE){
    stop <- TRUE
    break
    
  }else if(sd$add_left == TRUE){
    
    nm <- names(dat_path)
    dat_path <- cbind(c(rep(".",nrow(dat_path)-1),"#"), dat_path)
    curr_sand_x <- sd$x
    curr_sand_y <- sd$y
    left_boundary <- sd$left
    names(dat_path) <- c(paste0("V", left_boundary), nm)
    
  }else if(sd$add_right == TRUE){
    
    nm <- names(dat_path)
    dat_path <- cbind(dat_path, c(rep(".",nrow(dat_path)-1),"#"))
    curr_sand_x <- sd$x
    curr_sand_y <- sd$y
    right_boundary <- sd$right
    names(dat_path) <- c(nm, paste0("V", right_boundary))
    
  }else if(sd$at_rest == TRUE){
    
    dat_path[sd$y, paste0("V",sd$x)] <- "o"
    curr_sand_x <- 500
    curr_sand_y <- 1
    
  }else if(sd$at_rest == FALSE){
    
    curr_sand_x <- sd$x
    curr_sand_y <- sd$y
    
  }else{

    print("idk")
  }
  
  i <- i + 1
  if(i %% 10000 == 0){
    print(i)
    print(paste0("curr_sand_x: ", curr_sand_x))
    print(paste0("curr_sand_y: ", curr_sand_y))
    print("")
  }else{
    
  }
  

  if(stop){break}
}

sum(str_count(unlist(dat_path),"o"))
# 1199 - Puzzle 1 answer