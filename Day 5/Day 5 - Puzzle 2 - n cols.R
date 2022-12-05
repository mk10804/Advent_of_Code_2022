# wd <- getwd()
# setwd(paste0(wd,"/Day 5"))

library(dplyr)
library(stringr)

# Read in data
dat <- read.table("input.txt"
                  , header = FALSE
                  , sep = ""
                  , blank.lines.skip = FALSE)

# Split data input stacks and rearrangement procedure
r <- NA
for (i in 1:nrow(dat)){
  if(dat$V1[i] == ""){
    r <- i
  }
}

dat_assign <- dat[(r+1):nrow(dat),1:6]

dat_assign$V2 <- as.numeric(as.character(dat_assign$V2))
dat_assign$V4 <- as.numeric(as.character(dat_assign$V4))
dat_assign$V6 <- as.numeric(as.character(dat_assign$V6))

cols <- c(dat[r-1,]) %>% unlist %>% as.numeric %>% max(., na.rm=TRUE)


dat_stack_input <- read.fwf("C:/Users/marla/OneDrive/Desktop/Advent of Code 2022/Code-Advent/Day 5/input.txt"
                            , widths = rep(4, cols)
                            , n = 8) # Hardcoded for now :(

dat_stack_OG <- dat_stack_input # Because read.fwf seems to be finnicky

for(c in 1:cols){
  for(r in 1:nrow(dat_stack_OG)){
    dat_stack_OG[r,paste0("V", c)] <- gsub(" ","",dat_stack_OG[r, paste0("V", c)])
  }
}

# Rearrange stacks
dat_stack <- dat_stack_OG #So I can retest easily :)

for(i in 1:nrow(dat_assign)){
  
  for(stack in 1:dat_assign$V2[i]){
    # Add a blank for each box to move
    dat_stack <- rbind(rep("",cols), dat_stack)
  }
  
  from_val <- rep("", stack)
  v <- 1
  
  for(stack in 1:dat_assign$V2[i]){
    # Grab first stack # of values in dat_stack
    r <- 1
    
    while("" %in% from_val){
      if(dat_stack[r, paste0("V",dat_assign$V4[i])] == ""){
        r <- r + 1
        
        } else {
          from_val[v] <- dat_stack[r, paste0("V",dat_assign$V4[i])]
          dat_stack[r, paste0("V",dat_assign$V4[i])] <- ""
          r <- r + 1
          v <- v + 1
          
      }
    }
  }
  #Put from_val at top of "to" stack
  dat_stack[1:stack, paste0("V",dat_assign$V6[i])] <- from_val

}

#Find first non-blank row in each column
for(i in 1:cols){
  vals <- rep(NA,cols)
}

for(i in 1:cols){
  r <- 1
  
  while(dat_stack[r, i] == ""){
    r <- r + 1
  }
  vals[i] <- dat_stack[r, i]
}
vals
# "[S]" "[T]" "[H]" "[G]" "[R]" "[Z]" "[Z]" "[F]" "[R]"
