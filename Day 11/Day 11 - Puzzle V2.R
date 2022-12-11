# wd <- getwd()
# setwd(paste0(wd,"/Day 11"))

library(dplyr)

# Read in data
dat <- read.table("input.txt"
                  , header = FALSE
                  , sep = ""
                  , numerals = "no.loss"
                  , fill = TRUE
                  , col.names = c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10")) # Feels like cheating

##############################################################################
# Puzzle 1
##############################################################################

dat_monkey <- data.frame(matrix(NA,0,7))
names(dat_monkey) <- c("monkey","old_stress","operation","value","div_by","true_monkey","false_monkey")

# Parse data
for(i in 1:nrow(dat)){
  if(dat[i,"V1"] == "Monkey"){
    
    monkey <- as.numeric(gsub(":", "", dat[i,"V2"]))
    
  }else if(dat[i,"V1"] == "Starting"){
    
    # Get items starting in col V3
    old_stress <- dat[i,3:ncol(dat)]
    old_stress <- gsub(",","", unname(unlist(old_stress[1,]))) %>% as.numeric
    old_stress <- old_stress[!is.na(old_stress)]
    
  }else if(dat[i,"V1"] == "Operation:"){
    
    operation <- dat[i,"V5"]
    value <- dat[i,"V6"]
    
  }else if(dat[i,"V1"] == "Test:"){
    
    div_by <- dat[i,"V4"] %>% as.numeric()
    
  }else if(dat[i,"V1"] == "If" & dat[i,"V2"] == "true:"){
    
    true_monkey <- dat[i,"V6"] %>% as.numeric()
    
  }
  else if(dat[i,"V1"] == "If" & dat[i,"V2"] == "false:"){
    
    false_monkey <- dat[i,"V6"] %>% as.numeric()
    
    # At this point, all rows for 1 monkey have been read in
    # Start putting pieces together
    for(m in 1:length(old_stress)){
      dat_monkey <- rbind(dat_monkey
                          , c(monkey
                              , old_stress[m]
                              , operation
                              , value
                              , div_by
                              , true_monkey
                              , false_monkey))
    }
  }else{
    # Do nothing -- shouldn't exist
    print("Check else clause")
  }
}

names(dat_monkey) <- c("monkey","old_stress","operation","value","div_by","true_monkey","false_monkey")

dat_functions <- dat_monkey %>% 
  select(-old_stress) %>% 
  unique

dat_stress <- dat_monkey %>% 
  select(monkey, old_stress)

monkies <- dat_functions["monkey"] %>% unlist %>% unname

# Calculates new stress level (which becomes the old stress level)
# and the monkey to send the item to
get_stress <- function(old_stress, operation, value, div_by, true_monkey, false_monkey, stress_reduction_factor){
  
  old_stress <- as.numeric(old_stress)
  div_by <- as.numeric(div_by)
  v <- ifelse(value == "old", old_stress, as.numeric(value))
  
  new_stress <- floor(ifelse(operation == "+", old_stress + v, old_stress * v) / stress_reduction_factor) # Round down
  monkey <- ifelse(new_stress %% div_by == 0, true_monkey, false_monkey)
  
  return(c(monkey, new_stress))
}

dat_puzzle_1 <- dat_stress
rounds <- 20

num_inspected <- data.frame(matrix(0,1,length(monkies)))
names(num_inspected) <- monkies

for(r in 1:rounds){
  for(m in monkies){
    d <- dat_puzzle_1 %>% filter(monkey == m)
    dj <- left_join(d, dat_functions, by = c("monkey"))
    
    # Remove records associated with monkey being looked at
    dat_puzzle_1 <- dat_puzzle_1 %>% filter(monkey != m)
    
    if(nrow(d) == 0){
      # Do nothing -- no values to move around
    }else{
      for(i in 1:nrow(dj)){
        s <- get_stress(dj[i,2], dj[i,3], dj[i,4], dj[i,5], dj[i,6], dj[i,7], 3)
        dat_puzzle_1 <- rbind(dat_puzzle_1, s)
        num_inspected[1,m] <- num_inspected[1,m] + 1
        
      }
    }
  }
}

sort(num_inspected %>% unlist, decreasing=TRUE)[1] * sort(num_inspected %>% unlist, decreasing=TRUE)[2]
# 50830  -- Puzzle 1 answer

##############################################################################
# Puzzle 2
##############################################################################

supermodulo <- prod(as.numeric(dat_functions[,"div_by"]))

get_stress_2 <- function(old_stress, operation, value, div_by, true_monkey, false_monkey){
  
  old_stress <- as.numeric(old_stress)
  div_by <- as.numeric(div_by)
  v <- ifelse(value == "old", old_stress, as.numeric(value))
  
  new_stress <- ifelse(operation == "+", old_stress + v, old_stress * v)
  new_stress <- floor(new_stress %% supermodulo)
  monkey <- ifelse(new_stress %% div_by == 0, true_monkey, false_monkey)

  return(c(monkey, new_stress))
}

dat_puzzle_2 <- dat_stress
rounds <- 10000

num_inspected <- data.frame(matrix(0,1,length(monkies)))
names(num_inspected) <- monkies

for(r in 1:rounds){
  for(m in monkies){
    d <- dat_puzzle_2 %>% filter(monkey == m)
    dj <- left_join(d, dat_functions, by = c("monkey"))
    
    # Remove records associated with monkey being looked at
    dat_puzzle_2 <- dat_puzzle_2 %>% filter(monkey != m)
    
    if(nrow(d) == 0){
      # Do nothing -- no values to move around
    }else{
      for(i in 1:nrow(dj)){
        s <- get_stress_2(dj[i,2], dj[i,3], dj[i,4], dj[i,5], dj[i,6], dj[i,7])
        dat_puzzle_2 <- rbind(dat_puzzle_2, s)
        names(dat_puzzle_2) <- c('monkey', 'old_stress')
        num_inspected[1,m] <- num_inspected[1,m] + 1
      }
    }
  }
}

prod(sort(num_inspected %>% unlist, decreasing=TRUE)[1:2])
# 14399640002 - Puzzle 2 answer