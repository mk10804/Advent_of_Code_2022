# wd <- getwd()
# setwd(paste0(wd,"/Day 11"))

library(dplyr)
# library(reshape2)

# Read in data
dat <- read.table("input.txt"
                  , header = FALSE
                  , sep = ""
                  , numerals = "no.loss"
                  , fill = TRUE)

##############################################################################
# Puzzle 1
##############################################################################

num_monkey <- dat %>% filter(V1 == "Monkey") %>% nrow

dat_monkey <- data.frame(matrix(NA,0,7))
names(dat_monkey) <- c("monkey","old_stress","operation","value","div_by","true_monkey","false_monkey")

# Parse data

# monkey <- NA
# item_num <- 1
# old_stress <- NA

rounds <- 20

for(r in 1:rounds){
  if(r == 1){
    # Don't select an individual monkey, just set up data
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
        # value <- dat[i,"V6"] %>% as.numeric()
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
          # dat_monkey <- rbind(dat_monkey
          #                     , c(monkey
          #                         , old_stress[m]
          #                         , operation
          #                         , value
          #                         , div_by
          #                         , true_monkey
          #                         , false_monkey))
          
          v <- ifelse(value == "old", old_stress[m], as.numeric(value))
          new_stress <- ifelse(operation == "+", old_stress[m] + v, old_stress[m] * v)
          new_stress <- round(new_stress / 3, 0)
          
          new_stress_div <- new_stress / div_by
          divisible <- ifelse(new_stress %% div_by == 0, true_monkey, false_monkey)
          

        }
      }else{
        # Do nothing -- shouldn't exist
        print("Check else clause")
      }
    }
    
    
    
    
    
  }else{
    
  }
  
}
names(dat_monkey) <- c("monkey","old_stress","operation","value","div_by","true_monkey","false_monkey")
dat_monkey


