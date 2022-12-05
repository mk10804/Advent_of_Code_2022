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

dat_assign <- dat[(r+1):nrow(dat),]

dat_assign$parts <- rep(c('part_1', 'part_2'), nrow(dat_assign)/2)

dat_assign_1 <- dat_assign %>% 
  filter(parts == 'part_1') %>% 
  select(-parts)

dat_assign_2 <- dat_assign %>% 
  filter(parts == 'part_2') %>% 
  select(-parts)
names(dat_assign_2) <- c('V4','V5','V6')

dat_assign <- cbind(dat_assign_1, dat_assign_2)

dat_assign$V2 <- as.numeric(as.character(dat_assign$V2))
dat_assign$V4 <- as.numeric(as.character(dat_assign$V4))
dat_assign$V6 <- as.numeric(as.character(dat_assign$V6))


dat_stack_OG <- read.fwf("input.txt"
                         , widths = c(4,4,4)
                         , n = r - 2)

dat_stack_OG$V1 <- gsub(" ","",dat_stack_OG$V1)
dat_stack_OG$V2 <- gsub(" ","",dat_stack_OG$V2)
dat_stack_OG$V3 <- gsub(" ","",dat_stack_OG$V3)

# Rearrange stacks
dat_stack <- dat_stack_OG #So I can retest easily :)

for(i in 1:nrow(dat_assign)){
  
  for(stack in 1:dat_assign$V2[i]){
    
    dat_stack <- rbind(c("","",""), dat_stack) # Add a blank row at the top
    
    # Grab first non-blank "from" value in dat_stack
    r <- 1
    from_val <- ""
    
    while(dat_stack[r, paste0("V",dat_assign$V4[i])] == ""){
      r <- r + 1
    }
    
    from_val <- dat_stack[r, paste0("V",dat_assign$V4[i])]
    dat_stack[r, paste0("V",dat_assign$V4[i])] <- ""
    
    #Put from_val at top of "to" stack
    dat_stack[1, paste0("V",dat_assign$V6[i])] <- from_val
    print(dat_stack)
  }
}

#Find first non-blank row in each column

V1 <- NA
V2 <- NA
V3 <- NA

r_1 <- 1
r_2 <- 1
r_3 <- 1

while(dat_stack$V1[r_1] == ""){
  r_1 <- r_1 + 1
}

while(dat_stack$V2[r_2] == ""){
  r_2 <- r_2 + 1
}

while(dat_stack$V3[r_3] == ""){
  r_3 <- r_3 + 1
}

paste0(dat_stack$V1[r_1], dat_stack$V2[r_2], dat_stack$V3[r_3])
# CMZ
