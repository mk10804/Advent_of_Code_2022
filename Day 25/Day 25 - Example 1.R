#################################################################
# Day 25: 4-dimensional adventure
# https://adventofcode.com/2018/day/25
#################################################################

library(dplyr)
library(writexl)
library(data.table)

# Example 1
dat <- as.data.frame(rbind(
  c( 0,0,0,0)
  , c( 3,0,0,0)
  , c( 0,3,0,0)
  , c( 0,0,3,0)
  , c( 0,0,0,3)
  , c( 0,0,0,6)
  , c( 9,0,0,0)
  , c(12,0,0,0)
  
))

# Find Manhattan distance
for(i in 1:nrow(dat)){
  col_nm <- paste0('Dist_' , i)
  dat[[col_nm]] <- abs(dat$V1 - dat$V1[i]) + abs(dat$V2 - dat$V2[i]) + abs(dat$V3 - dat$V3[i]) + abs(dat$V4 - dat$V4[i])
}

# Confirm is Manhattan distance is within 3 to define constellation
for(i in 1:nrow(dat)){
  col_nm <- paste0('Dist_' , i)
  col_nm_chk <- paste0('Dist_' , i, '_Chk')
  
  dat[[col_nm_chk]] <- ifelse(dat[[col_nm]] <= 3, 1, 0)
}


# Get only the distance "check" columns and turn into tabular formuat
num_coords <- nrow(dat)
num_cols <- ncol(dat)

dat_chk <- dat[, (num_cols - num_coords + 1):num_cols]
dat_chk$coord1 <- 1:nrow(dat_chk)

dat_melt <- melt(dat_chk, id.vars = c('coord1'))
dat_melt$coord2 <- sort(rep(1:num_coords, num_coords))
dat_melt <- dat_melt[c('coord1','coord2','value')]


# Find group
start_grp <- 1

grps <- as.data.frame(cbind(1:num_coords, rep(NA, num_coords)))
names(grps) <- c('coord','group')

for(i in 1:num_coords){
  
  df <- dat_melt %>% 
    filter(coord1 == i, value != 0) %>% 
    select(coord2) %>% 
    unlist
  
  grps[df, 'group'] <- min(grps[df, 'group'], start_grp, na.rm = TRUE)
  
  start_grp <- start_grp + 1
  
}

length(unique(grps$group)) # 2