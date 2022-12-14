wd <- getwd()
setwd(paste0(wd,"/Day 7"))

library(dplyr)
library(stringr)

# Read in data
dat <- read.table("input.txt"
                  , header = FALSE
                  , sep = ""
                  , dec = "."
                  , fill = TRUE)

##############################################################################
# Puzzle 1
##############################################################################

dat_tab <- data.frame(matrix(NA, nrow=0, ncol=4))
names(dat_tab) <- c("file_path","file_name","size","type")

file_path <- NULL

for(i in 1:nrow(dat)){
  
  # If changing directories
  if(dat[i,"V2"] == "cd"){
    if(dat[i,"V3"] == ".."){
      file_path <- file_path[1:(length(file_path) - 1)]
      
    }else{
      file_path <- c(file_path, paste0("/", dat[i,"V3"]))
      
    }
  }else if(dat[i,"V2"] == "ls"){
    # Do nothing?
  }else{
    dat_tab <- rbind(dat_tab
                     , data.frame("file_path" = paste0(file_path, collapse = ' ')
                         , "file_name" = dat[i,"V2"]
                         , "size" = ifelse(dat[i,"V1"] == "dir",0,dat[i,"V1"])
                         , "type" = ifelse(dat[i,"V1"] == "dir","dir","file")
                     )
    )
    names(dat_tab) <- c("file_path","file_name","size","type")
  }
}

dat_agg <- dat_tab %>%
  mutate(size = as.numeric(as.character(size))) %>%
  group_by(file_path) %>%
  summarize(size = sum(size))

dat_agg$num_folder <- str_count(dat_agg$file_path," ") + 1
depth <- max(dat_agg$num_folder)

# Get all dirs of depth x
# then of x-1
# then of x-2 ...

dat_agg$file_path <- paste0(dat_agg$file_path, " ")
dat_agg$f <- NA

dat_append <- data.frame(matrix(NA,0,2))
names(dat_append) <- c("f","s")

for(d in depth:1){
  
  for(i in 1:nrow(dat_agg)){
    dat_agg$f[i] <- ifelse(dat_agg$num_folder[i] < d, NA,
                           substr(dat_agg$file_path[i], 1, str_locate_all(dat_agg$file_path[i]," ")[[1]][d,1]))
  }
  
  df <- dat_agg %>% 
    filter(!is.na(f)) %>% 
    group_by(f) %>% 
    summarize(s = sum(size))
  
  dat_append <- rbind(dat_append, df)
  
}


dat_append %>% 
  filter(s <= 100000) %>% 
  summarize(sum(s))
# 1297159 -- Puzzle 1 answer

##############################################################################
# Puzzle 2
##############################################################################
total_space <- dat_append %>% 
  filter(f == "// ") %>% 
  select(s) %>% 
  unlist %>% 
  unname

unused_space <- 70000000 - total_space
space_to_delete <- 30000000 - unused_space

dat_append %>%
  filter(s >= space_to_delete) %>% 
  summarize(min(s))
# 3866390 - Puzzle 2 answer