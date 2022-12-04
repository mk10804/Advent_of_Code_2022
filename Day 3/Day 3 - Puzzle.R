wd <- getwd()
setwd(paste0(wd,"/Day 3"))

library(dplyr)
library(stringr)
library(vecsets)
library(reshape2)

# Read in data
dat <- read.table("input.txt"
                  , header = FALSE
                  , sep = ""
                  , dec = "."
                  , blank.lines.skip = FALSE)
names(dat) <- c('contents')

##############################################################################
# Puzzle 1
##############################################################################

# Split each string into 2 -- each rucksack into the 2 compartments
dat$Compartment1 <- substr(dat$contents, 1, str_length(dat$contents) / 2)
dat$Compartment2 <- substr(dat$contents, str_length(dat$contents) / 2 + 1, str_length(dat$contents))



# Find shared character of each compartment in the rucksack
dat$shared_char <- NA
for(i in 1:nrow(dat)){
  dat$shared_char[i] <- vintersect(strsplit(dat$Compartment1, "")[[i]], strsplit(dat$Compartment2, "")[[i]], multiple = FALSE)
}

# Find score
score <- as.data.frame(
  cbind(
    c(letters, toupper(letters))
    , 1:length(c(letters, toupper(letters)))
  )
)
names(score) <- c("shared_char","score")

dat %>% 
  left_join(., score, by = c("shared_char")) %>% 
  summarize(answer = sum(as.numeric(as.character(score))))
# 7863 -- Puzzle 1 answer

##############################################################################
# Puzzle 2
##############################################################################

# Rearrange data a bit ...
dat$group <- sort(rep(1:(nrow(dat)/3), 3))
dat$rucksack_num <- paste0("rucksack_", rep(1:3, nrow(dat)/3))

dat_trans <- dcast(dat, group ~ rucksack_num, value.var = 'contents')

# Find shared character
dat_trans$shared_char_12 <- NA
dat_trans$shared_char_13 <- NA
dat_trans$shared_char <- NA

# Between 1 and 2...
for(i in 1:nrow(dat_trans)){
  dat_trans$shared_char_12[i] <- paste0(vintersect(strsplit(dat_trans$rucksack_1, "")[[i]]
                                                   , strsplit(dat_trans$rucksack_2, "")[[i]]
                                                   , multiple = TRUE)
                                        , collapse = '')
}

# Between 1 and 3...
for(i in 1:nrow(dat_trans)){
  dat_trans$shared_char_13[i] <- paste0(vintersect(strsplit(dat_trans$rucksack_1, "")[[i]]
                                                   , strsplit(dat_trans$rucksack_3, "")[[i]]
                                                   , multiple = TRUE)
                                        , collapse = '')
}

# Final shared char
for(i in 1:nrow(dat_trans)){
  dat_trans$shared_char[i] <- paste0(vintersect(strsplit(dat_trans$shared_char_12, "")[[i]]
                                                , strsplit(dat_trans$shared_char_13, "")[[i]]
                                                , multiple = FALSE)
                                        , collapse = '')
}

dat_trans %>% 
  left_join(., score, by = c("shared_char")) %>% 
  summarize(answer = sum(as.numeric(as.character(score))))
# Puzzle 2 answer: 2488