setwd("C:/Users/marla/OneDrive/Desktop/Advent of Code 2022/Code-Advent/Day 2")
library(dplyr)

opponent_throw <- as.data.frame(
  rbind(
    c('A','rock')
    , c('B','paper')
    , c('C','scissors')
  )
)
names(opponent_throw) <- c('opponent_throw','opponent_throw_type')


required_end <- as.data.frame(
  rbind(
    c('X','loss')
    , c('Y', 'draw')
    , c('Z', 'win')
  )
)
names(required_end) <- c('required_end','outcome')
# --> SOLVE FOR PLAYER THROW


throw_score <- as.data.frame(
  rbind(
    c('rock',1)
    , c('paper',2)
    , c('scissors',3)
  )
)
names(throw_score) <- c("player_throw_type","type_score")


outcome_logic <- as.data.frame(
  rbind(
    c('rock','rock','draw')
    , c('paper','paper','draw')
    , c('scissors','scissors','draw')
    
    , c('rock','scissors','win')
    , c('paper','rock','win')
    , c('scissors','paper','win')
    
    , c('rock','paper','loss')
    , c('paper','scissors','loss')
    , c('scissors','rock','loss')
  )
)
names(outcome_logic) <- c('player_throw_type', 'opponent_throw_type', 'outcome')

outcome <- as.data.frame(
  rbind(
    c('loss', 0)
    , c('draw', 3)
    , c('win', 6)
  )
)
names(outcome) <- c('outcome','outcome_score')

strategy_guide <- read.table("input.txt"
                             , header = FALSE
                             , sep = ""
                             , dec = ".")
names(strategy_guide) <- c('opponent_throw','required_end')

# Apply logic
(dat <- strategy_guide %>% 
  left_join(., opponent_throw, by = c('opponent_throw')) %>% 
  left_join(., required_end, by = c('required_end')) %>% 
  left_join(., outcome_logic, by = c('opponent_throw_type','outcome')) %>% 
  left_join(., outcome, by = c('outcome')) %>% 
  left_join(., throw_score, by = c('player_throw_type')) %>% 
  mutate(outcome_score = as.numeric(as.character(outcome_score))
         , type_score = as.numeric(as.character(type_score))) %>%
  summarize(total_score = sum(outcome_score + type_score)))

# Puzzle 2 - 11998
