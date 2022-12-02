#######################################
# Day 2
#######################################

library(dplyr)

opponent_throw <- as.data.frame(
  rbind(
    c('A','rock')
    , c('B','paper')
    , c('C','scissors')
  )
)
names(opponent_throw) <- c('opponent_throw','opponent_throw_type')


player_throw <- as.data.frame(
  rbind(
    c('X','rock')
    , c('Y', 'paper')
    , c('Z', 'scissors')
  )
)
names(player_throw) <- c('player_throw','player_throw_type')


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


strategy_guide <- as.data.frame(
  rbind(
    c('A','Y')
    , c('B','X')
    , c('C','Z')
  )
)
names(strategy_guide) <- c('opponent_throw','player_throw')

# Example data
dat <- strategy_guide %>% 
  left_join(., player_throw, by = c('player_throw')) %>% 
  left_join(., opponent_throw, by = c('opponent_throw')) %>% 
  left_join(., outcome_logic, by = c('player_throw_type','opponent_throw_type'))%>% 
  left_join(., outcome, by = c('outcome')) %>% 
  left_join(., throw_score, by = c('player_throw_type'))

outcome_score <- dat$outcome_score %>% as.character %>% as.numeric %>% sum
type_score <- dat$type_score %>% as.character %>% as.numeric %>% sum
outcome_score + type_score
