library(tidyverse)
library(lubridate)
library(reshape2)

#### Loading the main files and week 1 ####
games <- read.csv('data/games.csv')
players <- read.csv('data/players.csv')
plays <- read.csv('data/plays.csv')
week1 <- read.csv('data/week1.csv')

save(games,file = 'games.Rda')
save(players,file = 'players.Rda')
save(plays,file = 'plays.Rda')
save(week1,file = 'week1.Rda')

#### Loading the files ####


#### ####

