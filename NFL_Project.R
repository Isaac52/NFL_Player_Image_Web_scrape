library(tidyverse)
library(fs)
library(rvest)
library(furrr)
library(xopen)
library(jpeg) #to read and write the images
library(here) #to save the files to the right location - this only works if you're working with a R project
library(webp)

# loading in saved globe environment
load('nfl_images_web_scrape.RData')
file_path <- 'NFL_DATA/'

# reading in all file names
list_of_file_paths <- dir_ls(regexp = "\\.csv$")

# reading in games, players, and plays data
games   <- read_csv(list_of_file_paths[1])
players <- read_csv(list_of_file_paths[2])
plays   <- read_csv(list_of_file_paths[3])



# mapping over the file names with the read_csv function. map_dfr combines  list elements rowwise
whole_season <- list_of_file_paths[4:length(list_of_file_paths)] %>%
    map_dfr(read_csv,.id = 'weeks') %>%
    mutate(weeks = weeks %>% str_replace('.csv', '') %>% str_replace('week',''))




# Giving each tibble a unique identifier
uniqueIdentifier <- function(list){

    data.table::rbindlist(list,idcol = 'weeks')
}

whole_season_id <- uniqueIdentifier(whole_season) %>% tibble()
#############################################################################################################################


# Getting player names and team names for 2018 season
#############################################################################################################################
a <- games %>%
    select(gameId,homeTeamAbbr,visitorTeamAbbr,week) %>%
    mutate(
          homeTeam = "home"
        , awayTeam = 'away')

home_a <- a %>%
    select(gameId,homeTeamAbbr,homeTeam) %>%
    rename(teams = homeTeamAbbr,
           homeOrAway = homeTeam)
away_a <- a %>%
    select(gameId,visitorTeamAbbr,awayTeam) %>%
    rename(teams = visitorTeamAbbr,
           homeOrAway = awayTeam)

data <- home_a %>%
    bind_rows(away_a)

teams <- whole_season %>%
    select(nflId, position,displayName,gameId,playId,team)

playerTeams <- teams %>%
    inner_join(data, by=c('gameId' = 'gameId', 'team' = 'homeOrAway')) %>%
    inner_join(players, by = 'nflId') %>%
    distinct(nflId,.keep_all = TRUE) %>%
    select(nflId,gameId,playId,position.x,displayName.x,teams) %>%
    rename(
          position    = position.x
        , displayName = displayName.x
    )

playerTeams
##########################################################################################################################################



# Web Scraping player images form Google images
##############################################################################################################################################

# creating a tibble with unique Google link for each player
players_tbl <- playerTeams %>%
    separate(displayName,into = c('firstName','lastName'),sep = ' ',remove = FALSE) %>%

    mutate(
        url = str_glue('https://www.google.com/search?q={firstName}+{lastName}+{teams}&source=lnms&tbm=isch&sa=X&ved=2ahUKEwiku67O5MbtAhXPjFkKHSK6A60Q_AUoAnoECBIQBA&biw=1920&bih=937')
    )

players_tbl


# function for getting image link
player_image_id <- function(url){
    read_html(url) %>%
    html_nodes('img') %>%
    html_attr('src') %>%
    tibble() %>%
    slice(2) %>%
    rename('image_id' = '.')

}


# mapping over the google url column with the player_image_id function
plan(strategy = 'multisession')
player_image_tbl <- players_tbl %>%
    mutate(image_id = future_map(url,player_image_id))


google_player_images <- player_image_tbl %>%
    unnest(image_id)
############################################################################################################


# Saving images to computer
##############################################################################################################
saveImage <- function(data){

    first <- data %>%
        select(firstName,lastName) %>%
        mutate(fileLocation = str_glue('player_images_jpeg/{firstName}_{lastName}.jpeg')) %>%
        mutate(fileLocation = fileLocation %>% as.character()) %>%
        select(fileLocation) %>%
        pull()

    link <- data %>%
        unnest(image_id) %>%
        select(image_id) %>%
        pull()




    for(i in 1:length(first)){
         download.file(link[[i]],first[[i]],mode = 'wb')
    }

}



saveImage(player_image_tbl)

#########################################################################################################################################
