# -- Script for loading in Data

#'  For a single match, load all its PBP and ball trajectory data.
#'
#' This function loads and aggregates multiple datafiles as a dataframe.

#'
#' @param match_id str identifying match of interest.
#' @param data_path str for saved data URL.
#' @return A Dataframe for all recorded events corresponding to match ID.
#' @export
#' @examples
#' Eg: Load 2019 Roland Garros match between Thiem and Nadal
#' load_complete_match_data(match_id = 'atp_roland_garros_2019_SM001')
load_complete_match_data <- function(match_id,
                                     data_path = '/Users/petertea/Documents/courtvision_data/data/'){

  # -- Load Player ID data
  player_df <- read.csv(paste0(data_path, '/misc/player_ids.csv'))

  # -- Load all matches available data
  available_matches <- read.csv(paste0(data_path,'/misc/matches_available.csv'))

  # -- Check if match ID is valid
  if (! (match_id %in% available_matches$match_id)){
    stop("Match ID not found. Please input a valid ID from 'matches_available.csv'")
  }

  # -- Get filenames
  pbp_filename <- paste0(data_path, '/play_by_play/', match_id, '_pbp.csv')
  track_filename <- paste0(data_path,'/ball_trajectory/', match_id, '_ball_trajectory.csv')

  # -- load files
  pbp_df <- read.csv(pbp_filename)
  track_df <- read.csv(track_filename)

  pbp_df$server_id <- as.character(pbp_df$server_id)
  pbp_df$returner_id <- as.character(pbp_df$returner_id)

  # -- combine files
  complete_data <-
    track_df %>%
    dplyr::select(-c(set_num, game_num, point_num, serve_num )) %>%
    dplyr::left_join(pbp_df,
              by = c('point_ID' = 'point_ID')) %>%
    # -- add player names
    dplyr::left_join(player_df %>%
                       dplyr::select(name, id),
              by = c('server_id' = 'id')) %>%
    dplyr::rename(server_name = name) %>%
    dplyr::left_join(player_df %>%
                       dplyr::select(name, id),
              by = c('returner_id' = 'id')) %>%
    dplyr::rename(returner_name = name)

  return(complete_data)
}

### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ###
### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ###
### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ###
### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ###
#'  For a given player name, load all its PBP and ball trajectory data.
#'
#' This function collects all PBP and ball-trajectory data that a given
#' player competed.

#'
#' @param player_name Name of player. Eg: N.DJOKOVIC.
#' @param data_path str for saved data URL.
#' @return A Dataframe for all recorded events corresponding to match ID.
#' @export
#' @examples
#' load_complete_player_data(player_name = 'F.AUGERALIASSIME')

load_complete_player_data <- function(player_name,
                                      data_path = "/Users/petertea/Documents/courtvision_data/data/"){

  # -- Load Player ID data
  player_df <- read.csv(paste0(data_path,
                               'misc/player_ids.csv'))

  # -- Load all matches available data
  available_matches <- read.csv(paste0(data_path,
                                       'misc/matches_available.csv'))

  # -- Check if player_name is valid
  if (! (player_name %in% player_df$name)){
    stop("Player Name not found. Please input a valid name from 'player_ids.csv'")
  }

  # -- Pull PLAYER's IDs
  player_ids <-
    player_df %>%
    dplyr::filter(name == player_name) %>%
    dplyr::pull(id)

  # -- Get all Player's match IDs
  all_player_match_ids <-
    available_matches %>%
    dplyr::filter( (player1_id %in% player_ids) | (player2_id %in% player_ids) ) %>%
    dplyr::pull(match_id)


  # -- Collect all data for Player ----
  data_list = list()
  index = 1
  for(match_id in all_player_match_ids){

    match_df <- load_complete_match_data(match_id = match_id)

    match_df$match_id <- match_id
    data_list[[index]] <- match_df
    index = index + 1

  }

  complete_data <- do.call(rbind, data_list)

  return(complete_data)


}


#' Load all available matches
#'
#' This function loads a dataframe indicating which match_id's are valid for loading data.

#' @param data_path str for saved data URL.
#' @return A Dataframe of a catalogue of available matches (ATP & WTA).
#' @export

load_available_matches <- function(data_path = '/Users/petertea/Documents/courtvision_data/data/'){
  available_matches <- read.csv(paste0(data_path,'/misc/matches_available.csv'))
  return(available_matches)

}


#' Load all available players
#'
#' This function loads a dataframe indicating which players are valid for loading data.

#' @param data_path str for saved data URL.
#' @return A Dataframe of a catalogue of available players (ATP & WTA).
#' @export

load_available_players <- function(data_path = '/Users/petertea/Documents/courtvision_data/data/'){
  # -- Load Player ID data
  player_df <- read.csv(paste0(data_path,
                               'misc/player_ids.csv'))
  return(player_df)

}
#' @export
