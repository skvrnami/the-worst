library(dplyr)

args <- commandArgs(trailingOnly=TRUE)

START_DATE <- args[1] #"2019-09-02"
END_DATE <- args[2] #"2019-12-31"

if(length(args) == 0){
    START_DATE <- as.character(Sys.Date() - 7)
    END_DATE <- as.character(Sys.Date() - 1)
}

source("src/funs.R")

save_data <- function(station, data){
    EXISTING_INTERPRETS <- readRDS("output/interprets.RData")
    
    NEW_INTERPRETS <- data %>% select(interpret_id, interpret) %>%
        filter(!interpret_id %in% EXISTING_INTERPRETS$interpret_id) %>%
        unique
    
    EXISTING_TRACKS <- readRDS("output/tracks.RData")
    
    NEW_TRACKS <- data %>% select(track_id, track) %>%
        filter(!track_id %in% EXISTING_TRACKS$track_id) %>%
        unique
    
    if(nrow(NEW_INTERPRETS) > 0){
        interprets <- bind_rows(EXISTING_INTERPRETS, NEW_INTERPRETS)
        saveRDS(interprets, "output/interprets.RData")
    }
    
    if(nrow(NEW_TRACKS) > 0){
        tracks <- bind_rows(EXISTING_TRACKS, NEW_TRACKS)
        saveRDS(tracks, "output/tracks.RData")
    }
    
    data <- data %>%
        mutate(station_id = station) %>%
        rename(date = since) %>%
        select(-c(files, interpret, track))
    playlist <- readRDS("output/playlist.RData") %>%
        bind_rows(., data)
    saveRDS(playlist, "output/playlist.RData")
}

DATES <- seq(as.Date(START_DATE), as.Date(END_DATE), by = 1) %>%
    as.character()

get_station_data <- function(connection, dates, station){
    for(i in dates){
        cat(i, "\n")
        out <- get_playlist(i, station)
        if(length(out) > 0){
            save_data(station, out)
        }
    }
}

get_station_data(con, DATES, "radiozurnal")
get_station_data(con, DATES, "dvojka")
get_station_data(con, DATES, "jazz")
get_station_data(con, DATES, "radiowave")
get_station_data(con, DATES, "vltava")
get_station_data(con, DATES, "radiojunior")
get_station_data(con, DATES, "brno")
get_station_data(con, DATES, "cb")
get_station_data(con, DATES, "hradec")
get_station_data(con, DATES, "liberec")
get_station_data(con, DATES, "ostrava")
get_station_data(con, DATES, "pardubice")
get_station_data(con, DATES, "plzen")
get_station_data(con, DATES, "regina")
get_station_data(con, DATES, "strednicechy")
get_station_data(con, DATES, "sever")
get_station_data(con, DATES, "vysocina")
