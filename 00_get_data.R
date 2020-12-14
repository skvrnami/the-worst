library(dplyr)
library(RSQLite)

args <- commandArgs(trailingOnly=TRUE)

START_DATE <- "2020-11-29" #args[1] #"2019-09-02"
END_DATE <- "2020-12-10" #args[2] #"2019-12-31"

CREATE_PLAYLIST_SQL <- "create table if not exists playlist (station_id text, date text, id int, interpret_id int, track_id int, unique (station_id, date, id, interpret_id, track_id))"
CREATE_INTERPRET_SQL <- "create table if not exists interprets (interpret_id int, interpret text, unique (interpret_id, interpret))"
CREATE_TRACK_SQL <- "create table if not exists tracks (track_id int, track text, unique (track_id, track))"

con <- dbConnect(SQLite(), "data_2020.sqlite")
dbClearResult(dbSendStatement(con, CREATE_INTERPRET_SQL))
dbClearResult(dbSendStatement(con, CREATE_TRACK_SQL))
dbClearResult(dbSendStatement(con, CREATE_PLAYLIST_SQL))


get_playlist <- function(date, station = "radiowave"){
    
    ALLOWED_STATIONS <- c(
        "radiozurnal", 
        "dvojka",
        "vltava",
        "plus",
        "radiowave", 
        "d-dur", 
        "jazz", 
        "radiojunior", 
        "archiv", # Rádio Retro
        "webik", # Rádio Junior písničky
        "cro7", # vysílání do zahraničí
        "brno",
        "cb",
        "hradec", 
        "kv",
        "liberec",
        "olomouc",
        "ostrava", 
        "pardubice", 
        "plzen",
        "regina", 
        "strednicechy", 
        "sever", 
        "vysocina", 
        "zlin"
    )
    
    DATE <- as.character(date)
    stopifnot(station %in% ALLOWED_STATIONS)
    
    BASE_URL <- "https://api.rozhlas.cz/data/v2/playlist/day/"
    YEAR <- substr(DATE, 1, 4)
    MONTH <- substr(DATE, 6, 7)
    DAY <- substr(DATE, 9, 10)
    
    out <- httr::GET(url = paste0(BASE_URL, YEAR, "/", MONTH, "/", 
                                  DAY, "/", station, ".json"))
    
    jsonlite::fromJSON(stringr::str_conv(out$content, "UTF-8"))$data
}

insert_interprets_to_db <- function(connection, interprets){
    r <- dbSendStatement(connection, 
                    paste0("insert into interprets values ", 
                           paste0("(", interprets$interpret_id, ", '", 
                                  gsub("'", "`", interprets$interpret), "')", collapse = ", ")))
    dbClearResult(r)
}

insert_tracks_to_db <- function(connection, tracks){
    r <- dbSendStatement(connection, 
                         paste0("insert into tracks values ", 
                                paste0("(", tracks$track_id, ", '", 
                                       gsub("'", "`", tracks$track), "')", 
                                       collapse = ", ")))
    dbClearResult(r)
}

insert_playlist_to_db <- function(connection, station, playlist){
    r <- dbSendStatement(connection, 
                         paste0("insert into playlist values ", 
                                paste0("('", station, "', '", 
                                       playlist$since, "', ", 
                                       playlist$id, ", ", 
                                       playlist$interpret_id, ", ", 
                                       playlist$track_id, ")",
                                       collapse = ", ")))
    dbClearResult(r)
}

send_data_to_db <- function(connection, station, data){
    
    EXISTING_INTERPRETS <- dbGetQuery(con, "select * from interprets")
    
    NEW_INTERPRETS <- data %>% select(interpret_id, interpret) %>%
        filter(!interpret_id %in% EXISTING_INTERPRETS$interpret_id) %>%
        unique
    
    EXISTING_TRACKS <- dbGetQuery(con, "select * from tracks")
    
    NEW_TRACKS <- data %>% select(track_id, track) %>%
        filter(!track_id %in% EXISTING_TRACKS$track_id) %>%
        unique
    
    if(nrow(NEW_INTERPRETS) > 0){
        insert_interprets_to_db(connection, NEW_INTERPRETS)    
    }
    
    if(nrow(NEW_TRACKS) > 0){
        insert_tracks_to_db(connection, NEW_TRACKS)    
    }
    
    insert_playlist_to_db(connection, station, data)
}

DATES <- seq(as.Date(START_DATE), as.Date(END_DATE), by = 1) %>%
    as.character()

get_station_data <- function(connection, dates, station){
    for(i in dates){
        cat(i, "\n")
        out <- get_playlist(i, station)
        send_data_to_db(connection, station, out)
    }
}

get_station_data(con, DATES, "radiozurnal")
get_station_data(con, DATES, "dvojka")
get_station_data(con, DATES, "jazz")
get_station_data(con, DATES, "radiowave")
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