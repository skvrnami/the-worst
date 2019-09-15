library(dplyr)
library(RSQLite)

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

CREATE_PLAYLIST_SQL <- "create table if not exists playlist (station_id text, date text, id int, interpret_id int, track_id int)"
CREATE_INTERPRET_SQL <- "create table if not exists interprets (interpret_id int, interpret text)"
CREATE_TRACK_SQL <- "create table if not exists tracks (track_id int, track text)"

con <- dbConnect(SQLite(), "data.sqlite")
dbClearResult(dbSendStatement(con, CREATE_INTERPRET_SQL))
dbClearResult(dbSendStatement(con, CREATE_TRACK_SQL))
dbClearResult(dbSendStatement(con, CREATE_PLAYLIST_SQL))


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
        filter(!interpret_id %in% EXISTING_INTERPRETS$interpret_id)
    
    EXISTING_TRACKS <- dbGetQuery(con, "select * from tracks")
    
    NEW_TRACKS <- data %>% select(track_id, track) %>%
        filter(!track_id %in% EXISTING_TRACKS$track_id)
    
    if(nrow(NEW_INTERPRETS) > 0){
        insert_interprets_to_db(connection, NEW_INTERPRETS)    
    }
    
    if(nrow(NEW_TRACKS) > 0){
        insert_tracks_to_db(connection, NEW_TRACKS)    
    }
    
    insert_playlist_to_db(connection, station, data)
}

DATES <- as.character(seq.Date(as.Date("2019-01-01"), 
                  as.Date("2019-09-01"), 
                  by = 1))

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

r <- dbSendStatement(con, "create table if not exists last_fm_interprets (interpret_id int, interpret text, global_listeners int, global_scrobbles int, tags text)")
dbClearResult(r)

library(lastfmR)
for(i in 1:nrow(interprets)){
    cat(interprets$interpret[i], "\n")
    last_fm_data <- get_artist_info(artist_vector = c(interprets$interpret[i]))
    if(nrow(last_fm_data) > 0 & !is.na(last_fm_data$global_listeners)){
        dbSendStatement(con, paste0("insert into last_fm_interprets values (", 
                                    interprets$interpret_id[i], ", '",
                                    gsub("'", "`", interprets$interpret[i]), "', ", 
                                    last_fm_data$global_listeners, ", ", 
                                    last_fm_data$global_scrobbles, ", '", 
                                    gsub("'", "", last_fm_data$artist_tags), "')"
        )) -> o
        dbClearResult(o)
    }
}


