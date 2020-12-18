library(dplyr)
library(RSQLite)
library(spotifyr)

con <- dbConnect(SQLite(), "data_2020.sqlite")

r2 <- dbSendStatement(con, "create table if not exists spotify_tracks (
                      interpret_id int, 
                      track_id int, 
                      spotify_artist_id text,
                      duration_ms int,
                      explicit int,
                      spotify_track_id text,
                      track_name text,
                      popularity int,
                      spotify_url text,
                      found int
)")

dbClearResult(r2)

flatten_spotify_track <- function(result){
    data.frame(
        spotify_artist_id = paste0(result$artists[1][[1]]$id, collapse = ";"), 
        duration_ms = result$duration_ms[1], 
        explicit = result$explicit[1], 
        spotify_track_id = result$id[1], 
        track_name = result$name[1], 
        popularity = result$popularity[1], 
        spotify_url = result$external_urls.spotify[1]
    )
}

insert_spotify_track_into_db <- function(connection, interpret_id, track_id, result){
    if(nrow(result) > 0){
        tmp <- flatten_spotify_track(result) 
        r <- dbSendStatement(connection, 
                             paste0("insert into spotify_tracks values ", 
                                    paste0("(", interpret_id, ", ",
                                           track_id, ", '",
                                           tmp$spotify_artist_id, "', ",
                                           tmp$duration_ms, ", ", 
                                           as.numeric(tmp$explicit), ", '", 
                                           tmp$spotify_track_id, "', '", 
                                           gsub("'", "''", tmp$track_name), "', ", 
                                           tmp$popularity, ", '", 
                                           tmp$spotify_url, "', ", 
                                           1, ")")))
        dbClearResult(r)
    }else{
        r <- dbSendStatement(connection, paste0("insert into spotify_tracks (interpret_id, track_id, found) values (", 
                                                interpret_id, ", ", track_id, ", ", 0, ")"))
        dbClearResult(r)
    }
}

interprets <- dbGetQuery(con, "select * from interprets")

tracks <- dbGetQuery(con, "select distinct
                            t.interpret_id,
                            tracks.track_id,
                            tracks.track
                        from tracks
                        left join (select distinct interpret_id, track_id from playlist) t
                        on t.track_id=tracks.track_id") %>%
    left_join(., interprets, by = "interpret_id")

spotify_tracks <- dbGetQuery(con, "select * from spotify_tracks")

new_tracks <- tracks %>% 
    filter(!track_id %in% spotify_tracks$track_id) %>%
    unique

for(i in seq(nrow(new_tracks))){
    cat(i, "\n")
    cat(new_tracks$interpret[i], ":", new_tracks$track[i], "\n")
    result <- search_spotify(paste0(new_tracks$interpret[i], ": ", new_tracks$track[i]), 
                             type = "track")
    insert_spotify_track_into_db(con, new_tracks$interpret_id[i], 
                                 new_tracks$track_id[i], result)
    Sys.sleep(0.1)
}

# find interprets on spotify

r <- dbSendStatement(con, "create table if not exists spotify_interprets (
                                spotify_id text,
                                interpret text,
                                genre text,
                                popularity int,
                                spotify_url text,
                                spotify_followers int)")
dbClearResult(r)


spotify_tracks <- dbGetQuery(con, "select * from spotify_tracks")

spotify_interpret_ids <- unlist(strsplit(spotify_tracks$spotify_artist_id, ";"))

spotify_interprets <- dbGetQuery(con, "select * from spotify_interprets")

new_spotify_interpret_ids <- na.omit(spotify_interpret_ids[!spotify_interpret_ids %in%
                                                       spotify_interprets$spotify_id]) %>%
    unique


insert_spotify_interpret_into_db <- function(connection, result){
    r <- dbSendStatement(connection,
                             paste0("insert into spotify_interprets values ",
                                    paste0("('",
                                           result$id, "', '",
                                           gsub("'", "''", result$name), "', '",
                                           paste0(gsub("'", "''", result$genres), 
                                                  collapse = ";"), "', ",
                                           result$popularity, ", '",
                                           result$external_urls$spotify, "', ",
                                           result$followers$total, ")")))
    dbClearResult(r)
}

for(i in seq(length(new_spotify_interpret_ids))){
    cat(i, "\n")
    result <- get_artist(new_spotify_interpret_ids[i])
    insert_spotify_interpret_into_db(con, result)
    Sys.sleep(0.1)
}

dbDisconnect(con)

