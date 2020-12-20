library(dplyr)
library(RSQLite)
library(spotifyr)

args <- commandArgs(trailingOnly=TRUE)

STATION_ID <- "vltava" #args[1]

con <- dbConnect(SQLite(), "data_2020.sqlite")

convert_to_date <- function(x){
    as.Date(x, format = "%Y-%m-%dT%H:%M:%S+01:00")
}

playlist <- dbGetQuery(con, "select * from playlist") %>%
    filter(station_id == STATION_ID) %>%
    mutate(ymd = convert_to_date(date), 
           week_no = lubridate::week(ymd), 
           last_week = week_no == max(week_no)) 

MAX_WEEK <- max(playlist$week_no)
MAX_YEAR <- playlist %>% 
    filter(last_week) %>%
    pull(date) %>%
    min %>%
    lubridate::year()

spotify_tracks <- dbGetQuery(con, 
                             "select 
                                interpret_id, 
                                track_id, 
                                spotify_track_id,
                                spotify_url, 
                                track_name 
                             from spotify_tracks")

interprets <- dbGetQuery(con, "select * from interprets") %>%
    mutate(interpret = gsub("`", "'", interpret))

tracks <- dbGetQuery(con, "select * from tracks") %>%
    mutate(track = gsub("`", "'", track))

dbDisconnect(con)

old_music <- playlist %>% 
    filter(!last_week)

new_music <- playlist %>% 
    filter(last_week) %>% 
    # filter(station_id == STATION_ID) %>%
    anti_join(., old_music, by = c("interpret_id", "track_id")) %>%
    count(interpret_id, track_id, sort = TRUE) %>%
    unique

# last_week <- playlist %>%
#     filter(last_week) %>%
#     filter(station_id == STATION_ID) %>%
#     count(interpret_id, track_id, sort = TRUE) %>% 
#     left_join(., interprets, by = "interpret_id") %>% 
#     left_join(., tracks, by = "track_id") %>% 
#     left_join(., spotify_tracks, by = c("interpret_id", "track_id")) %>% 
#     # filter(!is.na(spotify_url)) %>%
#     mutate(song = paste0(interpret, ": ", track)) %>%
#     select(song, spotify_url, spotify_track_id, n) %>%
#     head(., 30)

best_new_music <- new_music %>%
    left_join(., interprets, by = "interpret_id") %>% 
    left_join(., tracks, by = "track_id") %>% 
    left_join(., spotify_tracks, by = c("interpret_id", "track_id")) %>%
    # filter(!is.na(spotify_url)) %>%
    mutate(song = paste0(interpret, ": ", track)) %>%
    unique %>% 
    select(song, spotify_url, spotify_track_id, n)

write_md <- function(df, station_id, max_week){
    MD_TITLE <- glue::glue("# Discover {station_id} weekly")
    MD_BODY <- purrr::map2_chr(df$song, df$spotify_url, 
                               function(song, url) {
                                   if(is.na(url)){
                                       url <- glue::glue("https://www.youtube.com/results?search_query={song}")
                                   }
                                   glue::glue("- [{song}]({url})\n")
                               })
    
    writeLines(c(MD_TITLE, "\n", MD_BODY), con = glue::glue("output/md/{station_id}_{max_week}.md"))
    
}

best_new_music %>%
    head(30) %>%
    write_md(., STATION_ID, MAX_WEEK)

# order playlist using hierarchical clustering by audio features
best_new_music_on_spotify <- best_new_music %>%
    filter(!is.na(spotify_track_id)) %>% 
    head(30)

SPOTIFY_IDS <- best_new_music_on_spotify %>%
    pull(spotify_track_id)

best_nm_features <- get_track_audio_features(SPOTIFY_IDS)

hc <- best_nm_features %>%
    select(danceability, energy, key, loudness, 
           mode, speechiness, acousticness, instrumentalness, 
           liveness, valence, tempo) %>%
    dist %>%
    hclust

best_new_music_clustered <- best_new_music %>%
    filter(!is.na(spotify_track_id)) %>%
    slice(hc$order)

bnm_final <- best_new_music_clustered %>%
    mutate(spotify_uri = paste0("spotify:track:", spotify_track_id))

write.csv(bnm_final, glue::glue("output/csv/discover_{STATION_ID}_{MAX_YEAR}_(#{MAX_WEEK}).csv"),
          row.names = FALSE)

# TODO: create playlist on Spotify
# spotify_playlist <- create_playlist("majky_", "test", public = FALSE)
# add_tracks_to_playlist(spotify_playlist$id, 
#                        bnm_final$spotify_uri)

    