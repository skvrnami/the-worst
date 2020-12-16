library(dplyr)
library(RSQLite)

STATION_ID <- "radiowave"

con <- dbConnect(SQLite(), "data_2020.sqlite")

convert_to_date <- function(x){
    as.Date(x, format = "%Y-%m-%dT%H:%M:%S+01:00")
}

playlist <- dbGetQuery(con, "select * from playlist") %>%
    mutate(ymd = convert_to_date(date), 
           week_no = lubridate::week(ymd), 
           last_week = week_no == max(week_no)) 

spotify_tracks <- dbGetQuery(con, 
                             "select 
                                interpret_id, 
                                track_id, 
                                spotify_url, 
                                track_name 
                             from spotify_tracks")

interprets <- dbGetQuery(con, "select * from interprets") %>%
    mutate(interpret = gsub("`", "'", interpret))

old_music <- playlist %>% 
    filter(!last_week)

new_music <- playlist %>% 
    filter(last_week) %>% 
    filter(station_id == STATION_ID) %>%
    anti_join(., old_music, by = c("interpret_id", "track_id")) %>%
    count(interpret_id, track_id, sort = TRUE)

nrow(new_music)

best_new_music <- new_music %>%
    left_join(., interprets, by = "interpret_id") %>% 
    left_join(., spotify_tracks, by = c("interpret_id", "track_id")) %>%
    filter(!is.na(spotify_url)) %>%
    mutate(song = paste0(interpret, ": ", track_name)) %>%
    select(song, spotify_url, n) %>%
    head(., 30)

MD_TITLE <- glue::glue("# New {STATION_ID} music")
MD_BODY <- purrr::map2_chr(best_new_music$song, best_new_music$spotify_url, 
                           function(song, url) {
                               glue::glue("- [{song}]({url})\n")
                           })

writeLines(c(MD_TITLE, "\n", MD_BODY), con = paste0("output/", STATION_ID, ".md"))
    