library(dplyr)

playlist <- readRDS("output/playlist.RData")
interprets <- readRDS("output/interprets.RData")
tracks <- readRDS("output/tracks.RData")
spotify_tracks <- readRDS("output/spotify_tracks.RData") %>%
    select(-interpret_id)
wikipedia_interprets <- readRDS("output/wikipedia_interprets.RData") %>%
    rename(wiki_interpret = interpret)

left_join(playlist, interprets, by = "interpret_id") %>% 
    left_join(., tracks, by = "track_id") %>%
    left_join(., spotify_tracks, by = "track_id") %>% 
    left_join(., wikipedia_interprets, by = "interpret_id") %>%
    mutate(station_id = case_when(
        station_id == "radiozurnal" ~ "RZ", 
        station_id == "dvojka" ~ "DJ", 
        station_id == "jazz" ~ "JZ", 
        station_id == "radiowave" ~ "RW", 
        station_id == "radiojunior" ~ "RJ", 
        station_id %in% c("brno", "cb", "hradec", 
                          "liberec", "ostrava", "pardubice", 
                          "plzen", "regina", "sever", 
                          "strednicechy", "vysocina", "zlin") ~ "RG", 
        TRUE ~ NA_character_
    )) -> all_data

all_data %>%
    mutate(song = paste0(interpret, " - ", track)) -> sample_data

sample_data %>%
    select(station_id, song, spotify_url) %>%
    mutate(song = gsub("`", "'", song)) %>%
    mutate(song = gsub("´", "'", song)) %>%
    unique -> sample_data_min

spotify_interprets <- readRDS("output/spotify_interprets.RData")

spotify_interprets %>%
    mutate(genre_category = case_when(
        grepl("pop", genre, ignore.case = TRUE) ~ "pop", 
        grepl("rock|punk", genre, ignore.case = TRUE) ~ "rock",
        grepl("electro|drone", genre, ignore.case = TRUE) ~ "electronic", 
        grepl("hiphop|hip-hop|hip\\shop|rap", genre, ignore.case = TRUE) ~ "hip-hop",
        grepl("classic", genre, ignore.case = TRUE) ~ "classical",
        grepl("core", genre, ignore.case = TRUE) ~ "*core",
        grepl("jazz", genre, ignore.case = TRUE) ~ "jazz",
        TRUE ~ "other"
    )) -> genres

genres %>%
    select(spotify_id, genre_category) %>% 
    left_join(sample_data, ., by = c("spotify_artist_id"="spotify_id")) %>% 
    mutate(genre_category = ifelse(is.na(genre_category), "other", genre_category)) %>%
    select(interpret_id, station_id, song, genre_category, spotify_track_id) %>%
    mutate(station_id = ifelse(station_id %in% c("brno", "cb", "hradec", 
                                                 "liberec", "ostrava", "pardubice", 
                                                 "plzen", "regina", "sever", 
                                                 "strednicechy", "vysocina", "zlin"), 
                               "region", station_id)) %>%
    mutate(song = gsub("`", "'", song)) %>%
    mutate(song = gsub("´", "'", song)) %>%
    filter(!grepl("volebni kucharka", song)) %>%
    unique -> sample_data_min_genres

write.csv(sample_data_min_genres %>% select(-interpret_id), 
          file = "playlist-generator/sample-data.csv", 
          row.names = FALSE)

sample_data %>%
    mutate(active_to = ifelse(active_to == "NA", NA_character_, active_to), 
           active_from = ifelse(active_from == "NA", NA_character_, active_from)) %>%
    mutate(not_active = case_when(!is.na(active_to) & !is.na(active_from) ~ 0,
                            !is.na(active_from) ~ 1,
                            TRUE ~ 2)) %>%
    select(interpret_id, not_active) %>% 
    unique -> not_active

sample_data_min_genres_dead <- sample_data_min_genres %>%
    left_join(., not_active, by = "interpret_id") %>% 
    select(-interpret_id)

write.csv(sample_data_min_genres_dead, file = "playlist-generator/sample-data-dead.csv", 
          row.names = FALSE)

