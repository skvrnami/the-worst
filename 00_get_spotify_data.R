library(dplyr)
library(spotifyr)

source("src/funs.R")

args <- commandArgs(trailingOnly = TRUE)
SPOTIFY_CLIENT_ID <- args[1]
SPOTIFY_CLIENT_SECRET <- args[2]

access_token <- get_spotify_access_token(SPOTIFY_CLIENT_ID, SPOTIFY_CLIENT_SECRET)

interprets <- readRDS("output/interprets.RData")
playlist <- readRDS("output/playlist.RData") %>%
    select(interpret_id, track_id) %>%
    unique
tracks <- readRDS("output/tracks.RData") %>%
    left_join(., playlist, by = "track_id") %>%
    left_join(., interprets, by = "interpret_id")

spotify_tracks <- readRDS("output/spotify_tracks.RData")

new_tracks <- tracks %>% 
    filter(!track_id %in% spotify_tracks$track_id) %>%
    unique %>%
    group_by(interpret_id, track_id) %>%
    mutate(row_no = row_number()) %>%
    ungroup %>% 
    filter(row_no == 1) %>%
    select(-row_no) %>%
    mutate(track = remove_text_in_bracket(track)) %>%
    mutate(track = remove_text_after_feat(track))

purrr::map_df(seq(nrow(new_tracks)), function(x) {
    Sys.sleep(0.1)
    cat(x, ")", new_tracks$interpret[x], ":", new_tracks$track[x], "\n")
    find_track_on_spotify(new_tracks$interpret[x], new_tracks$track[x], 
                          new_tracks$track_id[x], new_tracks$interpret_id[x], 
                          authorization = access_token)
}) -> new_spotify_tracks    
    
updated_spotify_tracks <- bind_rows(spotify_tracks, new_spotify_tracks)
saveRDS(updated_spotify_tracks, "output/spotify_tracks.RData")

# find interprets on spotify
spotify_interpret_ids <- unlist(strsplit(updated_spotify_tracks$spotify_artist_id, ";"))

spotify_interprets <- readRDS("output/spotify_interprets.RData")

new_spotify_interpret_ids <- na.omit(spotify_interpret_ids[!spotify_interpret_ids %in%
                                                       spotify_interprets$spotify_id]) %>%
    unique

batches_no <- ceiling(length(new_spotify_interpret_ids) / 50)
batches <- purrr::map(1:batches_no, function(x) 
    new_spotify_interpret_ids[((x - 1) * 50 + 1):(x * 50)])

new_spotify_interprets <- purrr::map_df(batches, function(x) get_artists(na.omit(x), 
                                                                         authorization = access_token))
updated_spotify_interprets <- new_spotify_interprets %>%
    mutate(genre = purrr::map_chr(genres, ~paste0(.x, collapse = ";"))) %>%
    select(spotify_id = id, 
           interpret = name, 
           genre, 
           popularity, 
           spotify_url = external_urls.spotify, 
           spotify_followers = followers.total) %>%
    bind_rows(spotify_interprets, .) 

saveRDS(updated_spotify_interprets, "output/spotify_interprets.RData")
