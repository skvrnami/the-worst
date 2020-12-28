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

# find tracks on spotify
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

find_most_similar <- function(df, track){
    df %>%
        mutate(distance = stringdist::stringdist(tolower(name), tolower(track))) %>%
        filter(distance == min(distance)) %>%
        slice(1)
}

find_track_on_spotify <- function(interpret, track, track_id, interpret_id, 
                                  authorization){
    result <- search_spotify(paste0(interpret, ": ", track), 
                             type = "track", 
                             market = "cz", 
                             authorization = authorization)
    if(nrow(result) > 0){
        find_most_similar(result, track) %>%
            mutate(spotify_artist_id = paste0(artists[[1]]$id, collapse = ";")) %>%
            select(spotify_artist_id,
                   duration_ms,
                   explicit,
                   spotify_track_id = id,
                   track_name = name, 
                   popularity,
                   spotify_url = external_urls.spotify) %>%
            mutate(track_id = track_id, 
                   interpret_id = interpret_id)
            
    }else{
        data.frame(
            track_id = track_id, 
            interpret_id = interpret_id
        )
    }
}

remove_text_in_bracket <- function(x){
    stringr::str_remove_all(x, "\\s\\([^)]*\\)")
}

remove_text_after_feat <- function(x){
    stringr::str_remove(x, "\\(*f[ea]*t.\\s[A-Ža-ž_0-9&, ]+\\)*")
}


