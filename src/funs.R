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
