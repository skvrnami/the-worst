library(dplyr)

source("src/funs.R")

START <- as.Date("2020-06-01")
END <- as.Date("2020-12-14")

DATES <- seq(from = START, to = END, by = 1)

pl <- get_playlist(END, "radiowave") 

summarise_playlist <- function(date, station_id = "radiowave"){
    song_count <- nrow(get_playlist(date, station_id))
    data.frame(date = date, 
               station_id = station_id,
               song_count = song_count)
        
}

possibly2 <- function(.f, otherwise=NULL) {
    function(...) {
        tryCatch({
            .f(...)  
        }, error = function(e) otherwise(...))
    }
}

STATIONS <- c("radiozurnal", "dvojka", "vltava",
              "plus", "radiowave", "d-dur", "jazz", 
              "radiojunior", "brno", "cb", "hradec", 
              "kv", "liberec", "olomouc", "ostrava", 
              "pardubice", "plzen", "regina", "strednicechy", 
              "sever", "vysocina", "zlin")

possibly_summarise_playlist <- possibly2(summarise_playlist, 
                                         otherwise = function(date, station_id) { 
                                             data.frame(date = date, 
                                                        station_id = station_id, 
                                                        song_count = 0)})
purrr::map_df(STATIONS, function(station) {
    purrr::map_df(DATES, function(x) {possibly_summarise_playlist(x, station)})
}) -> all_data

library(ggplot2)

ggplot(all_data %>% filter(station_id != "plus"), 
       aes(x = date, y = station_id, fill = song_count > 0)) + 
    geom_tile() + 
    theme_minimal()

ggsave("api_plot.png")
