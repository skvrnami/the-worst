library(dplyr)
library(ggplot2)
library(RSQLite)

con <- dbConnect(SQLite(), "data.sqlite")

theme_cro <- function(base_size = 11, base_family = "", legend_pos = "none", ...){
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(
            axis.ticks = element_blank(),
            axis.text.y = element_text(margin = margin(t = 0, r = 0.2,
                                                       l = 0), hjust = 1),
            legend.position = legend_pos,
            legend.background = element_blank(),
            legend.key = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_line(colour = "grey20"),
            panel.spacing = unit(1, "lines"),
            strip.background = element_blank(),
            plot.background = element_blank(),
            complete = TRUE, ...)
}


playlist <- dbGetQuery(con, "select * from playlist")
interprets <- dbGetQuery(con, "select * from interprets")
tracks <- dbGetQuery(con, "select * from tracks")

left_join(playlist, interprets, by = "interpret_id") %>% 
    left_join(., tracks, by = "track_id") -> all_data

# TOP TRACKS BY STATION

all_data %>%
    filter(station_id != "zlin") %>%
    count(station_id, interpret, track) %>%
    arrange(desc(n)) %>% 
    group_by(station_id) %>%
    filter(row_number() <= 10) -> top_tracks

plot_top_tracks <- function(data, station, fill_colour){
    data %>%
        filter(station_id == station) %>%
        mutate(track_name = paste0(track, " (", interpret, ")")) %>%
        ggplot(., aes(x = reorder(track_name, n), y = n)) + 
        geom_bar(stat = "identity", fill = fill_colour) + 
        coord_flip() + 
        theme_cro() + 
        labs(x = "", y = "", title = "Nejčastěji hrané skladby")
}

plot_top_tracks(top_tracks, "radiozurnal", "#ED2E38")
plot_top_tracks(top_tracks, "dvojka", "#85248F")
plot_top_tracks(top_tracks, "radiowave", "#CDA200")
plot_top_tracks(top_tracks, "regina", "#00AB96")
plot_top_tracks(top_tracks, "sever", "#00AB96")

# TOP ARTISTS

all_data %>%
    filter(station_id != "zlin") %>%
    count(station_id, interpret) %>%
    arrange(desc(n)) %>% 
    group_by(station_id) %>%
    filter(row_number() <= 10) -> top_artists

plot_top_artists <- function(data, station, fill_colour){
    data %>%
        filter(station_id == station) %>%
        ggplot(., aes(x = reorder(interpret, n), y = n)) + 
        geom_bar(stat = "identity", fill = fill_colour) + 
        coord_flip() + 
        theme_cro() + 
        labs(x = "", y = "", title = "Nejčastěji hraní interpreti")
    
}

plot_top_artists(top_artists, "radiozurnal", "#ED2E38")
plot_top_artists(top_artists, "dvojka", "#85248F")
plot_top_artists(top_artists, "radiowave", "#CDA200")
plot_top_artists(top_artists, "regina", "#00AB96")
plot_top_artists(top_artists, "sever", "#00AB96")

all_data %>%
    mutate(song = paste0(interpret, " - ", track)) -> sample_data

sample_data %>%
    select(station_id, song) %>%
    mutate(station_id = ifelse(station_id %in% c("brno", "cb", "hradec", 
                                                 "liberec", "ostrava", "pardubice", 
                                                 "plzen", "regina", "sever", 
                                                 "strednicechy", "vysocina", "zlin"), 
                               "region", station_id)) %>%
    mutate(song = gsub("`", "'", song)) %>%
    mutate(song = gsub("´", "'", song)) %>%
    unique -> sample_data_min

write.csv(sample_data_min, file = "playlist-generator/sample-data-min2.csv", 
          row.names = FALSE)

last_fm_interprets <- dbGetQuery(con, "select * from last_fm_interprets")
last_fm_interprets %>%
    select(tags) %>%
    pull(tags) %>%
    strsplit(., split = ";") %>%
    unlist %>%
    stringr::str_trim(., "both") -> tags

last_fm_interprets %>%
    mutate(genre = case_when(
        grepl("pop", tags, ignore.case = TRUE) ~ "pop", 
        grepl("rock|punk", tags, ignore.case = TRUE) ~ "rock",
        grepl("electro|drone", tags, ignore.case = TRUE) ~ "electronic", 
        grepl("hiphop|hip-hop", tags, ignore.case = TRUE) ~ "hip-hop",
        grepl("classic", tags, ignore.case = TRUE) ~ "classical",
        grepl("core", tags, ignore.case = TRUE) ~ "*core",
        grepl("jazz", tags, ignore.case = TRUE) ~ "jazz",
        TRUE ~ "other"
    )) -> genres

genres %>%
    select(interpret_id, genre) %>% 
    left_join(sample_data, ., by = "interpret_id") %>%
    mutate(genre = ifelse(is.na(genre), "other", genre)) %>%
    select(station_id, song, genre) %>%
    mutate(station_id = ifelse(station_id %in% c("brno", "cb", "hradec", 
                                                 "liberec", "ostrava", "pardubice", 
                                                 "plzen", "regina", "sever", 
                                                 "strednicechy", "vysocina", "zlin"), 
                               "region", station_id)) %>%
    mutate(song = gsub("`", "'", song)) %>%
    mutate(song = gsub("´", "'", song)) %>%
    filter(!grepl("volebni kucharka", song)) %>%
    unique -> sample_data_min_genres

write.csv(sample_data_min_genres, file = "playlist-generator/sample-data-min3.csv", 
          row.names = FALSE)

age <- dbGetQuery(con, "select * from interprets_age")
age %>%
    mutate(death = ifelse(death == "NA", NA_character_, death), 
           birth = ifelse(birth == "NA", NA_character_, birth)) %>%
    mutate(dead = case_when(!is.na(death) & !is.na(birth) ~ 0, 
                            !is.na(birth) ~ 1, 
                            TRUE ~ 2)) %>%
    select(interpret_id, dead) -> dead

genres %>%
    select(interpret_id, genre) %>% 
    left_join(sample_data, ., by = "interpret_id") %>%
    left_join(., dead, by = "interpret_id") %>%
    mutate(dead = ifelse(is.na(dead), 2, dead)) %>%
    mutate(genre = ifelse(is.na(genre), "other", genre)) %>%
    select(station_id, song, genre, dead) %>%
    mutate(station_id = ifelse(station_id %in% c("brno", "cb", "hradec", 
                                                 "liberec", "ostrava", "pardubice", 
                                                 "plzen", "regina", "sever", 
                                                 "strednicechy", "vysocina", "zlin"), 
                               "region", station_id)) %>%
    mutate(song = gsub("`", "'", song)) %>%
    mutate(song = gsub("´", "'", song)) %>% 
    filter(!grepl("volebni kucharka", song)) %>%
    unique -> sample_data_min_genres_dead
    
write.csv(sample_data_min_genres_dead, file = "playlist-generator/sample-data-dead.csv", 
          row.names = FALSE)

