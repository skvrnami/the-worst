library(dplyr)
library(WikidataR)

interprets <- readRDS("output/interprets.RData")

find_artist <- function(x){
    x[unlist(lapply(x, function(x) grepl("band|singer|artist|composer|rapper", x$description))) & 
          !unlist(lapply(x, function(x) grepl("discography", x$description)))]
}

convert_date_or_na <- function(x){
    if(is.null(x)){
        NA
    }else{
        tmp <- as.Date(x, format = "+%Y-%m-%d")
        if(is.na(tmp)){
            year <- stringr::str_extract(x, "[0-9]{4}")
            return(as.Date(paste0(year, "-01-01"), format = "%Y-%m-%d"))
        }
        tmp
    }
}

get_year_of_death <- function(int){
    convert_date_or_na(int[[1]]$claims$P570$mainsnak$datavalue$value$time[1])
}

get_year_of_birth <- function(int){
    convert_date_or_na(int[[1]]$claims$P569$mainsnak$datavalue$value$time[1])
}

get_years_active_from <- function(int){
    convert_date_or_na(int[[1]]$claims$P2031$mainsnak$datavalue$value$time[1])
}

get_years_active_to <- function(int){
    convert_date_or_na(tail(int[[1]]$claims$P2032$mainsnak$datavalue$value$time, 1))
}

possibly_get_year_of_death <- purrr::possibly(get_year_of_death, NA_character_)
possibly_get_year_of_birth <- purrr::possibly(get_year_of_birth, NA_character_)

possibly_get_years_active_from <- purrr::possibly(get_years_active_from, NA_character_)
possibly_get_years_active_to <- purrr::possibly(get_years_active_to, NA_character_)

get_years_active <- function(interpret, interpret_id){
    item <- find_item(interpret, lang = "cs")
    
    if(length(item)){
        artists <- find_artist(item)
        if(length(artists)){
            wikipedia_id <- artists[[1]]$id
            int <- get_item(wikipedia_id)
            
            type <- artists[[1]]$description
            if(is.null(type)){
                type <- NA
            }
            
            active_from <- possibly_get_year_of_birth(int)
            active_to <- possibly_get_year_of_death(int)
            
            if(is.na(active_from) & is.na(active_to)){
                active_from <- possibly_get_years_active_from(int)
                active_to <- possibly_get_years_active_to(int)
            }
            
            out <- data.frame(
                interpret_id = interpret_id, 
                interpret = interpret, 
                type = type, 
                active_from = active_from, 
                active_to = active_to
            )
            return(out)
        }
    }
    
    data.frame(
        interpret_id = interpret_id, 
        interpret = interpret
    )
    
}

# get new interprets
wikipedia_interprets <- readRDS("output/wikipedia_interprets.RData") %>%
    mutate(active_from = as.Date(active_from), 
           active_to = as.Date(active_to, format = "%Y-%m-%d"))

new_interprets <- interprets[!interprets$interpret_id %in% wikipedia_interprets$interpret_id, ]

purrr::map_df(seq(nrow(new_interprets)), function(x) {
    cat(x, "\n")
    get_years_active(interpret = new_interprets$interpret[x], 
                     interpret_id = new_interprets$interpret_id[x])
}) -> new_wikipedia_interprets

updated_wikipedia_interprets <- bind_rows(wikipedia_interprets, new_wikipedia_interprets)
saveRDS(updated_wikipedia_interprets, "output/wikipedia_interprets.RData")

# TODO: update data for interprets with NA in death column
