library(dplyr)
library(RSQLite)
library(WikidataR)

con <- dbConnect(SQLite(), "data_2020.sqlite")
interprets <- dbGetQuery(con, "select * from interprets")

r <- dbSendStatement(con, "create table if not exists wikipedia_interprets (interpret_id int, wikipedia_id text, interpret text, type text, active_from text, active_to text)")
dbClearResult(r)

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

insert_death_into_db <- function(connection, interpret_id, wikipedia_id, 
                                 interpret, type, active_from, active_to){
    r <- dbSendStatement(connection, 
                         paste0("insert into wikipedia_interprets values (", 
                                interpret_id, ", '", 
                                wikipedia_id, "', '",
                                gsub("'", "''", interpret), "', '", 
                                gsub("'", "''", type), "', '", 
                                active_from, "', '", 
                                active_to, "')"
                                ))
    dbClearResult(r)
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

get_years_active <- function(connection, interpret, interpret_id){
    item <- find_item(interpret, lang = "cs")
    
    if(length(item)){
        artists <- find_artist(item)
        if(length(artists)){
            wikipedia_id <- artists[[1]]$id
            int <- get_item(wikipedia_id)
            
            type <- artists[[1]]$description
            
            active_from <- possibly_get_year_of_birth(int)
            active_to <- possibly_get_year_of_death(int)
            
            if(is.na(active_from) & is.na(active_to)){
                active_from <- possibly_get_years_active_from(int)
                active_to <- possibly_get_years_active_to(int)
            }
            
            insert_death_into_db(connection, interpret_id, wikipedia_id, interpret, 
                                 type, active_from, active_to)   
        }
    }
}

# get new interprets
wikipedia_interprets <- dbGetQuery(con, "select * from wikipedia_interprets")

new_interprets <- interprets[!interprets$interpret_id %in% wikipedia_interprets$interpret_id, ]

for(i in seq(nrow(new_interprets))){
    cat(new_interprets$interpret[i], "\n")
    get_years_active(connection = con, 
              interpret = new_interprets$interpret[i], 
              interpret_id = new_interprets$interpret_id[i])
}

# TODO: update data for interprets with NA in death column
