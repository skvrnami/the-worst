library(dplyr)
library(RSQLite)
library(WikidataR)

con <- dbConnect(SQLite(), "data.sqlite")
interprets <- dbGetQuery(con, "select * from interprets")

r <- dbSendStatement(con, "create table if not exists interprets_age (interpret_id int, interpret text, birth text, death text)")
dbClearResult(r)

convert_date_or_na <- function(x){
    if(is.null(x)){
        "NA"
    }else{
        as.Date(x, format = "+%Y-%m-%d")
    }
}

insert_death_into_db <- function(connection, interpret_id, interpret, birth, death){
    r <- dbSendStatement(connection, 
                         paste0("insert into interprets_age values (", 
                                interpret_id, ", '", 
                                gsub("'", "`", interpret), "', '", 
                                convert_date_or_na(birth), "', '", 
                                convert_date_or_na(death), "')"
                                ))
    dbClearResult(r)
}

get_year_of_death <- function(int){
    int[[1]]$claims$P570$mainsnak$datavalue$value$time[1]
}

get_year_of_birth <- function(int){
    int[[1]]$claims$P569$mainsnak$datavalue$value$time[1]
}

possibly_get_year_of_death <- purrr::possibly(get_year_of_death, NA_character_)

possibly_get_year_of_birth <- purrr::possibly(get_year_of_birth, NA_character_)

get_years <- function(connection, interpret, interpret_id){
    item <- find_item(interpret, lang = "cs")
    if(length(item) > 0){
        int <- get_item(item[[1]]$id)
        
        birth <- possibly_get_year_of_birth(int)
        death <- possibly_get_year_of_death(int)
        
        insert_death_into_db(connection, interpret_id, interpret, birth, death)
    }
}

for(i in 1888:nrow(interprets)){
    cat(interprets$interpret[i], "\n")
    get_years(connection = con, 
              interpret = interprets$interpret[i], 
              interpret_id = interprets$interpret_id[i])
}

int_age <- dbGetQuery(con, "select * from interprets_age")
