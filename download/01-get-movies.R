
# packages ----------------------------------------------------------------

library(tidyverse)
library(glue)
library(httr)
library(rvest)
library(janitor)
library(progress)

# helper function ---------------------------------------------------------

scraper_movie <- function(path) {
  
  output <- list() ## this is somewhat inefficient, but I don't know how long it should be
  
  n <- 1
  
  repeat {
    
    website <- RETRY("GET", glue("https://letterboxd.com/{path}/ratings/page/{n}"))
    stopifnot(httr::status_code(website) == 200)
    website <- httr::content(website)
    
    groups <- website |> 
      rvest::html_elements(".film-rating-group")
    
    if (is_empty(groups)) break
    
    output <- append(output, list(map_df(groups, user_chunk)))
    
    cat(glue("{path}, page: {n}\r"))
    Sys.sleep(runif(1, 1, 3))
    
    n <- n + 1
    
  }
  
  df <- bind_rows(output) |> 
    rename(user = title)
  
  df$data_film_slug <- path
  
  return(df)
  
}

user_chunk <- function(g) {
  
  rating <- g |> 
    html_elements("h2") |> 
    html_text() |> 
    str_squish()
  
  users <- g |> 
    html_elements(".avatar") |> 
    html_attrs() |> 
    bind_rows() |> 
    select("href", "title")
  
  users$rating <- rating
  
  return(users)
  
}


# download ----------------------------------------------------------------

outfolder <- "download/movie-files/"
if (!dir.exists(outfolder)) dir.create(outfolder)

movies <- read_rds("download/metadata.rds") |> 
  select(data_film_id, data_film_slug) |> 
  distinct() |> 
  deframe()

done <- str_replace(dir(outfolder), ".rds", "")
left <- setdiff(names(movies), done)

pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)\n", total = length(left))

while (length(left) > 0) { 
  
  x <- sample(left, 1)
  output <- try(scraper_movie(movies[[x]]))
  
  output$data_film_id <- x
  
  write_rds(output, str_glue("{outfolder}{x}.rds"), compress = "gz")
  left <- left[-which(left %in% x)] ## int. subset
  
  pb$tick()
  Sys.sleep(runif(1, 1, 3))
  
}




