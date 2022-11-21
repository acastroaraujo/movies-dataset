
### TO DO, FIGURE OUT WHAT TO ASSIGN TO EMPTY RATINGS, SHOULD I JUST REMOVE THEM??


# packages ----------------------------------------------------------------

library(tidyverse)
library(glue)
library(httr)
library(rvest)
library(janitor)
library(progress)

# helper function ---------------------------------------------------------

scrape_movie_from_user <- function(user) {
  
  website <- httr::RETRY("GET", glue::glue("https://letterboxd.com/{user}/films/page/1"))
  stopifnot(httr::status_code(website) == 200)
  website <- httr::content(website)
  
  df_1 <- website |> 
    rvest::html_elements(".poster-container div") |> 
    rvest::html_attrs() |> 
    dplyr::bind_rows() |> 
    janitor::clean_names() |> 
    dplyr::select("data_film_slug", "data_film_id", "data_cache_busting_key")
  
  df_1$rating <- website |> 
    rvest::html_elements(".poster-viewingdata") |>
    rvest::html_text()
  
  N <- website |> 
    rvest::html_elements(".paginate-page") |> 
    rvest::html_text() |> 
    dplyr::last() |> 
    base::as.integer()
  
  message(glue("{user}: 1 of {N} pages"))
  
  out <- vector("list", N)  
  out[[1]] <- df_1
  
  if (N > 1) {
    
    for (n in 2:length(out)) {
      
      website <- httr::RETRY("GET", glue::glue("https://letterboxd.com/{user}/films/page/{n}"))
      stopifnot(httr::status_code(website) == 200)
      website <- httr::content(website)
      
      df_n <- website |> 
        rvest::html_elements(".poster-container div") |> 
        rvest::html_attrs() |> 
        dplyr::bind_rows() |> 
        janitor::clean_names() |> 
        dplyr::select("data_film_slug", "data_film_id", "data_cache_busting_key")
      
      df_n$rating <- website |> 
        rvest::html_elements(".poster-viewingdata") |>
        rvest::html_text()
      
      out[[n]] <- df_n
      
      message(glue("{user}: {n} of {N} pages"))
      Sys.sleep(runif(1, 1, 3))
      
    }
  }
  
  out <- dplyr::bind_rows(out)
  out$user <- user
  return(out)
  
}


# download ----------------------------------------------------------------

outfolder <- "download/user-files/"
if (!dir.exists(outfolder)) dir.create(outfolder)

users <- read_rds("download/users.rds") |> 
  mutate(id = str_remove_all(href, "/")) |> ## I removed the "/" because these names are used as file names later on
  pull(id)

done <- str_replace(dir(outfolder), ".rds", "")
left <- setdiff(users, done)

pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)\n", total = length(left))

while (length(left) > 0) { 
  
  x <- sample(left, 1)
  output <- try(scrape_movie_from_user(x))
  
  write_rds(output, glue("{outfolder}{x}.rds"), compress = "gz")
  left <- left[-which(left %in% x)] ## int. subset
  
  pb$tick()
  Sys.sleep(runif(1, 2, 4))
  
}

# organize ----------------------------------------------------------------

output <- dir(outfolder, full.names = TRUE) |> 
  map(read_rds)

error_index <- output |> 
  map_lgl(\(x) any(class(x) == "try-error")) 

sum(error_index)

df <- bind_rows(output)
