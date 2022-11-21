
# Note. THE WHOLE THING SEEMS CAPPED AT 5000 USERS, THIS MEANS THAT REALLY POPULAR MOVIES HAVE MASSIVE MISSING DATA.

# packages ----------------------------------------------------------------

library(tidyverse)
library(httr)
library(glue)
library(rvest)
library(progress)

# helper function ---------------------------------------------------------

scraper_movie <- function(path) {
  
  output <- list() ## this is somewhat inefficient, but I don't know how long it should be
  
  n <- 1
  
  repeat {
    
    website <- httr::RETRY("GET", glue::glue("https://letterboxd.com{path}ratings/page/{n}"))
    stopifnot(httr::status_code(website) == 200)
    website <- httr::content(website)
    
    groups <- website |> 
      rvest::html_elements(".film-rating-group")
    
    if (is_empty(groups)) break
    
    output <- append(output, list(purrr::map_df(groups, user_chunk)))
    
    cat(glue::glue("{path}, page: {n}\r"))
    Sys.sleep(runif(1, 1, 3))
    
    n <- n + 1
    
  }
  
  df <- dplyr::bind_rows(output) |> 
    dplyr::rename(user = title, user_href = href)
  
  df$data_film_slug <- path
  
  return(df)
  
}

user_chunk <- function(g) {
  
  rating <- g |> 
    rvest::html_elements("h2") |> 
    rvest::html_text() |> 
    stringr::str_squish()
  
  users <- g |> 
    rvest::html_elements(".avatar") |> 
    rvest::html_attrs() |> 
    dplyr::bind_rows() |> 
    dplyr::select("href", "title")
  
  users$rating <- rating
  
  return(users)
  
}

## i think big movies are capped at 5000 !?
## ews <- scraper_movie("/film/eyes-wide-shut/")

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
  
  x <- left[length(left)] ## starting at the end assures that the most popular movies are scraped last
  output <- try(scraper_movie(movies[[x]]))
  
  output$data_film_id <- x
  
  write_rds(output, str_glue("{outfolder}{x}.rds"), compress = "gz")
  left <- left[-which(left %in% x)] ## int. subset
  
  pb$tick()
  Sys.sleep(runif(1, 1, 3))
  
}

output <- dir(outfolder, full.names = TRUE) |> 
  map(read_rds)

error_index <- output |> 
  map_lgl(\(x) any(class(x) == "try-error")) 

df_index <- output |> 
  map_lgl(is.data.frame)

el <- bind_rows(output[!error_index & df_index])

el <- el |> 
  group_by(data_film_id) |>
  filter(n() < 5e3) |> 
  ungroup()

rating_replace <- c(
  "½" = 0.5,
  "★" = 1,
  "★½" = 1.5,
  "★★" = 2,
  "★★½" = 2.5,
  "★★★" = 3,
  "★★★½" = 3.5,
  "★★★★" = 4,
  "★★★★½" = 4.5,
  "★★★★★" = 5
)

el <- el |> 
  rename(stars = rating) |> 
  mutate(rating = rating_replace[stars]) |> 
  mutate(data_film_id = as.integer(data_film_id))

write_rds(el, "download/ratings_subset.rds", compress = "gz")

