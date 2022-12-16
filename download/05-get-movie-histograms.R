
# packages ----------------------------------------------------------------

library(tidyverse)
library(glue)
library(httr)
library(rvest)
library(progress)

# helper function ---------------------------------------------------------

scraper_histogram <- function(path) {
  
  website <- httr::RETRY("GET", glue::glue("https://letterboxd.com/csi{path}rating-histogram/"))
  stopifnot(httr::status_code(website) == 200)
  website <- httr::content(website)
  
  ratings <- website |> 
    rvest::html_elements(".rating-histogram-bar") |> 
    rvest::html_text() |> 
    readr::parse_number() |> 
    as.integer()
  
  names(ratings) <- 1:10
  ratings[is.na(ratings)] <- 0L
  
  out <- tibble::enframe(ratings, name = "rating", value = "n")
  out$rating <- as.integer(out$rating)
  out$data_film_slug <- path
  
  return(out)
  
}

# download ----------------------------------------------------------------

outfolder <- "download/movie-histograms/"
if (!dir.exists(outfolder)) dir.create(outfolder)

df <- read_rds("download/metadata.rds") |> 
  distinct(data_film_slug, data_film_id) |> 
  mutate_all(as.character) 

done <- str_replace(dir(outfolder), "\\.rds$", "")
left <- setdiff(df$data_film_id, done)

pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)\n", total = length(left))

while (length(left) > 0) {
  
  x <- sample(left, 1)
  
  sub <- filter(df, data_film_id == x)
  message(sub[["data_film_slug"]])
  output <- try(scraper_histogram(sub[["data_film_slug"]]))
  
  readr::write_rds(output, glue("{outfolder}{x}.rds"), compress = "gz")
  left <- left[-which(left %in% x)] ## int. subset
  
  pb$tick()
  Sys.sleep(runif(1, 1, 2))
  
}


# debug -------------------------------------------------------------------

# library(furrr)
# plan(multisession, workers = parallel::detectCores() - 1L)

files <- dir(outfolder, full.names = TRUE)
output <- map(files, readr::read_rds)

error_index <- output |> 
  map_lgl(\(x) any(class(x) == "try-error")) 

message("errors: ", sum(error_index))

if (sum(error_index) > 0) {
  file.remove(files[error_index])
  output <- output[!error_index]
}


# organize ----------------------------------------------------------------

full_ratings <- bind_rows(output)
movie_levels <- unique(df$data_film_slug)

full_ratings <- left_join(full_ratings, df) |> 
  mutate(data_film_id = as.integer(data_film_id)) |> 
  mutate(data_film_slug = factor(data_film_slug, levels = movie_levels))

full_ratings

