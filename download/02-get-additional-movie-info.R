
# packages ----------------------------------------------------------------

library(tidyverse)
library(glue)
library(httr)
library(rvest)
library(progress)

# helper function ---------------------------------------------------------

scraper_movie <- function(path) {
  
  website <- httr::RETRY("GET", glue::glue("https://letterboxd.com{path}"))
  stopifnot(httr::status_code(website) == 200)
  website <- httr::content(website)
  
  title <- website |> 
    html_elements(".js-widont") |> 
    html_text()
  
  year <- website |> 
    html_elements(".number") |> 
    html_text() |> 
    as.integer()
  
  actor <- website |> 
    html_elements("#tab-cast .tooltip") |> 
    html_text()
  
  role <- website |> 
    html_elements("#tab-cast .tooltip") |> 
    html_attr("title")
  
  genres <- website |> 
    html_elements("#tab-genres .text-slug") |> 
    html_attr("href")
  
  crew <- website |> 
    html_elements("#tab-crew .text-slug") |> 
    html_attr("href")
  
  meta_attrs <- website |> 
    html_elements("head meta")  |> 
    html_attrs()
  
  index <- which(map_lgl(meta_attrs, \(x) x["property"] == "og:image"))
  poster <- meta_attrs[[index]][["content"]]
  
  out <- list(cast = tibble(actor, role, data_film_slug = path), genres = genres, crew = crew, poster = poster, data_film_slug = path)
  return(out)
  
}

# download ----------------------------------------------------------------

outfolder <- "download/movie-files/"
if (!dir.exists(outfolder)) dir.create(outfolder)

df <- read_rds("download/user_ratings.rds") |> 
  distinct(data_film_slug, data_film_id) |> 
  mutate_all(as.character)
  
done <- str_replace(dir(outfolder), "\\.rds$", "")
left <- setdiff(df$data_film_id, done)

pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)\n", total = length(left))

while (length(left) > 0) {
  
  x <- sample(left, 1)
  
  sub <- filter(df, data_film_id == x)
  output <- try(scraper_movie(sub[["data_film_slug"]]))
  
  readr::write_rds(output, glue("{outfolder}{x}.rds"), compress = "gz")
  left <- left[-which(left %in% x)] ## int. subset
  output
  
  pb$tick()
  Sys.sleep(runif(1, 2, 4))
  
}







