
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
    rvest::html_elements(".js-widont") |> 
    rvest::html_text()
  
  year <- website |> 
    rvest::html_elements(".number") |> 
    rvest::html_text() |> 
    as.integer()
  
  actor <- website |> 
    rvest::html_elements("#tab-cast .tooltip") |> 
    rvest::html_text()
  
  role <- website |> 
    rvest::html_elements("#tab-cast .tooltip") |> 
    rvest::html_attr("title")
  
  genres <- website |> 
    rvest::html_elements("#tab-genres .text-slug") |> 
    rvest::html_attr("href")
  
  crew <- website |> 
    rvest::html_elements("#tab-crew .text-slug") |> 
    rvest::html_attr("href") 

  detail_names <- website |> 
    rvest::html_elements("#tab-details") |>
    rvest::html_elements("h3") |> 
    rvest::html_text()
  
  details <- website |> 
    rvest::html_elements("#tab-details") |>
    rvest::html_elements("div") |> 
    purrr::map(\(x) rvest::html_attr(rvest::html_elements(x, ".text-slug"), "href")) |> 
    purrr::set_names(detail_names)
  
  # meta_attrs <- website |> 
  #   rvest::html_elements("head meta")  |> 
  #   rvest::html_attrs()
  # 
  # index <- which(purrr::map_lgl(meta_attrs, \(x) x["property"] == "og:image"))
  # poster <- meta_attrs[[index]][["content"]]
  
  pic <- website |> 
    rvest::html_elements(xpath = '//*[@id="html"]/body/script[3]/text()') |> 
    rvest::html_text() |> 
    stringr::str_split("\n") |> 
    unlist() |> 
    purrr::pluck(3) |> 
    jsonlite::fromJSON() |> 
    purrr::pluck("image")
  
  out <- list(
    title = title, 
    year = year,
    cast = tibble::tibble(actor, role, data_film_slug = path), 
    genres = genres, 
    crew = crew, 
    details = details,
    poster = pic, 
    data_film_slug = path
  )
  
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
  
  pb$tick()
  Sys.sleep(runif(1, 2, 4))
  
}







