

## ADD DATE TO SCRAPER BEFORE DOWNLOADING, also producers

# packages ----------------------------------------------------------------

library(tidyverse)
library(httr)
library(glue)
library(rvest)
library(progress)

# helper function ---------------------------------------------------------

scraper_add_movie <- function(path) {
  
  website <- httr::RETRY("GET", glue::glue("https://letterboxd.com/film/triangle-of-sadness/"))
  stopifnot(httr::status_code(website) == 200)
  website <- httr::content(website)
  
  actor <- website |> 
    html_elements("#tab-cast .tooltip") |> 
    html_text()
  
  role <- website |> 
    html_elements("#tab-cast .tooltip") |> 
    html_attr("title")
  
  genres <- website |> 
    html_elements("#tab-genres .text-slug") |> 
    html_attr("href")

  return(list(tibble(actor, role), genres))
  
}

