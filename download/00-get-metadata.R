
# packages ----------------------------------------------------------------

library(tidyverse)
library(glue)
library(httr)
library(rvest)
library(janitor)
library(progress)

# helper function ---------------------------------------------------------

scraper_meta <- function(x) {
  
  website <- httr::RETRY("GET", glue::glue("https://letterboxd.com/films/ajax/popular/page/{x}/?esiAllowFilters=true"))
  stopifnot(httr::status_code(website) == 200)
  website <- httr::content(website)
  
  id <- website |> 
    rvest::html_elements("li div") |>
    rvest::html_attrs() |> 
    dplyr::bind_rows() |> 
    janitor::clean_names() |> 
    dplyr::select("data_film_slug", "data_film_id", "data_cache_busting_key")
  
  rating <- website |> 
    rvest::html_elements("li") |> 
    rvest::html_attrs() |> 
    dplyr::bind_rows() |> 
    janitor::clean_names() |> 
    dplyr::select("data_average_rating")
  
  img <- website |> 
    rvest::html_elements("li img") |> 
    rvest::html_attrs() |> 
    dplyr::bind_rows() |> 
    janitor::clean_names() |> 
    dplyr::select("alt", "src")
  
  out <- dplyr::bind_cols(id, rating, img)
  
  return(out)
  
}

# download ----------------------------------------------------------------

N <- 3000 ## number of pages to scrape

outfolder <- "download/metadata-files/"
if (!dir.exists(outfolder)) dir.create(outfolder)


# pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)\n", total = N)
#
# for (n in 1:N) {
#   out <- try(scraper_meta(n))
#   write_rds(out, str_glue("{outfolder}{n}.rds"), compress = "gz")
#   
#   pb$tick()
#   Sys.sleep(runif(1, 1, 5))
#   
# }


# assembly ----------------------------------------------------------------

output <- dir(outfolder, full.names = TRUE) |> 
  map(read_rds)

## make sure that there are no try-errors in the files
error_index <- output |> 
  map_lgl(\(x) any(class(x) == "try-error")) 

sum(error_index)

df <- bind_rows(output[!error_index]) |> 
  mutate(data_average_rating = as.double(data_average_rating)) |> 
  mutate(data_film_id = as.integer(data_film_id))

write_rds(df, glue("download/metadata.rds"), compress = "gz")
