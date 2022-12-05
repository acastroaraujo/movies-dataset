
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
  
  if (purrr::is_empty(year)) {
    year <- NA_integer_
  }
  
  cast_tab <- website |> 
    rvest::html_elements("#tab-cast .tooltip")
  
  if (!is_empty(cast_tab)) {
    
    cast <- website |> 
      rvest::html_elements("#tab-cast .tooltip") |> 
      rvest::html_attrs() |> 
      dplyr::bind_rows() |> 
      dplyr::select(!class) |> 
      dplyr::rename(actor_href = href)
    
    actor_name <- website |> 
      rvest::html_elements("#tab-cast .tooltip") |> 
      rvest::html_text()
    
    cast$actor <- actor_name
    cast$data_film_slug <- path
    
  } else {
    cast <- NULL
  }
  
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
  
  pic <- website |> 
    rvest::html_elements(xpath = '//*[@id="html"]/body/script[3]/text()') |> 
    rvest::html_text() |> 
    stringr::str_split("\n") |> 
    purrr::flatten_chr() |> 
    purrr::pluck(3) |> 
    jsonlite::fromJSON() |> 
    purrr::pluck("image")
  
  out <- list(
    title = title, 
    year = year,
    cast = cast, 
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
  message(sub[["data_film_slug"]])
  output <- try(scraper_movie(sub[["data_film_slug"]]))
  
  readr::write_rds(output, glue("{outfolder}{x}.rds"), compress = "gz")
  left <- left[-which(left %in% x)] ## int. subset
  
  pb$tick()
  Sys.sleep(runif(1, 1, 4))
  
}

# debug -------------------------------------------------------------------

files <- dir(outfolder, full.names = TRUE)
output <- map(files, read_rds)

error_index <- output |> 
  map_lgl(\(x) any(class(x) == "try-error")) 

if (sum(error_index) > 0) {
  file.remove(files[error_index])
  output <- output[!error_index]
}

# organize ----------------------------------------------------------------

cast <- map_df(output, pluck, "cast")
metadata <- map_df(output, \(x) x[c("title", "year", "data_film_slug")])



# movies <- map_df(output, function(x) {
#   
#   out <- x[c("title", "year", "data_film_slug")] 
#   if (is_empty(temp$year)) temp$year <- NA_integer_
#   
#   return(out)
#   
# })
# 
# i <- 836
# i <- 10
# 
# movies[i, ]
# output[i]
# 
# 
# t <- map_chr(output, pluck, "title")
# 
# y <- map(output, pluck, "year")
# 
# 
# t[836]
# y[836]
# map_chr(output, pluck, "data_film_slug")
# 
# 

