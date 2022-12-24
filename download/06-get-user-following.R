
# packages ----------------------------------------------------------------

library(tidyverse)
library(glue)
library(httr)
library(rvest)
library(progress)

# helper function ---------------------------------------------------------

scraper_following <- function(user) {
  
  output <- list()
  n <- 1
  
  repeat {
    
    website <- httr::RETRY("GET", glue::glue("https://letterboxd.com/{user}/following/page/{n}/"))
    stopifnot(httr::status_code(website) == 200)
    website <- httr::content(website)
    
    following <- website |> 
      rvest::html_elements(".person-summary") |> 
      rvest::html_elements(".title-3") |>
      rvest::html_elements("a") 
    
    if (purrr::is_empty(following)) break
    
    el <- tibble::tibble(
      from = user,
      to = rvest::html_attr(following, "href")
    )
    
    output <- append(output, list(el))
    
    cat(glue::glue("{user}: page {n}\r"))
    Sys.sleep(runif(1, 0.3, 1))
    n <- n + 1
    
  }
  
  return(dplyr::bind_rows(output))
  
}


# download ----------------------------------------------------------------

outfolder <- "download/user-following/"
if (!dir.exists(outfolder)) dir.create(outfolder)

df <- read_rds("download/user_ratings.rds") |> 
  distinct(href) |> 
  mutate(id = str_remove_all(href, "/")) 
  
done <- str_replace(dir(outfolder), "\\.rds$", "")
left <- setdiff(df$id, done)

pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)\n", total = length(left))

while (length(left) > 0) {
  
  x <- sample(left, 1)
  output <- try(scraper_following(x))
  
  readr::write_rds(output, glue("{outfolder}{x}.rds"), compress = "gz")
  left <- left[-which(left %in% x)] ## int. subset
  
  pb$tick()
  Sys.sleep(runif(1, 1, 3))
  
}


# debug -------------------------------------------------------------------

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

user_levels <- levels(df$href)

el <- bind_rows(output) |> 
  mutate(from = glue("/{from}/")) |> 
  filter(to %in% df$href) |> 
  mutate_all(factor, levels = user_levels)

readr::write_rds(el, "download/user_following.rds", compress = "gz")



