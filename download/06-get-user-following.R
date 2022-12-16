
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
    Sys.sleep(runif(1, 1, 2))
    n <- n + 1
    
  }
  
  return(dplyr::bind_rows(output))
  
}


# download ----------------------------------------------------------------

outfolder <- "download/user-following/"
if (!dir.exists(outfolder)) dir.create(outfolder)

df <- read_rds("download/users.rds") |> 
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
