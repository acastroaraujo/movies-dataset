
# packages ----------------------------------------------------------------

library(tidyverse)
library(glue)
library(httr)
library(rvest)
library(janitor)
library(progress)

# helper function ---------------------------------------------------------

scraper_user <- function(n) {
  
  website <- httr::RETRY("GET", glue::glue("https://letterboxd.com/members/popular/page/{n}/"))
  stopifnot(httr::status_code(website) == 200)
  website <- httr::content(website)
  
  out <- website |> 
    html_table() |>
    pluck(1) |> 
    dplyr::select(Name, Watched, Lists, Likes) |> 
    dplyr::rename_with(tolower) |> 
    separate(name, into = c("name", "reviews"), sep = "   ") |> 
    mutate(across(c(reviews, watched, likes), readr::parse_number)) |> 
    mutate(across(where(is.numeric), as.integer))
  
  out$href <- website |> 
    html_elements(".table-person .name") |> 
    html_attr("href")
    
  return(out)
  
}


# download ----------------------------------------------------------------

N <- 250 ## number of pages to scrape
out <- vector("list", N)

pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)\n", total = N)

for (n in 1:N) {
  out[[n]] <- try(scraper_user(n))
  pb$tick()
  Sys.sleep(runif(1, 1, 5))
}

error_index <- map_lgl(out, \(x) any(class(x) == "try-error")) 
sum(error_index)

df <- map_df(out[!error_index], function(x) {
  if (typeof(x$lists) == "character") {
    x <- mutate(x, lists = readr::parse_number(lists))
  }
  return(x)
})

write_rds(df, "download/users.rds", compress = "gz")
