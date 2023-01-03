
# packages ----------------------------------------------------------------

library(tidyverse)
library(glue)
library(httr)
library(rvest)
library(janitor)
library(progress)

# helper function ---------------------------------------------------------

scraper_user <- function(n) {
  
  ## note change to /popular/this/month/page/...
  website <- httr::RETRY("GET", glue::glue("https://letterboxd.com/members/popular/this/month/page/{n}/"))
  stopifnot(httr::status_code(website) == 200)
  website <- httr::content(website)
  
  out <- website |> 
    rvest::html_table() |>
    purrr::pluck(1) |> 
    dplyr::select(Name, Watched, Lists, Likes) |> 
    dplyr::rename_with(tolower) |> 
    tidyr::separate(name, into = c("name", "reviews"), sep = "   ") |> 
    dplyr::mutate(across(c(reviews, watched, likes), readr::parse_number)) |> 
    dplyr::mutate(across(where(is.numeric), as.integer))
  
  out$href <- website |> 
    rvest::html_elements(".table-person .name") |> 
    rvest::html_attr("href")
  
  return(out)
  
}

# load existing user list -------------------------------------------------

df <- readr::read_rds("download/users.rds")

# download new users ------------------------------------------------------

N <- 100 ## number of pages to scrape
out <- vector("list", N)

pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)\n", total = N)

for (n in 1:N) {
  out[[n]] <- try(scraper_user(n))
  pb$tick()
  Sys.sleep(runif(1, 1, 3))
}

error_index <- map_lgl(out, \(x) any(class(x) == "try-error")) 
message("errors: ", sum(error_index))

df_update <- map_df(out[!error_index], function(x) {
  if (typeof(x$lists) == "character") {
    x <- mutate(x, lists = readr::parse_number(lists))
  }
  return(x)
})


# combine user list -------------------------------------------------------

df <- df |> 
  filter(!href %in% df_update$href) |> 
  full_join(df_update)


# re-write dataset --------------------------------------------------------

readr::write_rds(df, "download/users.rds")

