
### Note: I have removed movies with empty ratings. 

# packages ----------------------------------------------------------------

library(tidyverse)
library(glue)
library(httr)
library(rvest)
library(janitor)
library(progress)

# helper functions --------------------------------------------------------

scrape_movie_from_user <- function(user) {
  
  website <- httr::RETRY("GET", glue::glue("https://letterboxd.com/{user}/films/page/1"))
  stopifnot(httr::status_code(website) == 200)
  website <- httr::content(website)
  
  df_1 <- website |> 
    rvest::html_elements(".poster-container div") |> 
    rvest::html_attrs() |> 
    dplyr::bind_rows() |> 
    janitor::clean_names() |> 
    dplyr::select("data_film_slug", "data_film_id", "data_cache_busting_key")
  
  df_1$rating <- website |> 
    rvest::html_elements(".poster-viewingdata") |>
    rvest::html_text()
  
  N <- website |> 
    rvest::html_elements(".paginate-page") |> 
    rvest::html_text() |> 
    dplyr::last() |> 
    base::as.integer()
  
  if (is.na(N)) N <- 1L ## users with few movies don't have pagination
  message(glue("{user}: 1 of {N} pages"))
  
  out <- vector("list", N)  
  out[[1]] <- df_1
  
  if (N > 1) {
    
    for (n in 2:length(out)) {
      
      website <- httr::RETRY("GET", glue::glue("https://letterboxd.com/{user}/films/page/{n}"))
      stopifnot(httr::status_code(website) == 200)
      website <- httr::content(website)
      
      df_n <- website |> 
        rvest::html_elements(".poster-container div") |> 
        rvest::html_attrs() |> 
        dplyr::bind_rows() |> 
        janitor::clean_names() |> 
        dplyr::select("data_film_slug", "data_film_id", "data_cache_busting_key")
      
      df_n$rating <- website |> 
        rvest::html_elements(".poster-viewingdata") |>
        rvest::html_text()
      
      out[[n]] <- df_n
      
      message(glue("{user}: {n} of {N} pages"))
      Sys.sleep(runif(1, 0.5, 2))
      
    }
  }
  
  out <- dplyr::bind_rows(out)
  out$user <- user
  return(out)
  
}

stars_to_numbers <- function(x) {
  dplyr::case_when(
    x == "½" ~ 0.5,
    x == "★" ~ 1,
    x == "★½" ~ 1.5,
    x == "★★" ~ 2,
    x == "★★½" ~ 2.5,
    x == "★★★" ~ 3,
    x == "★★★½" ~ 3.5,
    x == "★★★★" ~ 4,
    x == "★★★★½" ~ 4.5,
    x == "★★★★★" ~ 5,
    TRUE ~ NA_real_ ## this turns empty ratings into missing values
  )
}

# download ----------------------------------------------------------------

outfolder <- "download/user-files/"
if (!dir.exists(outfolder)) dir.create(outfolder)

users <- read_rds("download/users.rds") |> 
  mutate(id = str_remove_all(href, "/")) |> ## I removed the "/" because these names are used as file names later on
  pull(id)

done <- str_replace(dir(outfolder), "\\.rds$", "")
left <- setdiff(users, done)

pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)\n", total = length(left))

while (length(left) > 0) { 
  
  x <- sample(left, 1)
  output <- try(scrape_movie_from_user(x))
  
  readr::write_rds(output, glue("{outfolder}{x}.rds"), compress = "gz")
  left <- left[-which(left %in% x)] ## int. subset
  
  pb$tick()
  Sys.sleep(runif(1, 1, 2))
  
}

# organize ----------------------------------------------------------------

library(furrr)
plan(multisession, workers = parallel::detectCores() - 1L)

files <- dir(outfolder, full.names = TRUE)
output <- furrr::future_map(files, readr::read_rds)

names(output) <- stringr::str_remove(files, "\\.rds$") |> 
  stringr::str_remove(outfolder)

error_index <- purrr::map_lgl(output, \(x) any(class(x) == "try-error")) 

message("errors: ", sum(error_index))

if (sum(error_index) > 0) {
  file.remove(glue("{outfolder}{names(output[error_index])}.rds"))
  output <- output[!error_index]
}

df <- bind_rows(output)

df <- df |> 
  mutate(rating = stars_to_numbers(str_squish(rating))) |> 
  rename(href = user) |>             ## for compatibility purposes
  mutate(href = str_replace(href, "(.+)", "/\\1/")) |> ## put back "/"'s
  select(!data_cache_busting_key) |> ## this ended up being useless
  relocate(href) |>  
  filter(!is.na(rating)) |>          ## this removes empty ratings
  add_count(href, name = "n_href") |> 
  add_count(data_film_slug, name = "n_film") |> 
  filter(n_film >= 10, n_href >= 10) |> 
  mutate(data_film_id = as.integer(data_film_id)) |> 
  select(-c(n_href, n_film))

## the following lines are for memory efficiency

user_levels <- unique(df$href)
film_levels <- unique(df$data_film_slug)

df <- df |> 
  mutate(
    href = factor(href, levels = user_levels),
    data_film_slug = factor(data_film_slug, levels = film_levels),
    rating = as.integer(rating*2) ## transform and store as integer in [1, 10]
  )

readr::write_rds(df, "download/user_ratings.rds", compress = "gz")

