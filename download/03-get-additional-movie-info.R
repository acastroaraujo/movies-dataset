
# packages ----------------------------------------------------------------

library(tidyverse)
library(glue)
library(httr)
library(rvest)
library(jsonlite)
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
  
  duration <- website |> 
    rvest::html_nodes(".text-footer") |> 
    rvest::html_text() |> 
    stringr::str_extract("[\\d,]+[:whitespace:][:alpha:]+")
  
  out <- list(
    title = title, 
    year = year,
    duration = duration,
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
  
## use the following to fix the user_ratings file later on.
## slug is not as reliable as integer id!
## but some integer ids might be fishy too..

## names are correct, values are redirected

extra_slugs_duplicated <- c(
  "/film/zvenigora/" = "/film/zvenygora/",
  "/film/free-radicals-1958/" = "/film/free-radicals-1979/", 
  "/film/the-adventures-of-dollie/" = "/film/adventures-of-dollie/", 
  "/film/du-cote-dorouet/" = "/film/around-orouet/", 
  "/film/the-bride-and-the-beast/" = "/film/the-bride-the-beast/",
  "/film/slow-motion-2013/" = "/film/slow-motion-2014/",
  "/film/pinocchio-2022/" = "/film/pinocchio-2022-1/",
  "/film/the-wonderland-2019/" = "/film/birthday-wonderland/",
  "/film/zerograd/" = "/film/city-zero/",
  "/film/the-switchblade-sisters/" = "/film/switchblade-sisters/"
)

df <- df |> 
  filter(!data_film_slug %in% extra_slugs_duplicated)

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

library(furrr)
plan(multisession, workers = parallel::detectCores() - 1L)

files <- dir(outfolder, full.names = TRUE)
output <- furrr::future_map(files, readr::read_rds)

error_index <- output |> 
  furrr::future_map_lgl(\(x) any(class(x) == "try-error")) 

message("errors: ", sum(error_index))

if (sum(error_index) > 0) {
  file.remove(files[error_index])
  output <- output[!error_index]
}


# metadata ----------------------------------------------------------------

metadata <- map_dfr(output, \(x) {
    tibble::as_tibble(x[c("title", "data_film_slug", "year", "duration")])
  }) |> 
  mutate(duration = as.integer(readr::parse_number(duration))) |> 
  filter(data_film_slug %in% df$data_film_slug) |> 
  inner_join(df) |> 
  mutate(data_film_id = as.integer(data_film_id))

readr::write_rds(metadata, "download/metadata.rds", compress = "gz")

# cast --------------------------------------------------------------------

cast <- map_df(output, pluck, "cast") |> 
  rename(role = title) |> 
  filter(data_film_slug %in% df$data_film_slug) 

cast <- cast |> 
  mutate(actor_href = factor(actor_href), data_film_slug = factor(data_film_slug))

readr::write_rds(cast, "download/cast.rds", compress = "gz")

# crew --------------------------------------------------------------------

crew <- map(output, pluck, "crew")
names(crew) <- map_chr(output, pluck, "data_film_slug")
crew <- crew[names(crew) %in% df$data_film_slug]

crew <- enframe(crew, "data_film_slug", "person_href") |> 
  unnest(person_href) |> 
  mutate(
    role = str_replace(person_href, "/(.+)/(.+)/", "\\1"), 
    person = str_replace(person_href, "/(.+)/(.+)/", "\\2")
  ) |> 
  mutate(data_film_slug = factor(data_film_slug), role = factor(role))

readr::write_rds(crew, "download/crew.rds", compress = "gz")


# genres ------------------------------------------------------------------

genre <- output |> map(pluck, "genres")
names(genre) <- output |> map_chr(pluck, "data_film_slug")
genre <- genre[names(genre) %in% df$data_film_slug]

genre <- enframe(genre, name = "data_film_slug", value = "genre_href") |> 
  unnest(genre_href) |> 
  filter(str_detect(genre_href, "/films/genre/")) |> 
  mutate(genre = str_replace(genre_href, "/(.+)/(.+)/(.+)/", "\\3"))

readr::write_rds(genre, "download/genre.rds", compress = "gz")

# details -----------------------------------------------------------------

details <- output |> map(pluck, "details")
names(details) <- output |> map_chr(pluck, "data_film_slug")
details <- details[names(details) %in% df$data_film_slug]

replace_pattern <- c(
  "^Studios$" = "Studio", 
  "^Countries$" = "Country", 
  "^Alternative Titles$" = "Alternative Title",
  "^Original Language$" = "Language",
  "^Spoken Languages$" = "Spoken Language"
  )

index <- which(map_lgl(details, is.null))
details <- details[-index]

details <- map(details, function(x) {
  
  names(x) <- str_replace_all(names(x), replace_pattern)
  x[names(x) != "Alternative Title"]
  
})

df_details <- tibble(data_film_slug = names(details))
df_details$Studio <- map(details, pluck, "Studio")
df_details$Country <- map(details, pluck, "Country")
df_details$Language <- map(details, pluck, "Language")
df_details$`Spoken Language` <- map(details, pluck, "Spoken Language")

readr::write_rds(df_details, "download/details.rds", compress = "gz")
