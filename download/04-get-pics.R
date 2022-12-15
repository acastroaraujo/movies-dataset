
# packages ----------------------------------------------------------------

library(tidyverse)
library(glue)
library(progress)

# helper functions --------------------------------------------------------

modify_resize <- function(x) {
  
  x |> 
    stringr::str_replace("0-230-0-345-crop.jpg", "0-1000-0-1500-crop.jpg") |> 
    stringr::str_remove("\\?.+$")
  
}

# download ----------------------------------------------------------------

outfolder <- "download/movie-pics/" 
if (!dir.exists(outfolder)) dir.create(outfolder)

lookup_id <- read_rds("download/user_ratings.rds") |> 
  distinct(data_film_slug, data_film_id) |> 
  mutate_all(as.character) |> ## beware: integers -> characters
  deframe()

files <- dir("download/movie-files/", full.names = TRUE)

df <- map_df(files, function(x) {
    out <- read_rds(x)
    return(list(slug = out[["data_film_slug"]], pic_url = out[["poster"]]))
  }) |> 
  mutate(data_film_id = lookup_id[slug]) |> 
  mutate(pic_url = modify_resize(pic_url)) 

df <- filter(df, !is.na(pic_url))

done <- str_replace(dir(outfolder), "\\.jpg$", "")
left <- setdiff(df$data_film_id, done)

pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)\n", total = length(left))

while (length(left) > 0) { 
  
  x <- sample(left, 1)
  sub <- filter(df, data_film_id == x)
  
  message(sub$slug)
  try(download.file(sub$pic_url, destfile = glue("{outfolder}{sub$data_film_id}.jpg"), quiet = TRUE))
  left <- left[-which(left %in% x)] ## int. subset
  
  pb$tick()
  Sys.sleep(runif(1, 1, 4))
  
}


