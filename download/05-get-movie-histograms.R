
# packages ----------------------------------------------------------------

library(tidyverse)
library(glue)
library(httr)
library(rvest)
library(progress)

# helper function ---------------------------------------------------------

scraper_histogram <- function(path) {
  
  website <- httr::RETRY("GET", glue::glue("https://letterboxd.com/csi{path}rating-histogram/"))
  stopifnot(httr::status_code(website) == 200)
  website <- httr::content(website)
  
  ratings <- website |> 
    rvest::html_elements(".rating-histogram-bar") |> 
    rvest::html_text() |> 
    readr::parse_number() |> 
    as.integer()
  
  names(ratings) <- 1:10
  ratings[is.na(ratings)] <- 0L
  
  out <- tibble::enframe(ratings, name = "rating", value = "n")
  out$rating <- as.integer(out$rating)
  out$data_film_slug <- path
  
  return(out)
  
}

# download ----------------------------------------------------------------

outfolder <- "download/movie-histograms/"
if (!dir.exists(outfolder)) dir.create(outfolder)

df <- read_rds("download/metadata.rds") |> 
  distinct(data_film_slug, data_film_id) |> 
  mutate_all(as.character) 

done <- str_replace(dir(outfolder), "\\.rds$", "")
left <- setdiff(df$data_film_id, done)

pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)\n", total = length(left))

while (length(left) > 0) {
  
  x <- sample(left, 1)
  
  sub <- filter(df, data_film_id == x)
  message(sub[["data_film_slug"]])
  output <- try(scraper_histogram(sub[["data_film_slug"]]))
  
  readr::write_rds(output, glue("{outfolder}{x}.rds"), compress = "gz")
  left <- left[-which(left %in% x)] ## int. subset
  
  pb$tick()
  Sys.sleep(runif(1, 1, 2))
  
}


# debug -------------------------------------------------------------------

# library(furrr)
# plan(multisession, workers = parallel::detectCores() - 1L)

files <- dir(outfolder, full.names = TRUE)
output <- map(files, readr::read_rds)

error_index <- output |> 
  map_lgl(\(x) any(class(x) == "try-error")) 

message("errors: ", sum(error_index))

# examples of errors:
#
# "/film/the-twilight-saga-collection/"                      
# "/film/journey-to-the-oscars/"                             
# "/film/wicked-2024/"                                       
# "/film/escorts/"                                           
# "/film/into-the-night-with-harmony-korine-and-gaspar-noe/" 
# "/film/cooking-with-bill-damasu-950/"                      
# "/film/lima/"                                              
# "/film/the-moors-murders/"                                 
# "/film/resident-evil-collection/"                          
# "/film/jackass-collection/"                                
# "/film/the-super-mario-bros-movie/"                        
# "/film/fuck-the-devil-2-return-of-the-fucker/"             
# "/film/the-hannibal-lecter-collection-1/"                  
# "/film/mad-max-collection/"                                
# "/film/final-destination-collection/"                      
# "/film/transformers-collection/"                           
# "/film/superman-collection/"                               
# "/film/alien-collection/"                                  
# "/film/friendship-bracelet/"                               
# "/film/ctrlaltdel/"                                        
# "/film/keep-out/"                                          
# "/film/scary-movie-collection-1/"                          
# "/film/spider-man-across-the-spider-verse/"                
# "/film/tomb-raider-collection/"                            
# "/film/the-matrix-collection/"                             
# "/film/evil-dead-collection/"                              
# "/film/the-mummy-brendan-fraser-series/"                   
# "/film/die-hard-collection/"                               
# "/film/harry-potter-collection/"                           
# "/film/austin-powers-collection/"                          
# "/film/saw-collection/"                                    
# "/film/the-chronicles-of-narnia-collection/"               
# "/film/predator-collection/"                               
# "/film/oceans-collection/"                                 
# "/film/pirates-of-the-caribbean-collection/"               
# "/film/the-godfather-collection/"                          
# "/film/indiana-jones-collection/"                          
# "/film/band-battle/"                                       
# "/film/scared-stiff-oregon/"                               
# "/film/dune-part-two/"                                     
# "/film/grey-cloud-island-minnesota/"                       
# "/film/kung-fu-panda-collection/"                          
# "/film/ai-mama/"                                           
# "/film/dogwood-azalea-missouri/"                           
# "/film/mufasa-the-lion-king/"                              
# "/film/even-the-best/"                                     
# "/film/hes-dead-so-am-i/"                                  
# "/film/spy-kids-collection/"                               
# "/film/cars-collection/"                                   
# "/film/the-gremlins-collection/"                           
# "/film/halloween-collection-2/"                            
# "/film/the-red-riding-trilogy/"                            
# "/film/joker-folie-a-deux/"                                
# "/film/peppergrass/"                                       
# "/film/venom-3/"                                           
# "/film/squirrels-to-the-nuts/"                             
# "/film/what-is-a-woman-2022/"                              
# "/film/the-batman-deleted-arkham-scene/"                   
# "/film/king-on-screen/"                                    
# "/film/archangel/"                                         
# "/film/a-sinister-halloween-scary-opposites-solar-special/"
# "/film/the-hobbit-trilogy/"                                
# "/film/pixar-short-films-collection/" 

# A lot of them are from collections!!

if (sum(error_index) > 0) {
  file.remove(files[error_index])
  output <- output[!error_index]
}


# organize ----------------------------------------------------------------

full_ratings <- bind_rows(output)
movie_levels <- unique(df$data_film_slug)

full_ratings <- left_join(full_ratings, df) |> 
  mutate(data_film_id = as.integer(data_film_id)) |> 
  mutate(data_film_slug = factor(data_film_slug, levels = movie_levels))

readr::write_rds(full_ratings, "download/movie_ratings.rds", compress = "gz")


