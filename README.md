
<!-- README.md is generated from README.Rmd. Please edit that file -->

# movies-dataset

<!-- badges: start -->
<!-- badges: end -->

The `movies-dataset` repository contains data scraped from
<https://letterboxd.com/>

The main datasets includes some `metadata.rds` that was later used to
download individual level ratings for some movies, contained in
`ratings_subset.rds`.

The number of individual level ratings is capped at 5000 users, so we
remove all movies that have 5000 users under the presumption that they
are incomplete. This means that we drop all popular movies and end up
using *niche movies* instead.

``` r
library(tidyverse)

metadata <- read_rds("download/metadata.rds")
ratings <- read_rds("download/ratings_subset.rds")

glimpse(metadata)
#> Rows: 216,000
#> Columns: 6
#> $ data_film_slug         <chr> "/film/parasite-2019/", "/film/joker-2019/", "/…
#> $ data_film_id           <chr> "426406", "406775", "51568", "348914", "475370"…
#> $ data_cache_busting_key <chr> "957fa8f5", "382688a5", "f1d3d7f5", "c51f1bf5",…
#> $ data_average_rating    <dbl> 4.58, 3.89, 4.29, 4.07, 4.05, 4.29, 3.85, 4.19,…
#> $ alt                    <chr> "Parasite", "Joker", "Fight Club", "The Batman"…
#> $ src                    <chr> "https://s.ltrbxd.com/static/img/empty-poster-7…
glimpse(ratings)
#> Rows: 570,191
#> Columns: 6
#> $ user_href      <chr> "/demidelanuit/", "/missamorydahl/", "/domkid/", "/vvas…
#> $ user           <chr> "DemiDeLaNuit", "missamorydahl", "Domkid", "vvasteras",…
#> $ stars          <chr> "★★★★★", "★★★★★", "★★★★★", "★★★★½", "★★★★½", "★★★★½", "…
#> $ data_film_slug <chr> "/film/princesa/", "/film/princesa/", "/film/princesa/"…
#> $ data_film_id   <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ rating         <dbl> 5.0, 5.0, 5.0, 4.5, 4.5, 4.5, 4.0, 4.0, 4.0, 4.0, 4.0, …
```

Number of movies in the subset:

``` r
length(unique(ratings$data_film_slug))
#> [1] 2884
```

Number of users in the subset:

``` r
length(unique(ratings$user_href))
#> [1] 225742
```

Example:

``` r
ratings |> 
  nest(!data_film_slug) |> 
  slice_sample(n = 15) |> 
  unnest(data) |> 
  left_join(select(metadata, data_film_slug, alt), by = "data_film_slug") |> 
  ggplot(aes(rating)) + 
  geom_bar(width = 1/5) + 
  facet_wrap(~alt, ncol = 3)
#> Warning: All elements of `...` must be named.
#> Did you want `data = !data_film_slug`?
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

------------------------------------------------------------------------

*Maybe we can fix the user cap by scrapping individual users instead of
individual movies?*

*We should also add other types of metadata per movie, like date and
genre.*

*Note, there’s currently an API in beta. We should consider applying for
this so that the data becomes “legal.”*

-   <https://letterboxd.com/api-beta/>
-   <https://api-docs.letterboxd.com/>
