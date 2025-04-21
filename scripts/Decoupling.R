library(tidyverse)
library(sf)
library(stplanr)

triangle <- st_as_sf(
  data.frame(
    x = c(0, 50, 100),
    y = c(0, 86.6, 0)
  ), coords = c('x', 'y')
) |>
  st_union() |>
  st_convex_hull() |> 
  st_cast('POLYGON') 

tri_borders <- triangle |>
  sf::st_cast('POINT') 

pieces <- lwgeom::st_split( st_cast(triangle, 'LINESTRING'), tri_borders)[[1]] 

border_segments <- function(x, y){
  
  tri_borders <- x |>
    sf::st_cast('POINT') 
  
  pieces <- lwgeom::st_split(sf::st_cast(x, 'LINESTRING'), tri_borders)[[1]] 

  breakup <- function(y){
    stplanr::line_segment1(l = y, n_segments = 50) |>
    sf::st_as_sf() |>
    dplyr::mutate(ID = 1:n())
  }
  pieces <- lapply(pieces, st_geometry)
  broken <- lapply(pieces, breakup)

}

bindr <- function(x, y){
  
  bind_rows(x, y) |>
    select(ID, geometry) |>
    group_by(ID) |>
    summarize(geometry = st_union(geometry)) |>
    st_cast('LINESTRING') |>
    st_intersection( triangle, 'within')  %>% 
    filter(st_is(., "LINESTRING"))
}

ob <- border_segments(triangle) |>
  bind_rows()


# h lines 
start <- data.frame(
  ID = 1:10,
  x = rep(0, 10), 
  y = seq(0, 86.6, length.out = 10)
) |>
  sf::st_as_sf(coords = c('x', 'y'))

end <- data.frame(
  ID = 1:10,
  x = rep(100, 10),
  y = seq(0, 86.6, length.out = 10)
) |>
  sf::st_as_sf(coords = c('x', 'y'))

h_lines <- bindr(start, end) 

# 45 degree lines 
start <- data.frame(
  ID = 1:10,
  x = seq(0, 100, length.out = 10), 
  y = rep(0, length.out = 10)
) |>
  sf::st_as_sf(coords = c('x', 'y'))

end <- data.frame(
  ID = 1:10,
  x = (seq(0, 100, length.out = 10)+50), 
  y = rep(86.6, 10)
) |>
  sf::st_as_sf(coords = c('x', 'y'))

v45_lines <- bindr(start, end)

# v 135 lines
start <- data.frame(
  ID = 1:10,
  x = seq(0, 100, length.out = 10), 
  y = rep(0, 10)
) |>
  sf::st_as_sf(coords = c('x', 'y'))

end <- data.frame(
  ID = 1:10,
  x = (seq(0, 100, length.out = 10)-50), 
  y = rep(86.6, 10)
) |>
  sf::st_as_sf(coords = c('x', 'y'))

v135_lines <- bindr(start, end)

ggplot() + 
  geom_sf(data = h_lines) + 
  geom_sf(data = v45_lines) + 
  geom_sf(data = v135_lines) + 
  geom_sf(data = ob, aes(color = factor(ID)))  +
  theme(aspect.ratio = 1) + 
  theme_void()
