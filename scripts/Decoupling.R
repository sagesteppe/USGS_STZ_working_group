library(tidyverse)
library(sf)
library(stplanr)
library(stars)
library(terra)
library(gstat)
setwd('~/Documents/assoRted/USGS_STZ_working_group/scripts')
source('functions.R')

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

rm(start, end)


# create a grid for the background to color the evapo demand/availability
pt_intrsct <- do.call(bind_rows,
    list(
      st_intersection(h_lines, v45_lines),
      st_intersection(h_lines, v135_lines),
      st_intersection(v45_lines, v135_lines)
  )
) |>
  distinct()

pt_intrsct <- mutate(pt_intrsct,
       x = st_coordinates(pt_intrsct)[,1] * (1/2), 
       y = st_coordinates(pt_intrsct)[,2]) |>
  rowwise() |>
  mutate(mesic = sum(x, y), .before = geometry) |>
  select(mesic)

grd <- st_bbox(triangle) |>
  stars::st_as_stars(100) |>
  st_crop(triangle) 
int_bg <- gstat::idw(mesic~1, pt_intrsct, grd)
int_bg <- terra::rast(int_bg)
int_bg <- focal(int_bg, w = 21, fun = mean, na.rm = TRUE)
names(int_bg) <- c('ID', 'other')

int_bg <- mask(int_bg, vect(triangle))
int_bg <- terra::as.data.frame(int_bg, xy = TRUE)

rm(grd)

ggplot() + 
  geom_raster(data = int_bg, aes(x = x, y = y, fill = ID)) + 
  scale_fill_distiller(palette = "RdBu", guide = "none", direction = 1) + 
  # first we draw the triangle 
  geom_sf(data = h_lines, color = 'grey80') + 
  geom_sf(data = v45_lines, color = 'grey80') + 
  geom_sf(data = v135_lines, color = 'grey80') + 
  geom_sf(data = ob, aes(color = ID), lwd = 2) +
  
  scale_color_distiller(palette = "RdBu", guide = "none") + 
  
  # labels for easy orientation 
  geom_label(aes(x = 20, y = 45, label='Temperature'), angle = 60) + 
  geom_text(aes(x = 5, y = 15, label='warmer'), angle = 60) + 
  geom_text(aes(x = 40, y = 75, label='cooler'), angle = 60) + 
  
  geom_label(aes(x = 80, y = 45, label='Precipitation'), angle = 300) + 
  geom_text(aes(x = 60, y = 75, label='more'), angle = 300) + 
  geom_text(aes(x = 95, y = 15, label='less'), angle = 300) +  
  
  geom_label(aes(x = 50, y = -5, label='Geomorphology')) + 
  geom_text(aes(x = 15, y = -3, label='drier')) + 
  geom_text(aes(x = 85, y = -3, label='wetter')) + 
  
  theme(aspect.ratio = 1) + 
  theme_void()


