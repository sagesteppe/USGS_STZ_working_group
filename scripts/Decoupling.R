library(tidyverse)
library(sf)
library(stplanr)
library(stars)
library(terra)
library(gstat)
library(ggpattern)
library(ggnewscale)
set.seed(27)
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

borders <- border_segments(triangle) |>
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

# aggregate the lines into a single data set

lines <- bind_rows(h_lines, v45_lines, v135_lines)
rm(h_lines, v45_lines, v135_lines)

## Create some points which represent the morphotypes for pops from the 
# common garden experiment 

mesic_grp <- data.frame(
  Group = 'Mesic',
  Status = c(rep('Core', 8), 'Decoupled'),
  x = c(sample(42:52, 8), 60), 
  y = c(sample(65:75, 8), 60)
) |>
  st_as_sf(coords = c('x', 'y'))

dry_grp <- data.frame(
  Group = 'Dry',
  Status = c(rep('Core', 8), 'Decoupled'),
  x = c(sample(35:45, 8), 24), 
  y = c(sample(20:30, 8), 37)
  ) |>
  st_as_sf(coords = c('x', 'y'))

med_grp <- data.frame(
  Group = 'Med',
  Status = c(rep('Core', 8), 'Decoupled', 'Decoupled'),
  x = c(sample(50:60, 8), 65, 45), 
  y = c(sample(35:45, 8), 30, 50)
) |>
  st_as_sf(coords = c('x', 'y'))

morphos <- bind_rows(mesic_grp, dry_grp, med_grp)

dry_ell <- ellipse_drw(dry_grp)
med_ell <- ellipse_drw(med_grp)
mes_ell <- ellipse_drw(mesic_grp)

core <- bind_rows(dry_ell$core, med_ell$core, mes_ell$core) |>
  st_as_sf() |>
  mutate(Group = c('Dry', 'Med', 'Mesic'))

decoupled <- bind_rows(dry_ell$diff, med_ell$diff, mes_ell$diff) |>
  st_as_sf() |>
  mutate(Group = c('Dry', 'Med', 'Mesic'))


# Group colors
#17183B # very dark blue
#A40E4C # dark red 
#39A2AE # middle group
values = c("Dry" = "#A40E4C", "Med" = "#39A2AE", "Mesic" = "#17183B")
shapes = c('Core' = 21, 'Decoupled' = 23)

ggplot() + 
  
  # first draw the raster background, everything else will be superimposed on top 
  # of it
  geom_raster(data = int_bg, aes(x = x, y = y, fill = ID)) + 
  scale_fill_distiller(palette = "RdBu", guide = "none", direction = 1) + 
  new_scale_fill() +

  # now we add the grid lines across it, in the style of a USDA soil triangle
  geom_sf(data = lines, color = 'grey95') + 
  
  # we add the border on top of the gridlines, because the edges are confluent
  # with our border and will draw over it, and that looks bad
  geom_sf(data = borders, aes(color = ID), lwd = 2) +
  scale_color_distiller(palette = "RdBu", guide = "none") + 
  new_scale_color() + 
  
  ############ finally we start to add data to the plot.  ##################
  

  # these are the core CCA areas, without the influence of a decoupled population
  geom_sf(data = core, aes(color = Group, fill = Group), lwd = 1.1, alpha = 0.2 ) +
  
  # add the areas where the climate is decoupled within a morphotype 
  # based seed zone
  geom_sf(data = decoupled, aes(color = Group), lwd = 1.1, fill = NA) + 
  
  # these are the points representing morphos from a CCA
  geom_sf(data = morphos, aes(shape = Status, fill = Group), size = 3) + 
  scale_shape_manual(values = shapes)  + 
  scale_fill_manual(values = values, labels = c('Driest', 'Intermediate', 'Wettest')) + 
  scale_color_manual(values = values, labels = c('Driest', 'Intermediate', 'Wettest')) + 
  
  # labels for easy orientation 
  geom_label(aes(x = 20, y = 45, label='Temperature'), size = 5, angle = 60) + 
  geom_text(aes(x = 5, y = 15, label='warmer'), angle = 60) + 
  geom_text(aes(x = 40, y = 75, label='cooler'), angle = 60) + 
  
  geom_label(aes(x = 80, y = 45, label='Precipitation'),  size = 5, angle = 300) + 
  geom_text(aes(x = 60, y = 75, label='more'), angle = 300) + 
  geom_text(aes(x = 95, y = 15, label='less'), angle = 300) +  
  
  geom_label(aes(x = 50, y = -5, label='Geomorphology'), size = 5,) + 
  geom_text(aes(x = 15, y = -3, label='drier')) + 
  geom_text(aes(x = 85, y = -3, label='wetter')) + 
  
  theme(aspect.ratio = 1) + 
  theme_void()


