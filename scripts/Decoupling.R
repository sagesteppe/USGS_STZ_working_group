library(tidyverse)
library(sf)
library(stplanr)
library(stars)
library(terra)
library(gstat)
library(ggpattern)
library(ggnewscale)
library(ggarrow)
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
  x = c(sample(45:50, 8, replace = TRUE), 62), 
  y = c(sample(63:70, 8), 58)
) |>
  st_as_sf(coords = c('x', 'y'))

dry_grp <- data.frame(
  Group = 'Dry',
  Status = c(rep('Core', 8), 'Decoupled'),
  x = c(sample(33:42, 8), 22), 
  y = c(sample(18:28, 8), 25)
  ) |>
  st_as_sf(coords = c('x', 'y'))

med_grp <- data.frame(
  Group = 'Med',
  Status = c(rep('Core', 8), 'Decoupled', 'Decoupled'),
  x = c(sample(50:60, 8), 77, 33), 
  y = c(sample(30:40, 8), 23, 48)
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


values = c("Dry" = "#A40E4C", "Med" = "#B5C2B7", "Mesic" = "#124E78")
shapes = c('Core' = 21, 'Decoupled' = 23)
labs <- c('Driest', 'Intermediate', 'Wettest')

Wettest_label <- c("The wettest group's decoupled pop. has more\n soil moisture available; resulting in a\n lower range of precip and temp.")
Intermediate_label <- c('The intermediate group has two decoupled\npops; increasing the predicted range of\ntemp and precip in both directions.')
Driest_label <- c('The driest group has less\n soil moisture available\n and extends to areas\nwith higher precip')

wet_lab_bg <- data.frame(
  x = c(-14, 36, 30, -8, -14), 
  y = c(85, 85, 75, 75, 85)
) |>
  st_as_sf(coords = c('x', 'y')) |>
  st_union() |>
  st_convex_hull() |>
  st_buffer(1)

int_lab_bg <- data.frame(
  x = c(66, 113, 108, 71, 66), 
  y = c(75,  75,  65, 65, 75)
)|>
  st_as_sf(coords = c('x', 'y')) |>
  st_union() |>
  st_convex_hull() |>
  st_buffer(1)

dri_lab_bg <- data.frame(
  x = c(-4.5, 25, 17.5, 3.5, -4.5), 
  y = c(66, 66, 53, 53, 66)
)|>
  st_as_sf(coords = c('x', 'y')) |>
  st_union() |>
  st_convex_hull() |>
  st_buffer(1)


ggplot() + 
  
  # first draw the raster background, everything else will be superimposed on top 
  # of it
  geom_raster(data = int_bg, aes(x = x, y = y, fill = ID)) + 
  scale_fill_distiller(palette = "RdBu", guide = "none", direction = 1) + 
  new_scale_fill() +

  # now we add the grid lines across it, in the style of a USDA soil triangle
  geom_sf(data = lines, color = '#FFFFF0') + 
  
  # we add the border on top of the gridlines, because the edges are confluent
  # with our border and will draw over it, and that looks bad
  geom_sf(data = borders, aes(color = ID), lwd = 2) +
  scale_color_distiller(palette = "RdBu", guide = "none") + 
  new_scale_color() + 
  
  ############ finally we start to add data to the plot.  ##################
  
  # these are the core CCA areas, without the influence of a decoupled population
  geom_sf(data = core, aes(color = Group, fill = Group), lwd = 1.1, alpha = 0.3) +
  
  # add the areas where the climate is decoupled within a morphotype 
  # based seed zone
  geom_sf(data = decoupled, aes(color = Group), lwd = 1.1, fill = NA, lty = 1) + 
  
  # these are the points representing morphos from a CCA
  geom_sf(data = morphos, aes(shape = Status, fill = Group), size = 3) + 
  scale_shape_manual(values = shapes, name = "Population:")  + 
  
  scale_fill_manual(
    values = values, 
    labels = labs,
    name = 'STZ Group'
    ) + 
  scale_color_manual(
    values = values, 
    labels = labs,
    name = 'STZ Group'
    ) + 
  
  # labels for easy orientation 
  geom_label(aes(x = 20, y = 45, label='Temperature'), size = 5, angle = 60, fill = '#FFFFF0') + 
  geom_text(aes(x = 8, y = 20, label='warmer'), angle = 60) + 
  geom_text(aes(x = 36, y = 68, label='cooler'), angle = 60) + 
  
  geom_label(aes(x = 80, y = 45, label='Precipitation'),  size = 5, angle = 300, fill = '#FFFFF0') + 
  geom_text(aes(x = 64, y = 68, label='more'), angle = 300) + 
  geom_text(aes(x = 92, y = 20, label='less'), angle = 300) +  
  
  geom_label(aes(x = 50, y = -5, label='Geomorphology'), size = 5, fill = '#FFFFF0') + 
  geom_text(aes(x = 23, y = -3, label='drier')) + 
  geom_text(aes(x = 78, y = -3, label='wetter')) + 

  # backdrop ellipses for the text boxes. 
  geom_sf(data = wet_lab_bg, fill = '#124E78', alpha = 0.4, color = '#124E78', lwd = 1.1) + 
  geom_sf(data = int_lab_bg, fill = '#B5C2B7', alpha = 0.4, color = '#B5C2B7', lwd = 1.1) + 
  geom_sf(data = dri_lab_bg, fill = '#A40E4C', alpha = 0.4, color = '#A40E4C', lwd = 1.1) + 
  
  # text boxes explaining the groups
  geom_text(aes(x = 10, y = 80, label = Wettest_label), size = 3) + 
  geom_text(aes(x = 90, y = 70, label = Intermediate_label), size = 3) + 
  geom_text(aes(x = 10.25, y = 60, label = Driest_label), size = 3) + 
  
  labs(
    title = 'Possible effects of climatic decoupling on STZ delineation', 
    caption = 'Each ellipsoid represents clusters of morphotypes, an STZ group, from a canonical\n correspondence analysis of traits measured in a common garden(s).') + 
  
  theme_void() + 
  xlim(-10, 110) + 
  theme(
    legend.position = 'bottom', 
    legend.box = "vertical",
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 0.5)
    ) +
  guides(
    shape = guide_legend(title = "Population:", nrow = 1, order = 1),
    color = guide_legend(nrow = 1)
  )



