library(tidyverse)
setwd('~/Documents/assoRted/USGS_STZ_working_group/scripts')
source('functions.R')
set.seed(12)


# dummy data for individual sieves. 

dat <- data.frame(
  matrix(data = c(
    sample(0:1, size = 36, replace = T, prob = c(0.25, 0.75)), 
    sample(0:1, size = 36, replace = T, prob = c(0.2, 0.80)), 
    sample(0:1, size = 36, replace = T, prob = c(0.15, 0.85))
    ),
    ncol = 3)
  )

focus <- apply(dat, MARGIN = 1, min)
dat <- cbind(dat, focus) |>
  setNames(c('landcover', 'nlcd', 'seeded', 'focus'))

rm(focus)

# combine our dummy data with some dummy coordinates, which will become points
# in the x and y direction. We will buffer out the points to turn them into
# raster tiles. 
sf_lyrs <- cbind(
  dat,
  x = rep(1:6, each = 6), 
  y = rep(1:6) # automatically recycles  
) |>
  data.frame() |>
  sf::st_as_sf(coords = c('x', 'y'), remove = FALSE) |>
  sf::st_buffer(0.5, endCapStyle = 'SQUARE') |>
  dplyr::mutate(across(landcover:focus, ~ factor(.x)))

rm(dat)

# create a specified palette for our classified layers s
cols <- c("0" = 'black', "1" = 'white')

# create the plot using the rotate_sf function. 
r_plot <- ggplot() + 
  
  geom_sf(data = rotate_sf(sf_lyrs), # bottom layer
          aes(fill = focus)) +
  
  geom_sf(data = rotate_sf(sf_lyrs, y_add = 7), # lower layer
    aes(fill = seeded), alpha = .7) + 
  
  geom_sf(data = rotate_sf(sf_lyrs, y_add = 14), # middle layer
    aes(fill = nlcd),  alpha = .7) + 

  geom_sf(data = rotate_sf(sf_lyrs, y_add = 21), # uppermost layer, y_add relative to base
    aes(fill = landcover),  alpha = .7) +
  
  scale_fill_manual(values = cols, name = 'Suitable:') + 
  theme_void() + 
  theme(
    legend.position="bottom", 
    plot.title = element_text(hjust = 0.5)) + 
  labs(title = 'Land use `Sieve`') + 
  annotate("text", x = 18, y = -1.5, size=5, color="gray15", label = "Suitable Areas") +

  annotate("text", x = 18, y = 5, size = 3.75, color="gray15", # lowest sieve
           label = "Historic Seeding") + 
  annotate("text", x = 18, y = 12, size = 3.75, color="gray15", # middle sieve
           label = "NLCD") + 
  annotate("text", x = 18, y = 19, size = 3.75, color="gray15", # upper sieve
           label = "Land Cover") 
  
r_plot
ggsave('../figures/EnvironmentalSieve.png', r_plot, bg = 'transparent', 
       dpi = 300, units = 'in', height = 5, width = 3.5)


rm(r_plot, sf_lyrs)

################################################################################
#    now we prepare another data set to exemplify ensembling a model 

# use 49 data sets. 

dat <- matrix(nrow = 7, ncol = 7)

dat[,1:3] <- 1
dat[1:2,4:7] <- 2
dat[3:4,4:7] <- 3
dat[5:7,4:7] <- 4

dat
