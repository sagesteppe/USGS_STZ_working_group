library(tidyverse)


#' Rotate simple features for 3D layers
#' @description Rotates a simple features layer using a shear matrix transformation on the 
#' \code{geometry} column. This can get nice for visualization and works with
#' points, lines and polygons. Code by Stefan Jünger Denis Cohen, perhaps based upon work by
#' spacedman. 
#'
#' @param data an object of class \code{sf}
#' @param x_add integer; x value to move geometry in space
#' @param y_add integer; x value to move geometry in space
#'
#' @importFrom magrittr %>%

rotate_sf <- function(data, x_add = 0, y_add = 0) {
  
  shear_matrix <- function (x) { 
    matrix(c(2, 1.2, 0, 1), 2, 2) 
  }
  
  rotate_matrix <- function(x) { 
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
  }
  
  data %>% 
    dplyr::mutate(
      geometry = 
        .$geometry * shear_matrix() * rotate_matrix(pi / 20) + c(x_add, y_add)
    )
}


# create dummy data, here each value will indicate a layer classification from 
# 1 to 4
v <- rep(1:4, each = 9)

v2 <- v 
v2[10] <- 1 ; v2[c(7,9)] <- 2  ; v2[18] <- 3  ; v2[c(26,27)] <- 4 

v3 <- v
v3[c(11,12)] <- 1  ; v3[9] <- 2  ; v3[c(13,15)] <- 3  ; v3[c(24,26,27)] <- 4 

v[c(9, 18)] <- 1; v[7] <- 2 ; v[29] <- 4

# combined our dummy data with some dummy coordinates, which will become points
# in the x and y direction. We will buffer out the points to turn them into
# raster tiles. 
sf_lyrs <- cbind(
  'lyr1' = v,  'lyr2' = v2, 'lyr3' = v3,
  x = rep(1:6, each = 6), 
  y = rep(1:6) # automatically recycles  
) |>
  data.frame() |>
  sf::st_as_sf( coords = c('x', 'y'), remove = FALSE) |>
  sf::st_buffer(0.5, endCapStyle = 'SQUARE') |>
  dplyr::mutate(across(lyr1:lyr3, ~ factor(.x)))

rm(v, v2, v3)
# Now we can create a final raster from our example of three different classification 
# runs 

sf_sum <- sf_lyrs |>
  pivot_longer(lyr1:lyr3) |>
  group_by(x, y) |>
  add_count(value) |>
  group_by(x, y) |> 
  mutate(ID = cur_group_id(), .before  = geometry)

# identify the cells which have a plurality of consensus
consen <- filter(sf_sum, n >= 2) # all but one cell's have 2/3 or 3/3 agreement

no_consen <- filter(sf_sum, ! ID %in% consen$ID) |>
  # select the value from our reference layer, we will say that layer 2 is that. 
  filter(name == 'lyr2')

output_r <- bind_rows(consen, no_consen) |>
  arrange(ID) |>
  select(lyr_consensus = value)

rm(consen, no_consen, sf_sum)

# create a specified palette for our classified layers s
cols <- c("1" = '#007991', "2" = '#E9D985', "3" = '#EF476F', "4" = '#BCD8C1')

# create the plot using the rotate_sf function. 
r_plot <- ggplot() + 
  
  geom_sf(data = rotate_sf(output_r), # bottom layer
          aes(fill = lyr_consensus)) +
  
  geom_sf(data = rotate_sf(sf_lyrs, y_add = 7), # lower layer
    aes(fill = lyr1), alpha = .7) + 
  
  geom_sf(data = rotate_sf(sf_lyrs, y_add = 14), # middle layer
    aes(fill = lyr2),  alpha = .7) + 

  geom_sf(data = rotate_sf(sf_lyrs, y_add = 21), # uppermost layer, y_add relative to base
    aes(fill = lyr3),  alpha = .7) +
  
  scale_fill_manual(values = cols, name = 'Seed Zone:') + 
  theme_void() + 
  theme(
    legend.position="bottom", 
    plot.title = element_text(hjust = 0.5)) + 
  labs(title = 'Three model predictions\n from a classifier\nand a consensus layer') + 
  annotate("text", x = 18, y = -1.5, size=5, color="gray15", label = "consensus") +

  annotate("text", x = 18, y = 5, size=3.75, color="gray15",
           label = deparse(bquote('1'^'st'*' classification')), parse = TRUE) + 
  annotate("text", x = 18, y = 12, size=3.75, color="gray15", 
           label = deparse(bquote('2'^'nd'*' classification')), parse = TRUE) + 
  annotate("text", x = 18, y = 19, size=3.75, color="gray15", 
           label = deparse(bquote('3'^'rd'*' classification')), parse = TRUE) 
  
ggsave('../poster/Rasters.png', r_plot, bg = 'transparent', 
       dpi = 300, units = 'in', height = 5, width = 3.5)




################################################################################
# now we prepare another data set to exemplify ensembling a model 