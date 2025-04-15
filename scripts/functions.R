#' Smooth polygons for ENM patches and return polygons for seed zones
#' @param x seed zone dummy data created by reed, which actually end up becoming
#' the suitable habitat polygons, while the space between them is filled by the
#' # nearest neighboring polygon to become seed zones. 
prep_dat <- function(x){
  
  # smooth the hard vertices from hand drawn polygons
  x <- smoothr::smooth(x, method = "chaikin")
  x <- st_buffer(x, 5000)

  # sample points across the domain, classify as closest polygon. 
  # these will be the surfaces for the Seed Zones in both current and forecast time 
  pts <- st_sample(
  st_bbox(
    st_union(x)
    ),
  7500
  ) |>
  st_as_sf()
  
  # now just quickly group the classified points, convert them into a single
  # object and draw a concave hull around each object. Then buffer the hulls to 
  # cover slivers between them, and take the difference across hulls so they don't overlap. 
  sz_big <- bind_cols(
    Poly = x[['Poly']][sf::st_nearest_feature(pts, x)],
    geom = pts
  ) |> 
    st_as_sf() |>
    dplyr::group_by(Poly) |>
    dplyr::summarise() |>
    st_cast("MULTIPOINT") |>
    st_concave_hull(ratio = 0.5) |>
    st_buffer(10000) |>
    st_difference()

  return(
    list(
      SZ = sz_big, 
      enm = x
    )
  )

}

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