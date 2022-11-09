## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----aus-anim, warning = FALSE, message = FALSE-------------------------------
library(sugarbag)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(sf)

## ----centroids----------------------------------------------------------------
# Find the longitude and latitude centroid for each region or area
centroids <- create_centroids(tas_sa2, sf_id = "sa2_name_2016")

## ----grid---------------------------------------------------------------------
grid <- create_grid(centroids = centroids, hex_size = 0.2, buffer_dist = 1.2)

## ----allocate-----------------------------------------------------------------
# Allocate the centroids to the hexagon grid
# We have the same amount of rows, as individual regions
hex_allocated <- allocate(centroids = centroids,
                          sf_id = "sa2_name_2016",
                          hex_grid = grid,
                          hex_size = 0.2, # same size used in create_grid
                          hex_filter = 10,
                          focal_points = capital_cities,
                          # same column used in create_centroids
                          width = 30, verbose = TRUE) 

## ----ggplot-------------------------------------------------------------------
hexagons <- hex_allocated %>%
  fortify_hexagon(hex_size = 0.2, sf_id = "sa2_name_2016") %>%
  left_join(., tas_sa2) %>% 
  mutate(poly_type = "hex")

polygons <- fortify_sfc(tas_sa2) %>% 
  mutate(poly_type = "geo")

ggplot(mapping = aes(fill = sa4_name_2016)) +
  geom_polygon(data = polygons, aes(x=long, lat, group = interaction(sa2_name_2016,polygon)), alpha = 0.4) +
  geom_polygon(data = hexagons, aes(x=long, lat, group = interaction(sa2_name_2016))) + scale_fill_viridis_d()

## ----prepareanimation---------------------------------------------------------
hexagon_points <- hexagons %>% 
  select(sa4_name_2016, sa2_name_2016, 	
sa2_name_2016, long, lat, poly_type) %>% 
  left_join(polygons %>% distinct(sa2_name_2016, polygon), by = "sa2_name_2016")
polygon_points <- polygons %>% 
  select(sa4_name_2016, sa2_name_2016, long, lat, polygon, poly_type)

animate_tas <- bind_rows(hexagon_points, polygon_points) %>%
  left_join(homeless, by = "sa2_name_2016")

animate_tas %>% 
  ggplot(aes(x=long, y=lat, group = interaction(polygon, sa2_name_2016))) +
  geom_polygon(aes(fill = sa4_name_2016)) +
  geom_polygon(data = polygon_points %>% select(-poly_type), fill = "grey40", alpha = 0.05) + 
  coord_equal() + 
  theme_void() + 
  guides(fill = guide_legend(title = NULL)) + 
  theme(legend.position = "bottom") +
  facet_wrap(~poly_type) + 
  scale_fill_viridis_d()

## ----createanimation, eval = FALSE--------------------------------------------
#  library(gganimate)
#  library(transformr)
#  
#  animation <- animate_tas %>%
#    ggplot(aes(x=long, y=lat, group = interaction(polygon, sa2_name_2016))) +
#    geom_polygon(aes(fill = sa4_name_2016)) +
#    geom_polygon(data = polygon_points %>% select(-poly_type), fill = "grey40", alpha = 0.05) +
#    coord_equal() +
#    theme_void() +
#    guides(fill = guide_legend(title = NULL)) +
#    theme(legend.position = "bottom") +
#    transition_states(states = poly_type) +
#    scale_fill_viridis_d()
#  
#  animated <- animate(animation, fps = 10, duration = 15,
#                      start_pause = 5, end_pause = 5, rewind = FALSE)
#  
#  anim_save(filename = "tasmania_animation.gif", animated)

