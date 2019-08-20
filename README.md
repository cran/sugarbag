
<!-- README.md is generated from this README.Rmd. Please edit this file -->

# sugarbag <img src='man/figures/logo.png' align="right" height="138.5" />

[![Travis-CI Build
Status](https://travis-ci.org/srkobakian/sugarbag.svg?branch=master)](https://travis-ci.org/srkobakian/sugarbag)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/sugarbag)](https://cran.r-project.org/package=sugarbag)
[![Downloads](http://cranlogs.r-pkg.org/badges/sugarbag?color=brightgreen)](https://cran.r-project.org/package=sugarbag)

The **sugarbag** package creates tesselated hexagon maps for visualising
geo-spatial data. Hexagons of equal size are positioned to best preserve
relationships between individual areas and the closest focal point, and
minimise distance from their actual location. This method allows all
regions to be compared on the same visual scale, and provides an
alternative to cartograms.

Maps containing regions with a few small and densely populated areas are
extremely distorted in cartograms. An example of this is a population
cartogram of Australia, which distorts the map into an unrecognisable
shape. The technique implemented in this package is particularly useful
for these regions.

## Installation

You can install the released version of sugarbag from
[CRAN](https://CRAN.R-project.org) with:

``` r
# install.packages("sugarbag")
```

You can install the development version from GitHub using:

``` r
# install.packages("remotes")
# remotes::install_github("srkobakian/sugarbag")
```

## Getting started

Refer to pkgdown site: <https://srkobakian.github.io/sugarbag/>

We will use the `eechidna` package for Australian electorates and
election data.

``` r
library(sugarbag)
library(eechidna)
```

The create\_centroids function may have been used to find centroid for
each area. Instead we use the eechidna package. This has been derived
already and is stored in nat\_data16.

``` r
# Find the longitude and latitude centroid for each region or area
centroids <- nat_data16 %>% select(elect_div, longitude = long_c, latitude = lat_c)
```

The `sugarbag` package operates by creating a grid of possible hexagons
to allocate electorates. The buffer extends the grid beyond the
geographical space, this is especially useful for densely populated
coastal areas or cities, such as Brisbane and Sydney.

``` r
# Create a grid of hexagons to allocate centroids
grid <- create_grid(centroids = centroids, hex_size = 0.9, buffer_dist = 5)
```

The two key pieces, the centroids and the grid can be used by the
`allocate` function.

``` r
# Allocate the centroids to the hexagon grid
# We have the same amount of rows, as individual regions
hex_allocated <- allocate(centroids = centroids,
  sf_id = "elect_div",
  hex_grid = grid,
  hex_size = 0.9, # same size used in create_grid
  hex_filter = 10,
  focal_points = capital_cities,
  width = 30, verbose = TRUE) # same column used in create_centroids
```

New England was located quite far from the focal point. Due to this it
was placed in an unfortunate position. We can find where we would like
to place it by finding neighbouring electorates and looking at their
hexagon locations.

``` r
# Move hexagons if not placed in desirable hexagon tile
hex_allocated <- hex_allocated %>% mutate(hex_long = ifelse(elect_div == "NEW ENGLAND", 150.6779 + 0.45, hex_long),
  hex_lat = ifelse(elect_div == "NEW ENGLAND", -30.31636 +0.9, hex_lat))
```

The function `fortify_hexagon` assists in plotting. We now have 6 points
per region, one for each point of a hexagon. Connecting these points
will allow actual hexagons to be plotted.

The additional demographic information or data can now be added. This
can be used to allow plots to be coloured by region.

For animations to move between geography and hexagons the `sf_id` must
match, there also needs to be an identifier to separate the states to
animate between for `gganimate`.

``` r
h1 <- hex_allocated %>%
  fortify_hexagon(hex_size = 0.9, sf_id = "elect_div") %>%
  left_join(nat_data16, by = "elect_div") %>% mutate(poly_type = "hex")

# When plotting, the polygons are needed, rather than single centroid points.
# We can apply the same function as above, to find 6 points per centroid
p1 <- nat_data16 %>% 
  select(elect_div, hex_long = long_c, hex_lat = lat_c) %>%
  fortify_hexagon(hex_size = 0.1, sf_id = "elect_div") %>%
  left_join(nat_data16, by = "elect_div") %>%
  mutate(poly_type = "geo")

hex_anim <- h1 %>% 
  select(state, elect_div, long, lat, id = id.x, poly_type, focal_dist) %>% 
  left_join(p1 %>% distinct(elect_div, polygon), by = "elect_div")
#> Warning: Trying to compute distinct() for variables not found in the data:
#> - `polygon`
#> This is an error, but only a warning is raised for compatibility reasons.
#> The following variables will be used:
#> - elect_div
geo_anim <- p1 %>% 
  select(elect_div, long, lat, poly_type)
anim_aus <- bind_rows(hex_anim, geo_anim)

# Join election data from 2016
anim_aus <- anim_aus %>% 
  left_join(fp16 %>% filter(Elected == "Y") %>% select(elect_div = DivisionNm, PartyAb, PartyNm, Percent), by = "elect_div")
```

Here we show the two sets of areas they can be plotted separately using
`geom_facet`.

``` r
auscolours <- c(
  "ALP" = "#DE3533",
  "LNP" = "#080CAB",
  "KAP" = "#b50204",
  "GRN" = "#10C25B",
  "XEN" = "#ff6300",
  "LNP" = "#0047AB",
  "IND" = "#307560")

anim_aus %>% 
  ggplot(aes(x=long, y=lat, group = interaction(elect_div))) +
  geom_polygon(aes(x=long, y=lat, group = group),fill = "grey", alpha = 0.3, data= nat_map16) +
  geom_polygon(aes(fill = PartyAb)) +
  coord_equal() + 
  theme_void() + 
  scale_fill_manual(values = auscolours) +
  facet_wrap(~poly_type)
```

<img src="man/figures/README-plot_facet-1.png" width="100%" />

We can move between the two plots using the `transition_states` function
from the `gganimate` package.

``` r
library(gganimate)
anim <- anim_aus %>% 
  ggplot(aes(x=long, y=lat, group = interaction(elect_div))) +
  geom_polygon(aes(x=long, y=lat, group = group),fill = "grey", alpha = 0.3, data= nat_map16) +
  geom_polygon(aes(fill = PartyAb)) + 
  coord_equal() + 
  theme_void() +
  scale_fill_manual(values = auscolours) +
  guides(fill = guide_legend(title = NULL)) + 
  theme(legend.position = "bottom") +
  transition_states(poly_type)
animate(anim, duration = 6, nframes = 60)
```

<img src="man/figures/README-animated-1.gif" width="100%" />
