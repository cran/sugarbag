#' Create a data frame of longitude and latitude centroids of each polygon.
#' 
#'
#' @param shp_sf an sf object, a data set with a simple feature list column
#' @param sf_id a string to indicate the column to identify individual polygons
#' @param largest logical; for `st_centroid`: if `TRUE`, return centroid of the
#' largest subpolygon of a `MULTIPOLYGON` rather than the whole `MULTIPOLYGON`
#' @param verbose a boolean to indicate whether to show function progress
#'
#' @return a tibble containing longitude and latitude
#' @export
#'
#' @examples
#' centroids <- create_centroids(tas_lga, "lga_code_2016")

create_centroids <- function(shp_sf, sf_id, largest = TRUE, verbose = FALSE) {
  if (verbose) {
    message("Deriving polygon centroids")
  }
  
  # create a set of id values
  ids <- shp_sf %>%
    sf::st_as_sf() %>%
    sf::st_set_geometry(NULL) %>%
    mutate(!!sf_id := !!sym(sf_id)) %>%
    dplyr::select(!!sf_id)

  # derive the central point of each of the polygons
  centroids <- sf::st_geometry(shp_sf) %>%
    sf::st_centroid(., of_largest_polygon = largest) %>%
    sf::st_coordinates() %>%
    tibble::as_tibble()

  # return with id column as specified
  if (is.null(sf_id)) {
    centroids <- centroids %>%
      mutate(sf_id = as.character(dplyr::row_number())) %>%
      dplyr::select(sf_id, longitude = X, latitude = Y)
  } else {
    centroids <- bind_cols(ids, centroids %>%
      dplyr::select(longitude = X, latitude = Y))
  }

  # remove empty geometries
  centroids <- centroids %>%
    filter(!is.na(longitude)) %>%
    filter(!is.na(latitude))
  
  return((centroids))
}
