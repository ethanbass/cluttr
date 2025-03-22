#' Map trait
#'
#' Convenience function to plot traits or other data onto a map.
#'
#' Relies on \code{ggmap}.
#'
#' @importFrom ggplot2 geom_point .data xlab ylab aes
#' @param map A \code{ggmap} object created by \code{\link[ggmap:get_map]{get_map}}.
#' @param data A \code{data.frame} containing latitude and longitude coordinates
#' along with trait data to plot on the map.
#' @param trait A string specifying the column name in \code{data} containing
#' the values to map.
#' @param lat A string specifying the column name in \code{data}
#' containing the latitudes.
#' @param lon A string specifying the column name in \code{data}
#' containing the longitudes.
#' @param alpha Transparency of dots. Argument to
#' \code{\link[ggplot2:geom_point]{geom_point}}. Defaults to \code{0.8}.
#' @param ... Additional arguments to \code{\link[ggplot2:geom_point]{geom_point}}.
#' @export
map_trait <- function(map, data, trait, lat, lon, alpha = 0.8, ...){
  ggmap::ggmap(map)  +
    ggplot2::geom_point(aes(x = .data[[lat]], y = .data[[lon]],
                            color = .data[[trait]]),
                        size = 2, na.rm = TRUE, data = data[!is.na(get(trait))],
                        alpha = alpha, ...) +
    ggplot2::xlab("") + ggplot2::ylab("")
}
