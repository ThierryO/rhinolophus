#' get contours as SpatialPolygons
#' @importFrom raster rasterToContour
#' @importFrom sp coordinates Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame
#' @export
#' @param x a raster
#' @param levels a vector with the required levels
get_contours <- function(x, levels){
  contour_line <- rasterToContour(x, levels = levels)
  contour_closed <- lapply(
    coordinates(contour_line),
    function(x){
      sapply(
        x,
        function(y){
          identical(y[1, ], y[nrow(y), ])
        }
      )
    }
  )
  contour_line$level <- as.numeric(levels(contour_line$level))[
    contour_line$level
  ]
  contours <- lapply(
    seq_along(contour_line),
    function(i)
      {
        closed_lines <- coordinates(contour_line)[[i]][contour_closed[[i]]]
        if (length(closed_lines) == 0) {
          NULL
        } else {
          polygons <- lapply(seq_along(closed_lines), function(j){
            polygon <- Polygon(closed_lines[[j]])
            Polygons(list(polygon), ID = sprintf("%06i%06i", j, i))
          })
          polygons <- SpatialPolygons(polygons)
          dataset <- data.frame(
            ID = sprintf("%06i%06i", seq_along(closed_lines), i),
            ContourAmplitude = contour_line$level[i],
            stringsAsFactors = FALSE
          )
          rownames(dataset) <- dataset$ID
          SpatialPolygonsDataFrame(Sr = polygons, data = dataset)
        }
    }
  )
  contours[!sapply(contours, is.null)] %>%
    do.call(what = rbind)
}
