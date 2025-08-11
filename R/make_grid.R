NULL
#' Make Grid / Create Grid 
#'
#' @param x  elevation / terrain digital model (DTM) , a SpatRast object 
#' @param fact,... furhrer agruments for \code{\link[terra]{aggregate}}
#'  
#'
#' @importFrom terra aggregate vect
#' @importFrom sf st_polygon st_sf st_sfc
#'
#'
#'
#' @seealso \code{\link[terra]{aggregate}}
#'
#' @export
#' 
#' @examples
#' 
#' elev <- rast(system.file("ex/elev_vinschgau.tif",package="terra"))
#'
#' out <- make_grid(elev)
#' 
#' plot(elev)
#' plot(out,add=TRUE)
#' 

make_grid <- function(x,fact=10,
                      ...) {
  out <- aggregate(x,fact=fact,...)
  
  
  out <- out*0
  dx <- xres(out)
  dy <- yres(out)
  grid_points <- as.data.frame(out,xy=TRUE,na.rm=FALSE)
  
  # 
  # x_range <- seq(0, 10, by = 1)
  # y_range <- seq(0, 5, by = 1)
  # 
  # 
  # cell_size <- 1  # lato del quadrato
  # 
  # # Crea la griglia di punti
  # grid_points <- expand.grid(x = x_range, y = y_range)
  
  # Funzione per creare un quadrato attorno a un punto
  create_square <- function(x, y, dx,dy) {
    x <- x-dx/2 ## move from center to (0,0) corner.
    y <- y-dy/2
    st_polygon(list(matrix(c(
      x, y,
      x + dx, y,
      x + dx, y + dy,
      x, y + dy,
      x, y
    ), ncol = 2, byrow = TRUE)))
  }
  
  # Crea i poligoni
  polygons <- mapply(create_square, grid_points$x, grid_points$y, MoreArgs = list(dx=dx,dy=dy), SIMPLIFY = FALSE)
  
  # Crea un oggetto sf
  grid_sf <- st_sf(geometry = st_sfc(polygons))
  
  # Visualizza la griglia
 ### plot(grid_sf$geometry, col = NA, border = "blue", main = "Griglia di poligoni")
  
  out <- vect(grid_sf)
  
  
  
  
  ####
  return(out)
  
}

