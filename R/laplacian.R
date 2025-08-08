NULL
#' Laplacian filter 
#'
#' @param x  elevation / terrain digital model (DTM) , a SpatRast object 
#' @param w window, default is dimension of \code{laplacian_filter}. See \code{\link[terra]{focal}}
#' @param laplacian_filter filter used to calculate the laplacian. See default
#' @param laplacian_diagonal_filer diagonal filter: \code{matrix(c(1,0,1,0,-4,0,1,0,1), nrow=3)/2}
#' @param laplacian_xy_filer laplacian xy filter:  \code{matrix(c(0,1,0,1,-4,1,0,1,0), nrow=3)}
#' @param diagonal_lambda weighting factor for diagonal filter against laplacian xy filter. Default is 0.5 . 
#' @param name output raster name
#' @param na.rm,... further arguments for \code{\link[terra]{focal}}
#'  
#'
#' @importFrom terra focal xres yres 
#' 
#' @seealso \code{\link[terra]{focal}},\code{\link[terra]{terrain}}
#'
#' @export
#' 
#' @examples
#' 
#' # example code
#' library(terra)
#' library(leaflet) 
#' 
#' 
#' elev1 <- array(NA,c(9,9))
#' elev2 <- elev1
#' dx <- 1
#' dy <- 1 
#' for (r in 1:nrow(elev1)) {
#'   y <- (r-5)*dx
#'   for (c in 1:ncol(elev1)) {
#'
#'     x <- (c-5)*dy
#'     elev1[r,c] <- 5*(x^2+y^2)
#'    elev2[r,c] <- 10+5*(abs(x))-0.001*y ### 5*(x^2+y^2)
#'   }
#' } 
#' 
#' 
#' ## Elevation Raster Maps
#' elev1 <- rast(elev1)
#' elev2 <- rast(elev2)
#' elev3 <- rast(system.file("ex/elev_vinschgau.tif",package="terra"))
#'
#' out1 <- laplacian(elev1)
#' out2 <- laplacian(elev2)
#' out3 <- laplacian(elev3)
#' 
#' plet(out3,tiles="Esri.WorldImagery")
#' plet(out3,tiles="OpenTopoMap")
#' 
#' 
#' 

laplacian <- function(x,w=dim(laplacian_filter),
                      laplacian_filter=laplacian_xy_filer*(1-diagonal_lambda)+laplacian_diagonal_filer*diagonal_lambda,
                      laplacian_diagonal_filer=matrix(c(1,0,1,0,-4,0,1,0,1), nrow=3)/2,
                      laplacian_xy_filer=matrix(c(0,1,0,1,-4,1,0,1,0), nrow=3),
                      diagonal_lambda=0.5,
                      na.rm=FALSE,name="laplacian",
                      ...) {
  
  xxres <- xres(x)
  yyres <- yres(x)
  
  if (xxres!=yyres) {
    
    msg <- sprintf("xres: %f yres: %f (not equal) result can be non-correct")
    warning(msg)
    
    
  }
###  print(laplacian_filter)
  
  funf <- function(x,filterf,na.rm=TRUE) {
    
    sum(x * filterf,na.rm=na.rm)
    
  }
  out <- focal(x=x,w=w,fun=funf,filterf=laplacian_filter/xxres^2,na.rm=na.rm,...)
  names(out) <- name
  return(out)
  
}

