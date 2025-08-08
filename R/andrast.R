NULL
#' INteresction (AND) of catagories in a multi-layer raster/rast object
#'
#' @param x   a SpatRast object 
#' @param nmax_cat maximum number of catagories per layer
#' @param name output raster name
#' @param ... further arguments
#'
#'
#' @importFrom terra levels levels<- as.factor
#' @export
#' 
#' @examples
#' 
#' library(terra)
#' library(leaflet)
#' elev <- rast(system.file("ex/elev_vinschgau.tif",package="terra"))
#' names(elev) <- "elev"
#' elev_rcl <- c(0,1000,2000,3000,4000) ##c(0,500,1500,2000,2500,3000,3500)
#' elevb <- classify(elev,rcl=elev_rcl)
#' #######
#' #######
#' naspect <- 4 ##8
#' aspect <- terrain(elev,"aspect")
#' aspect[aspect>360*(1-1/(naspect*2))] <- aspect[aspect>360*(1-1/(naspect*2))]-360
#' aspect_rcl <- (0:naspect)/naspect*360-360/(naspect*2)
#' aspectb <- classify(aspect,rcl=aspect_rcl)
#' #######
#' #######
#' laplacian <- laplacian(elev)
#' laplacian_rcl <- c(-Inf,-0.01,0.01,Inf)
#' laplacianb <- classify(laplacian,rcl=laplacian_rcl)
#' 
#' x <- c(elevb,aspectb,laplacianb)
#' 
#' out <- andrast(x)
#' 
#' plet(out,col=rainbow(80),tiles="OpenTopoMap")
#' 
#'  ## TO DO 

andrast <- function(x,nmax_cat=100,name="andrast",...) {
  
  ll <- terra::levels(x)
  if (is.null(ll)) stop("x must be factor (e.g. category raster)")
  print(sapply(ll,FUN=nrow))
  nmax_cat_a=max(sapply(ll,FUN=nrow))
  if (nmax_cat<=nmax_cat_a) nmax_cat=nmax_cat_a+10
  
  out <- NULL
  llout <- NULL
  
  for (i in 1:length(ll)) {
    u <- ll[[i]]
    u[,paste0(names(u)[2],"_id")] <- u[,1]
    u[,1] <- (u[,1]+1)*nmax_cat^(i-1)
   ## u <- u[,rev(names(u))]
    
    
    
    ll[[i]] <- u
    
    temp <- (x[[i]]+1)*nmax_cat^(i-1)
    if (is.null(out)) {
      
      out <- temp 
    } else {
      
      
      out <- out+temp
    }
    
    if (is.null(llout)) {
      
      llout <- u
    } else {
      bb <- list()
      for (j in 1:nrow(u)) {
        
        ##bb[[jj]] <- llout 
        du <- u[array(j,nrow(llout)),,drop=FALSE]
        du$ID <- du$ID+llout$ID
        nn <- names(llout)
        nn <- nn[nn!="ID"]
        du[,nn] <- llout[,nn]
        
        bb[[j]] <- du
        ##print(du)
        
        ## TO DO 
        ##bb[[jj]] <- 
        
        
        
      }
      llout <- do.call(what="rbind",args=bb)
      
     ## TO DO  out <- out+temp
    }
  }
  
  
  for (it in names(x)) {
    
    llout[,it] <- paste(it,llout[,it],sep=":")
    
  }
  llout$value <- apply(llout[,names(x)],FUN=paste,collapse="_",MARGIN=1)
  
  llout <- llout[,c(names(llout)[1],"value")]
  
  out <- as.factor(out)
  names(out) <- name
  levels(out) <- llout
  
  
  ##out <- x 
  return(out)
  
}

