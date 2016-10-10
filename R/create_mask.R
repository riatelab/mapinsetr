#' @title Create a Mask
#' @name create_mask
#' @description Create a mask based on a bounding box, a Spatial*DataFrame or defined interactively.
#' @param bb either a bounding box or a Spatial*DataFrame
#' @param prj  a projection string
#' @param interactive define the mask interactively 
#' @param add add the mask to the current plot
#' @return A SpatialPolygonsDataFrame
#' @import sp
#' @examples 
#' if(require(cartography)){
#'  data(nuts2006)
#'  mybbox <- rbind(c(3998249, 4720519), c(2323018,2879522)) 
#'  mymask <- create_mask(bb = mybbox, prj = proj4string(nuts0.spdf), add=FALSE)
#'  plot(nuts0.spdf, col = "grey80")
#'  plot(mymask, add=TRUE, border = "red", lwd = 3)
#'  mymask2 <- create_mask(bb = nuts0.spdf[nuts0.spdf$id=="BG",], 
#'                         prj = proj4string(nuts0.spdf))
#'  plot(mymask2, add=TRUE, border = "green", lwd = 3)
#' }
#' @export
create_mask <- function(bb, prj, interactive = FALSE, add = FALSE){
  if(interactive){
    x <- graphics::locator(n=2, type="n")
    minx <- min(x$x)
    maxx <- max(x$x)
    miny <- min(x$y)
    maxy <- max(x$y)
  }else{
    if(methods::is(bb, "Spatial")){
      x <- bbox(bb)
    }else{
      x <- bb
    }
    minx <- x[1,1]
    maxx <- x[1,2]
    miny <- x[2,1]
    maxy <- x[2,2]
  }
  wkt <- paste0("POLYGON((", 
                minx, " ", miny, ",",
                minx, " ", maxy, ",", 
                maxx, " ", maxy, ",",
                maxx, " ", miny, ",", 
                minx, " ", miny,
                "))")  
  mask <- rgeos::readWKT(wkt)
  if(add){sp::plot(mask, add=T)}
  proj4string(mask) <- prj
  mask <- SpatialPolygonsDataFrame(Sr = mask, data = data.frame(id = 1), match.ID = F)
  
  return(mask)
}