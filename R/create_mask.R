#' @title Create a Mask
#' @name create_mask
#' @description Create a mask based on a bounding box (\code{bbox}), a simple 
#' feature collection (\code{sf}) extent, a simple feature geometry list column 
#' (\code{sfc}) extent or an interactively defined rectancle.
#' @param bb either a bounding box, a numeric vector (xmin, ymin, xmax, ymax), 
#' an sf or sfc object.  
#' @param prj a CRS string.
#' @param interactive define the mask interactively.
#' @param add add the mask to the current plot. 
#' @return An sf object is returned.
#' @import sf
#' @import graphics
#' @examples 
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' nc <- st_transform(nc, 32119)
#' 
#' plot(st_geometry(nc))
#' bb <- st_bbox(nc[nc$CNTY_ID %in% c('2030', '1989', '1938'),])
#' mask <- create_mask(bb = bb, add = TRUE)
#' 
#' plot(st_geometry(nc))
#' bb <- nc[nc$CNTY_ID %in% c('2030', '1989', '1938'),]
#' mask <- create_mask(bb = bb, add = TRUE)
#' 
#' plot(st_geometry(nc))
#' bb <- c(589912, 159757, 694332, 257053)
#' mask <- create_mask(bb = bb, prj=32119, add = TRUE)
#' @export
create_mask <- function(bb, prj, interactive = FALSE, add = FALSE){
  err <- simpleError("bb should have a CRS or prj should be provided.")
  
  # for interactive
  if(interactive){
    if(missing(prj)){
      stop(err)
    }
    x <- graphics::locator(n=2, type="n")
    bb <- st_bbox( c(xmin = min(x$x), ymin = min(x$y), 
                     xmax = max(x$x), ymax = max(x$y)), 
                   crs =  prj)
    }
  
  # for numeric vector
  if(is.numeric(bb) & length(bb)==4 & class(bb)[1]!="bbox"){
    if(missing(prj)){
      stop(err)
    }
    bb <- st_bbox(c(xmin= bb[1], ymin = bb[2], 
                    xmax = bb[3], ymax = bb[4]), crs =  prj)
    }
  
  # for sf object
  if(max(class(bb) %in% c("sf", "sfc"))==1){
    if(is.na(st_crs(bb)) & missing(prj)){
      stop(err)
    }
    bb <- st_bbox(bb)
  }
  
  # prj for bbox objetc
  if(is.na(st_crs(bb)) & missing(prj)){
    stop(err)
  }

  
  # build the mask
  mask <- st_sf(id = 1, geometry =  st_as_sfc(bb), crs = st_crs(bb))
  
  # plot the mask
  if(add){plot(sf::st_geometry(mask), add=T, border = "red", lwd = 2)}
  
  return(mask)
}