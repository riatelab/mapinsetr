#' @title Create a Mask
#' @name create_mask
#' @description Create a mask based on a bounding box, an sf POLYGON or 
#' MULTIPOLYGON or an interactively defined rectancle.
#' @param bb either a bounding box (bbox object), a numeric vector (xmin, ymin, 
#' xmax, ymax) or an sf POLYGON or MULTIPOLYGON object. 
#' @param prj a projection string
#' @param interactive define the mask interactively 
#' @param add add the mask to the current plot
#' @return An sf object
#' @import sf
#' @import graphics
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
  mask <- st_sf(id = 1, geometry = st_as_sfc(bb), crs = st_crs(bb))
  
  # plot the mask
  if(add){plot(sf::st_geometry(mask), add=T)}
  
  return(mask)
}