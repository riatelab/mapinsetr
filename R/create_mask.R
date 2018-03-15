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
#' @export
create_mask <- function(bb, prj, interactive = FALSE, add = FALSE){
  err <- simpleError("bb should have a CRS or prj should be provided.")
  
  # for interactive
  if(interactive){
    if(missing(prj)){
      stop(err)
    }
    x <- graphics::locator(n=2, type="n")
    bb <- st_bbox(c(xmin = min(x$x), ymin = min(x$y), 
                    xmax = max(x$x), ymax = max(x$y)), 
                  crs =  prj)  
  }
  
  # for numeric vector
  if(is.numeric(bb) & length(bb)==4){
    if(missing(prj)){
      stop(err)
    }
    bb <- st_bbox(c(xmin = bb[1], ymin=bb[2], xmax = bb[3], ymax = bb[4]), crs =  prj)
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
  if(add){plot_sf(st_geometry(mask), add=T)}

  return(mask)
}
# 
# 


# library(sf)
# library(sp)
# 
# polysf <- mtq[1, ]
# mask <- create_mask(bb = polysf, add = F)
# 
# plot(mtq$geometry)
# plot(mask, add=T)
# 
# bb <- bbsp
# create_mask(bb, prj = st_crs(mtq), add=T)
# bb <- bbsf
# create_mask(bb, add=T)
# bb <- polysf
# create_mask(bb, add=T)
# create_mask(interactive = TRUE, prj = proj4string(as(mtq, "Spatial")), add=T)
# 
# mtq2 <- mtq
# st_crs(mtq2) <- NA
# polysf <- mtq2[1, ]
# bbsf <- st_bbox(polysf)
# polysp <- as(polysf, "Spatial")
# bbsp <- bbox(polysp)
# 
# 
# bb <- polysp
# create_mask(bb)
# bb <- bbsp
# create_mask(bb, prj = st_crs(mtq))
# bb <- bbsf
# create_mask(bb)
# bb <- polysf
# create_mask(bb)
# create_mask(interactive = TRUE, prj = proj4string(as(mtq, "Spatial")))
# 
