#' @title Move and Resize a Spatial*DataFrame
#' @name move_and_resize
#' @description Move and Resize an sf POLYON or MULTIPOLYGON object
#' @param x an sf POLYON or MULTIPOLYGON object to resize and move.
#' @param mask  an sf POLYON or MULTIPOLYGON object used to select the area to move an resize. 
#' @param xy coordinates used for the move process, bottomleft corner of the inset. 
#' @param prj outputted projection of the inset. 
#' @param k factor used to resize. 
#' @return An sf object
#' @import sf
#' @export
move_and_resize <- function(x, mask = NULL, xy, prj, k = 1){
  # default prj 
  if(missing(prj)){prj <- st_crs(x)}
  
  # default mask
  if (missing(mask)){
    mask <- st_union(x)
  }
  
  # input proj tests
  stopifnot(!is.na(st_crs(mask)), !is.na(st_crs(x)))

  # union mask
  if(nrow(mask) > 1 ){
    mask <- st_union(mask)
  }

  # intersect mask and x
  options(warn=-1)
  x <- st_collection_extract(st_intersection( x, st_geometry(mask)), 
                             type = c("POLYGON"))
  options(warn=0)

  # add mask to x
  xm <- x[1, ]
  st_geometry(xm) <- st_geometry(mask)
  x <- rbind(xm,x)
  
  # resize & move
  cntrd <- st_centroid(st_combine(x))
  xg <- (st_geometry(x) - cntrd) * k + cntrd 
  st_geometry(x) <- xg + xy - st_bbox(xg)[1:2]
  
  # get rid of mask
  x <- x[-1,]
  if(missing(prj)){}
  st_crs(x) <- prj
  
  return(x)
}
