#' @title Move and Resize a Spatial*DataFrame
#' @name move_and_resize
#' @description Move and Resize a Spatial*DataFrame
#' @param spdf the Spatial*Dataframe to resize and move.
#' @param mask  a SpatialPolygonsDataFrame used to select the area to move an resize. 
#' @param xy coordinates used for the move process, bottomleft corner of the inset. 
#' @param prj projection of the inset. 
#' @param k factor used to resize. 
#' @return A SpatialPolygonsDataFrame
#' @import sp
#' @import maptools
#' @import rgeos
#' @examples
#' if(require(cartography)){
#'   data(nuts2006)
#'   mybbox <- rbind(c(4235059,4507390),c(2559828, 2784798)) 
#'   mymask <- create_mask(bb = mybbox, prj = proj4string(nuts0.spdf), add=FALSE)
#'   inset <- move_and_resize(spdf = nuts0.spdf, mask = mymask, 
#'                            xy = c(5566624, 3599815), k = 5)
#'   plot(nuts0.spdf)
#'   plot(mymask, lwd = 2, border = "red", add = TRUE)
#'   plot(inset, add = TRUE)
#' }
#' @export
move_and_resize <- function(spdf, mask = NULL, xy, prj = proj4string(spdf), k = 1){
  # A faire fonction pour transformer une bbox en spdf ?
  # argument pos pour l'encrage du carton

  # default mask
  if (is.null(mask)){
    mask <- gBuffer(spdf)
  }
  
  # proj tests
  stopifnot(is.projected(spdf), is.projected(mask))
  stopifnot(proj4string(mask)==proj4string(spdf))
  
  # union mask
  if(length(mask) > 1 ){
    mask <- rgeos::gBuffer(mask)
  }

  # intersect spdf with mask
  inter <- rgeos::gIntersection(mask, spdf, byid = T)
  df <- data.frame(id = sapply(methods::slot(inter, "polygons"), 
                               methods::slot, "ID"), stringsAsFactors = F)
  df <- data.frame(do.call('rbind', (strsplit(as.character(df$id)," "))))
  df <- data.frame(id = df$X2, stringsAsFactors = F)
  row.names(df) <- df$id
  df <- data.frame(spdf@data[match(row.names(df),  row.names(spdf) ), ])
  colnames(df) <- colnames(spdf@data)
  row.names(inter) <- row.names(df)
  spdf <- SpatialPolygonsDataFrame(Sr = inter, data = df, match.ID = T)
  
  # add mask to spdf
  x <- spdf[1, ]
  x@polygons <- mask@polygons
  x@polygons[[1]]@ID <- "YOLO"
  row.names(x@data) <- row.names(x)
  
  # resize
  spdf <- rbind(x, spdf)
  spdf_bb <- bbox(spdf) 
  spdf_w <- spdf_bb[1,2] - spdf_bb[1,1]
  spdf_h <- spdf_bb[2,2] - spdf_bb[2,1]
  spdf_sizemax <- which.max(c(spdf_w, spdf_h))
  
  if(spdf_sizemax==1){
    scale <- spdf_w * k
  }else{
    scale <- spdf_h * k
  }
  
  # special k=1 case
  if(k==1){
    spdf_resized <- spdf
  }else{
    spdf_resized <- elide(obj = spdf, scale = scale)
  }
  spdf_resized <- maptools::elide(obj = spdf, scale = scale)

  # move
  xy <- xy - bbox(spdf_resized)[,1]
  spdf_moved <- elide(obj = spdf_resized,  shift=xy)
  
  # get rid of mask
  spdf_moved <- spdf_moved[-1, ]

  # project
  proj4string(spdf_moved) <- prj
  
  return(spdf_moved)
}