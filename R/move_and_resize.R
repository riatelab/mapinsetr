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
#' \dontrun{
#' if(require(cartography)){
#'   data(nuts2006)
#'   nuts0.spdf@data <- nuts0.df
#'   spdf <- nuts0.spdf[nuts0.spdf$id %in% c("FR", "LU", "DE", "BE"), ]
#'   mask <- rgeos::readWKT("POLYGON((3862526 2770745, 
#'                               3862526 2971831,
#'                               4200108 2971831, 
#'                               4200108 2770745, 
#'                               3862526 2770745))")
#'   proj4string(mask) <- proj4string(spdf)
#'   x <- move_and_resize(spdf = spdf, mask = mask, xy = c(5566624, 3599815), k = 3)
#'   plot(nuts0.spdf)
#'   points(5566624, 3599815)
#'   plot(mask, add=T)
#'   plot(x, add=T)
#' }
#'        }
#' @export
move_and_resize <- function(spdf, mask, xy, prj = proj4string(spdf), k){
  # A faire fonction pour transformer une bbox en spdf
  # argument pos pour l'encrage du carton
  # gUnaryNunionage du mask si nrow > 1
  # problème si une seule colonne dans spdf@data
  if (is.null(mask)){
    mask <- gBuffer(spdf)
    
  }
  if(length(mask) > 1 ){
    mask <- rgeos::gBuffer(mask)
  }
  

  inter <- gIntersection(mask, spdf, byid = T)
  
  df <- data.frame(id = sapply(methods::slot(inter, "polygons"), 
                               methods::slot, "ID"))
  df <- data.frame(do.call('rbind', (strsplit(as.character(df$id)," "))))
  row.names(df) <- df$X2
  df <- data.frame(spdf@data[match(row.names(df),  row.names(spdf) ), ])
  row.names(inter) <- row.names(df)
  spdf <- SpatialPolygonsDataFrame(Sr = inter, data = df, match.ID = T)
  
  x <- spdf[1, ]
  x@polygons <- mask@polygons
  x@polygons[[1]]@ID <- "YOLO"
  row.names(x@data) <- row.names(x)
  spdf <- rbind(x, spdf)
  
  
  spdf_bb <- bbox(spdf) 
  spdf_w <- spdf_bb[1,2] - spdf_bb[1,1]
  spdf_h <- spdf_bb[2,2] - spdf_bb[2,1]
  spdf_sizemax <- which.max(c(spdf_w, spdf_h))
  
  
  
  if(k==1){
    k <- 1.0000001
  }
  
  if(spdf_sizemax==1){
    scale <- spdf_w * k
  }else{
    scale <- spdf_h * k
  }
  
  
  # ça marche pas....
  # if(k==1){
  #   spdf_resized <- spdf
  # }else{
  #   spdf_resized <- elide(obj = spdf, scale = scale)
  # }
  # points(x = xy[1], y = xy[2], cex = 2, col = "red", pch = 20)
  
  

  spdf_resized <- elide(obj = spdf, scale = scale)
  
  spdf_moved <- elide(obj = spdf_resized,  shift=xy)
  
  spdf_moved <- spdf_moved[-1, ]
  proj4string(spdf_moved) <- prj
  
  return(spdf_moved)
  
}