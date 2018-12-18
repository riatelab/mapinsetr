#' @title Rbind sf Objects With Rows and Cols Handling 
#' @name inset_rbinder
#' @description Takes a list of sf polygons or multipolygons objects and output 
#' a single sf polygon or multipolygon object. 
#' @param l a list of sf POLYGON or MULTIPOLYGON objects.
#' @return An sf polygon or multipolygon object is returned. 
#' @import sf
#' @examples
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' nc <- st_transform(nc, 32119)
#' 
#' plot(st_geometry(nc))
#' mask1 <- st_buffer(st_centroid(st_geometry(nc[nc$CNTY_ID == 2026,])),dist = 30000)
#' mask2 <- st_buffer(st_centroid(st_geometry(nc[nc$CNTY_ID == 2016,])),dist = 30000)
#' plot(st_geometry(mask1), border = "red", lwd = 2, add = TRUE)
#' plot(st_geometry(mask2), border = "red", lwd = 2, add = TRUE)
#' inset1 <- move_and_resize(nc, mask1, xy = c(200000, 5000), k = 2)
#' inset2 <- move_and_resize(nc, mask2, xy = c(200000 + 130000, 5000), k = 2)
#' plot(st_geometry(inset1), add = TRUE)
#' plot(st_geometry(inset2), add = TRUE)
#' 
#' nc_insets <- inset_rbinder(l = list(nc, inset1, inset2))
#' plot(st_geometry(nc_insets))
#' plot(nc_insets[,9])
#' @export
inset_rbinder <- function(l = list()){
  df <- data.frame(class = sapply(l,  class)[[1]],
                   prj1 = sapply(l,st_crs)[[1]],
                   prj2 = sapply(l,st_crs)[[2]],
                   nrow = sapply(l, nrow),
                   cnames = sapply(l,function(x){paste0(colnames(x), collapse = ",")}),
                   geom = sapply(l,function(x)unique(st_geometry_type(x))),
                   stringsAsFactors = FALSE)
  if(!length(unique(df[, 1][1]))==1){
    stop("Objects are not of the same type", call. = F)
  }

  if(!length(unique(df[, 2]))==1){
    stop("Objects are not in the same projection", call. = F)
  }
  if(!length(unique(df[, 3]))==1){
    stop("Objects are not in the same projection", call. = F)
  }

  if(!length(unique(df[, 5]))==1){
    warning("As columns are not the sames only the first one is kept and it is called 'id'", call. = F)
    for (i in 1:length(l)){
      l[[i]] <- data.frame(l[[i]][,1], stringsAsFactors = F)
      names(l[[i]])[1] <- "id"
    }
  }

  # avoid GEOMETRY column type
  if(!length(unique(df[, 6]))==1){
    for (i in 1:length(l)){
      l[[i]] <- st_cast(l[[i]], "MULTIPOLYGON")
    }
  }
  
  r <- 0
  for (i in 1:length(l)){
    row.names(l[[i]]) <- as.character(r + 1:df[i,4])
    r <- r + df[i,4]
  }
  
  
  
  return(do.call('rbind', l))
  
}

