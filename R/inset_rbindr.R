#' @title Rbind Spatial*DataFrame Objects With Rows and Cols Handling 
#' @name inset_rbinder
#' @description Take a list of sf POLYGON or MULTIPOLYGON objects and output a 
#' single sf POLYGON or MULTIPOLYGON object. 
#' @param l a list of POLYGON or MULTIPOLYGON objects
#' @return An sf POLYGON or MULTIPOLYGON object
#' @import sf
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

