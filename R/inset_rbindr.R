#' @title Rbind Spatial*DataFrame Objects With Rows and Cols Handling 
#' @name inset_rbinder
#' @description Take a list of Spatial*DataFrame and output a single Spatial*DataFrame
#' @param l a list of Spatial*DataFrame
#' @return A Spatial*DataFrame
#' @import sp
#' @examples
#' if(require(cartography)){
#'   data(nuts2006)
#'   mybbox <- rbind(c(4235059,4507390),c(2559828, 2784798)) 
#'   mymask <- create_mask(bb = mybbox, prj = proj4string(nuts0.spdf), add=FALSE)
#'   inset <- move_and_resize(spdf = nuts0.spdf, mask = mymask, 
#'                            xy = c(5566624, 3599815), k = 5)
#'   newnuts <- inset_rbinder(list(nuts0.spdf, inset))  
#'   plot(newnuts)
#' }
#' @export
inset_rbinder <- function(l = list()){

  df <- data.frame(class = sapply(l,  class),
                   prj = sapply(l,proj4string),
                   nrow = sapply(l, nrow),
                   cnames = sapply(l,function(x){paste0(colnames(x@data), collapse = ",")}),
                   stringsAsFactors = FALSE)


  if(!length(unique(df[, 1]))==1){
    stop("Objects are not of the same type", call. = F)
  }

  if(!length(unique(df[, 2]))==1){
    stop("Objects are not in the same projection", call. = F)
  }

  if(!length(unique(df[, 4]))==1){
    warning("As columns are not the sames only the first one is kept and it is called 'id'", call. = F)
    for (i in 1:length(l)){
      l[[i]]@data <- data.frame(id = l[[i]]@data[,1], stringsAsFactors = F)
    }
  }


  r <- 0
  for (i in 1:length(l)){
    row.names(l[[i]]) <- as.character(r + 1:df[i,3])
    r <- r + df[i,3]
  }

  return(do.call('rbind', l))
  
}