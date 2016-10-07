# inset_rbinder <- function(l = list(x, nuts0.spdf, y)){
#   
#   df <- data.frame(class = sapply(l,  class), 
#                    prj = sapply(l,proj4string), 
#                    nrow = sapply(l, nrow), 
#                    cnames = sapply(l,function(x){paste0(colnames(x@data), collapse = ",")}), 
#                    stringsAsFactors = FALSE)
#   
#   
#   if(!length(unique(df[, 1]))==1){
#     stop("Objects are not of the same type", call. = F)
#   }
#   
#   if(!length(unique(df[, 2]))==1){
#     stop("Objects are not in the same projection", call. = F)
#   }
#   
#   if(!length(unique(df[, 4]))==1){
#     warning("As columns are not the sames only the first one is kept and it is called 'id'", call. = F)
#     for (i in 1:length(l)){
#       l[[i]]@data <- data.frame(id = l[[i]]@data[,1], stringsAsFactors = F)
#     }
#   }
#   
#   
#   r <- 0
#   for (i in 1:length(l)){
#     row.names(l[[i]]) <- as.character(r + 1:df[i,3])
#     r <- r + df[i,3]
#   }
#   
#   return(do.call('rbind', l))
# }