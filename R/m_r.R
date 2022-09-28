#' @title Move and Resize in Box
#' @description do stuff
#' @param x the layer to cut, resize and move
#' @param mask the targeted area in x
#' @param y destination 
#' @param return_k return the k factor 
#'
#' @return a layer
#' @export
#'
#' @examples
#' library(sf)
#' library(mapsf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' # Ne fonctionne qu'avec 2 fonds projetés
#' nc <- st_transform(nc, 32119)
#' # Créer des boites
#' y <- st_as_sfc(st_bbox(
#'   c(xmin = 270000, ymin = 00000,
#'     xmax = 550000, ymax = 100000),
#'   crs = 32119))
#' y <- st_make_grid(y, n = c(3,2))
#' mf_map(nc)
#' mf_map(y, add = TRUE)
#' # plusieurs objets agrandis
#' inset1 <- m_r(x = nc, mask = nc[1,], y = y[1])
#' mf_map(nc[1, ], col = 2, add = TRUE)
#' mf_map(inset1, col = 2, add = TRUE)
#' # plusieurs objets agrandis
#' inset2 <- m_r(x = nc, mask = nc[2,], y = y[2])
#' mf_map(nc[2, ], col = 3, add = TRUE)
#' mf_map(inset2, col = 3, add = TRUE)
#' # un seul objet, réduit
#' inset3 <- m_r(x = nc[nc$CNTY_ID == 2000, ], 
#'               mask = nc[nc$CNTY_ID == 2000, ], 
#'               y = y[3])
#' mf_map(nc[nc$CNTY_ID == 2000, ], col = 4, add = TRUE)
#' mf_map(inset3, col = 4, add = TRUE)
#' # plusieurs objets dans une autre proj
#' mtq <- mf_get_mtq()
#' inset4 <- m_r(x = mtq, mask = mtq, y = y[4])
#' mf_map(inset4, col = 5, add = TRUE)
#' # bouger des points
#' pts <- st_as_sf(st_sample(x = nc[1, ], 10))
#' inset5 <- m_r(x = pts, mask = nc[1,], y = y[1])
#' mf_map(pts, cex = .2, add=TRUE)
#' mf_map(inset5, cex = .2, add = TRUE)
m_r <- function(x, mask, y, return_k = FALSE){
  # input management
  test_input(x, "x", TRUE)
  test_input(mask, "mask", FALSE)
  test_input(y, "y", FALSE)
  if(st_crs(x) != st_crs(mask)){
    stop(paste0('x and mask should use the same CRS.'), call. = FALSE)
  }
  
  # names order mngmt
  namesorder <- names(x)
  
  # type mgmt
  type <- test_type(x)
  
  
  # compute matching mask geometry
  bbm <- st_bbox(mask)
  bby <- st_bbox(y)
  bby_l <- bby[3] - bby[1]
  bby_h <- bby[4] - bby[2] 
  bbm_l <- bbm[3] - bbm[1]
  bbm_h <- bbm[4] - bbm[2]
  hly <- bby_h / bby_l
  hlm <- bbm_h / bbm_l
  
  # si hauteur y > largeur y
  if(hly <= 1){
    fact <- 1 / hly
  }else{
    fact <- hly
  }
  # si y est plus étiré en hauteur que m
  if(hly >= hlm){
    new_h <- bbm_l * fact
    ad <- (new_h - bbm_h) / 2 
    bbm[2] <- bbm[2] - ad
    bbm[4] <- bbm[4] + ad
  }else{
    # si y est moins étiré en hauteur que m
    new_l <- bbm_h * fact
    ad <- (new_l - bbm_l) / 2 
    bbm[1] <- bbm[1] - ad
    bbm[3] <- bbm[3] + ad
  }
  mask <- st_as_sfc(bbm, crs = st_crs(mask))
  
  # compute matching k value
  bbm <- st_bbox(mask)
  bbm_l <- bbm[3] - bbm[1]
  bbm_h <- bbm[4] - bbm[2]
  k <- bby_l / bbm_l
  
  if(return_k){
    return(k)
  }
  
  
  xy <- bby[1:2]
  
  # intersect mask and x
  if (type == "POINT"){
    x <-  st_intersection(x, mask)
    type <- sf::st_geometry_type(x, by_geometry = FALSE)
    type <- as.character(unique(type))
    if (type == "GEOMETRY") {
      x <-  st_collection_extract(x, type = "POINT")
    }
  } else {
    st_agr(x) <- "constant"
    x <- st_intersection(x, mask)
  }
  
  # add mask to x
  xm <- x[1, ]
  st_geometry(xm) <- mask
  x <- rbind(xm,x)
  
  # resize & move
  cntrd <- st_centroid(st_combine(x))
  xg <- (st_geometry(x) - cntrd) * c(k) + cntrd[[1]][]
  st_geometry(x) <- xg + xy - st_bbox(xg)[1:2]
  
  # get rid of mask
  x <- x[-1,]
  
  # MULTIPOLYGON mgmt
  if (type == "MULTIPOLYGON"){
    x <- st_cast(x, "MULTIPOLYGON")
  }
  
  # Asssign destination layer CRS to result
  st_crs(x) <- st_crs(y)
  
  # names order mngmt
  x <- x[, namesorder]
  

  
  return(x)
}
