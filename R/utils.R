test_input <- function(x, name, sf = TRUE){
  if(sf){
    if (!inherits(x, "sf")){
      stop(paste0(name, ' should be an sf data.frame.'), call. = FALSE)
    }
  }else{
    if (!inherits(x, c("sf", "sfc"))){
      stop(paste0(name, ' should be an sf data.frame or an sfc object.'), 
           call. = FALSE)
    }
  }

  if (is.na(st_crs(x))){
    stop(paste0(name, ' should be have a valid CRS.'), call. = FALSE)
  }
  if (st_is_longlat(x)){
    stop(paste0(name, ' should use a projected CRS (not lon/lat).'), 
         call. = FALSE)
  }
}

test_type <- function(x){
  type <- sf::st_geometry_type(x, by_geometry = FALSE)
  type <- as.character(unique(type))
  if (!type %in% c("POINT", "MULTIPOINT", 
                   "LINESTRING", "MULTILINESTRING", 
                   "POLYGON", "MULTIPOLYGON")){
    stop(paste0("x geometry should be of type POINT, MULTIPOINT,", 
                "LINESTRING, MULTILINESTRING, POLYGON or MULTIPOLYGON."), 
         call. = FALSE)
  } 
  return(type)
}