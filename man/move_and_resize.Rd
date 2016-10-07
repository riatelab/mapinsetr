% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/move_and_resize.R
\name{move_and_resize}
\alias{move_and_resize}
\title{Move and Resize a Spatial*DataFrame}
\usage{
move_and_resize(spdf, mask, xy, prj = proj4string(spdf), k)
}
\arguments{
\item{spdf}{the Spatial*Dataframe to resize and move.}

\item{mask}{a SpatialPolygonsDataFrame used to select the area to move an resize.}

\item{xy}{coordinates used for the move process, bottomleft corner of the inset.}

\item{prj}{projection of the inset.}

\item{k}{factor used to resize.}
}
\value{
A SpatialPolygonsDataFrame
}
\description{
Move and Resize a Spatial*DataFrame
}
\examples{
\dontrun{
if(require(cartography)){
  data(nuts2006)
  nuts0.spdf@data <- nuts0.df
  spdf <- nuts0.spdf[nuts0.spdf$id \%in\% c("FR", "LU", "DE", "BE"), ]
  mask <- rgeos::readWKT("POLYGON((3862526 2770745, 
                              3862526 2971831,
                              4200108 2971831, 
                              4200108 2770745, 
                              3862526 2770745))")
  proj4string(mask) <- proj4string(spdf)
  x <- move_and_resize(spdf = spdf, mask = mask, xy = c(5566624, 3599815), k = 3)
  plot(nuts0.spdf)
  points(5566624, 3599815)
  plot(mask, add=T)
  plot(x, add=T)
}
       }
}
