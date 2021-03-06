% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_mask.R
\name{create_mask}
\alias{create_mask}
\title{Create a Mask}
\usage{
create_mask(bb, prj, interactive = FALSE, add = FALSE)
}
\arguments{
\item{bb}{either a bounding box, a numeric vector (xmin, ymin, xmax, ymax), 
an sf or sfc object.}

\item{prj}{a CRS string.}

\item{interactive}{define the mask interactively.}

\item{add}{add the mask to the current plot.}
}
\value{
An sf object is returned.
}
\description{
Create a mask based on a bounding box (\code{bbox}), a simple 
feature collection (\code{sf}) extent, a simple feature geometry list column 
(\code{sfc}) extent or an interactively defined rectancle.
}
\examples{
library(sf)
nc <- st_read(system.file("shape/nc.shp", package="sf"))
nc <- st_transform(nc, 32119)

plot(st_geometry(nc))
bb <- st_bbox(nc[nc$CNTY_ID \%in\% c('2030', '1989', '1938'),])
mask <- create_mask(bb = bb, add = TRUE)

plot(st_geometry(nc))
bb <- nc[nc$CNTY_ID \%in\% c('2030', '1989', '1938'),]
mask <- create_mask(bb = bb, add = TRUE)

plot(st_geometry(nc))
bb <- c(589912, 159757, 694332, 257053)
mask <- create_mask(bb = bb, prj=32119, add = TRUE)
}
