\name{download_pbf}
\alias{download_pbf}

\title{
Download osm map in .pbf
}
\description{
This function make download of a geofabrik extraction of OSM map and cut for a specific área, passed by place.
}
\usage{
download_pbf("Fortaleza, CE")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{place}{
A sf object, a bbox or a character. A layer or a place used to filter the map by location. When use sf the st_bbox is used to get the bbox of layer. When use character the place is inserted in OSM API and the polygon of place is used, note that OSM API can return more than one polygon, pass a sufficiently complete address to the function.
}

  \item{dest.path}{
A character. The directory for save the results.
}

  \item{fname}{
A character. The filename to save the .osm.pbf data.
}
}

\author{
"Guimarães, Gustavo <gustavo234guimaraes@gmail.com>"
}

\examples{

download_pbf("Fortaleza, CE",fname="fortaleza")

centro<-read_sf("centro_fortaleza.gpkg")

download_pbf(
place=centro,
dest.path=getwd(),
fname="centro"
)


}
