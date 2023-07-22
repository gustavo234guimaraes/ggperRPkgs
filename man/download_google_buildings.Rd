\name{download_google_building}
\alias{download_google_building}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Download Open Buildings from Google
}
\description{
This function takes a layer in sf or a place name and export an data with the Google Open Buildings buildings in the inserted layer
(View https://sites.research.google/open-buildings/)
}
\usage{
download_google_building(layer,destiny_file=path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{layer}{
A sf object or a character. A layer or a place used to filter the buildings by location. When use sf, if the layer not is a POLYGON the st_bbox is used, note that this is less accurate and probably return unnecessary buildings.
The place is inserted in OSM API and the polygon of place is used, note that OSM API can return more than one polygon, pass a sufficiently complete address to the function.
}
  \item{destiny_file}{
A character. A path to a file with extension csv, txt or geo formats to save the outputs (see details).
}
}
\details{
When use large polygons this functions save the output by tile which requires less RAM memory but may take longer.

Output formats need be only of a list csv, txt, geojson, gpkg, kml or shp.
}
\references{
https://sites.research.google/open-buildings/
https://rspatialdata.github.io/osm.html
}
\author{
"Guimarães, Gustavo <gustavo234guimaraes@gmail.com>"
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
library(osmdata)
https://sites.research.google/open-buildings/tiles.geojson
}
\examples{


The function is currently defined as

poly_layer<-read_sf(path)

download_google_buildings(layer = ploy_layer,destiny_file="file.csv")

get_google_buildings(
layer = "Fortaleza, CE",
,destiny_file="file.csv"
)


}

