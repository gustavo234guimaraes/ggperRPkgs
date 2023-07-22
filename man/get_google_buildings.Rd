\name{get_google_buildings}
\alias{get_google_buildings}

\title{
Get Open Buildings from Google
}
\description{
This function takes a layer in sf or a place name and returns an sf with the Google Open Buildings buildings in the inserted layer
(View https://sites.research.google/open-buildings/)
}
\usage{
get_google_building(layer)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{layer}{
A sf object or a character. A layer or a place used to filter the buildings by location. When use sf, if the layer not is a POLYGON the st_bbox is used, note that this is less accurate and probably return unnecessary buildings.
The place is inserted in OSM API and the polygon of place is used, note that OSM API can return more than one polygon, pass a sufficiently complete address to the function.
For large area polygons the return can be too large too, try use download_google_buildings for best experience.
}
  \item{output.format}{
A character. Default is "data.frame", the format of result make be "csv" or "sf"
}
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



\seealso{
osmdata
https://sites.research.google/open-buildings/tiles.geojson
}
\examples{
## The function is currently defined as

poly_layer<-read_sf(path)

get_google_buildings(layer = poly_layer)

get_google_buildings(layer = "Fortaleza, CE")



}

