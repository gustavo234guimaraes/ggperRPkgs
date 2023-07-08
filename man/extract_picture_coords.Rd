\name{extract_picture_coords}
\alias{extract_picture_coords}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
"Extract coordinates of a picture"
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
"This function extracts coordinates from a georeferenced image or multiple images, the process is based in exiftool"
}
\usage{
extract_picture_coords("./image.jpg")

extract_picture_coords(
fig.path = NULL,
dir = getwd(),
fig.pattern = ".jpg",
export.path="same",
save.geometry=TRUE,
fig.crs = 4326,
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fig.path}{
A character. Path for a geocoded picture file
}
  \item{dir}{
A character. Path with multiple pictures to extract coordinates,
this algorithm is recursive and returns a csv file with all coordinates of all images from all subdirectories in dir (valid only when fig.path=NULL)
}
  \item{fig.pattern}{
A character. When extract multiple images the extension of pictures to list. You can pass a multiple extensions using .jpg|.png|.jpeg, note that .jpg don't is the same of .JPG
}
  \item{export.path}{
A character. Directory for save outputs, the default is 'same' which uses the same dir value
}
  \item{save.geometry}{
A logical. Whether the function should export a geometry of pictures as POINT layer in .geojson format. Default is TRUE
}
  \item{fig.crs}{
A integer. Coordinate Reference System code for use when convert the coordinates from data.frame to sf
}
}

\author{
"Guimarães, Gustavo <gustavo234guimaraes@gmail.com>"
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
"exiftoolr"
}
\examples{

remotes::install_git("https://github.com/gustavo234guimaraes/ggperRPkgs.git")
library(ggperRPkgs)

coords<-extract_picture_coords("./image.jpg")

extract_picture_coords(
dir=getwd(),
fig.pattern = ".jpg|.JPG",
save.geometry = FALSE
)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
