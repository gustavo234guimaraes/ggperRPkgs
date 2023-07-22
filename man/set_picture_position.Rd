\name{set_picture_position}
\alias{set_picture_position}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Input GPS parameters in images
}
\description{
This function input a GPS Latitude, Longitude and Altitude in a image file using exiftool
}
\usage{
set_picture_position(
  fig.path="img1.png",
  fig.x=3,
  fig.y=-2
)

set_picture_position(
  data.frame(path="img1.png",x=1,y=1,z=1)
)


}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data.frame with collumns "path" with the directory of pictures and columns with geometries "x", "y" and "z (optional)".
}
  \item{fig.path}{
A character. With 
}
  \item{fig.x}{
A 
}


}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
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
