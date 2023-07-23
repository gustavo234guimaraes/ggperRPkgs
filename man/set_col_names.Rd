\name{set_col_names}
\alias{set_col_names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set column names of a data.frame
}
\description{
This functions set column names of a data.frame
}
\usage{
set_col_names(x,names)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A data.frame, tbl or matrix
}
  \item{names}{
A character. A character vector with names to set in x, the length of names need be the same of number of columns of the x
}
}

\author{
"Guimarães, Gustavo <gustavo234guimaraes@gmail.com>"
}

\examples{

data<-read.csv(path)

set_col_names(data,c("Col1","Col2"))

data<-data %>% 
  filter(V1<10) %>% 
  set_col_names(c("Col1","Col2"))

}
