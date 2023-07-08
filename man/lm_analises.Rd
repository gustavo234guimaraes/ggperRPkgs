\name{lm_analises}
\alias{lm_analises}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
"Run a linear model and export analysis"
}
\description{
"This model run a linear model of a simple multinomial regression and export the results and a analyse of a model assumptions"
}
\usage{
lm_analises(
f.model = Y~x1+x2,
data_ = data.frame(),
output.path = getwd(),
show.progress = TRUE,
plot.labels = "Model one",
force_positive = TRUE,
weigth_negative = 3
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f.model}{
An object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. For details of model specification view lm function
}
  \item{data_}{
A data frame or environment containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which lm is called
}
  \item{output.path}{
A character. Directory for save the outputs, graphs and model
}
  \item{show.progress}{
A logical. When TRUE the progress of function is showded
}
  \item{plot.labels}{
A character. The label for complement the graphs, If plot.labels="de Produção" the plot label will be "Resíduos do modelo de Produção"
}
  \item{force_positive}{
A logical. When TRUE tries force positive values of explained variable using weights
}
  \item{weight_negative}{
A numeric. When force_positive = TRUE, a numeric value greater than 1 to use as weight multiplying the error terms in optimization algorithm
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
lm, glm
}
\examples{


remotes::install_git("https://github.com/gustavo234guimaraes/ggperRPkgs.git")
library(ggperRPkgs)

lm_analises()

lm_analises(
f.model=Ozone~0+Solar.R+Temp,
data_=airquality,
output.path=getwd(),
show.progress=TRUE,
plot.labels="de emissão",
force_positive=FALSE,
weight_negative=NULL
)

lm_analises(
f.model=Ozone~0+Solar.R+Temp,
data_=airquality,
output.path=getwd(),
show.progress=TRUE,
plot.labels="de emissão",
force_positive=TRUE,
weight_negative=15
)

}


