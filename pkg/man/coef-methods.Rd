% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Extractors.R
\docType{methods}
\name{coef}
\alias{coef}
\alias{coef,VSLCMresults-method}
\title{Extract the parameters}
\usage{
\S4method{coef}{VSLCMresults}(object)
}
\arguments{
\item{object}{instance of \code{\linkS4class{VSLCMresults}}.}
}
\description{
This function returns an instance of class \code{\linkS4class{VSLCMparam}} which contains the model parameters.
}
\examples{
# Data loading:
data(heart)

# Cluster analysis without variable selection (number of clusters between 1 and 3)
res  <- VarSelCluster(heart[,-13], 1:3, vbleSelec = FALSE)

# Get the ICL value
coef(res)
}
