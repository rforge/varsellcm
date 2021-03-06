% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Extractors.R
\docType{methods}
\name{BIC}
\alias{BIC}
\alias{BIC,VSLCMresults-method}
\title{BIC criterion.}
\usage{
\S4method{BIC}{VSLCMresults}(object)
}
\arguments{
\item{object}{instance of \code{\linkS4class{VSLCMresults}}.}
}
\description{
This function gives the BIC criterion of an instance of \code{\linkS4class{VSLCMresults}}. 
BIC is computed according to the formula \deqn{BIC=log-likelihood - 0.5*\nu*log(n)} 
where  \eqn{\nu} denotes the number of parameters in the fitted model and \eqn{n} represents the sample size.
}
\examples{
# Data loading:
data(heart)

# Cluster analysis without variable selection (number of clusters between 1 and 3)
res<- VarSelCluster(heart[,-13], 2, vbleSelec = FALSE)

# Get the BIC value
BIC(res)
}
\references{
Schwarz, G. (1978). Estimating the dimension of a model. Annals of Statistics, 6(2), 461-464.
}
