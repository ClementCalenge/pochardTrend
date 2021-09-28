\name{fittedModel}
\alias{fittedModel}
\docType{data}
\title{
  Model Fitted in the paper of Folliot et al. (in prep.)
}
\description{
  This dataset contains the model fitted in Folliot et al. (in
  prep.). It was obtained with the function \code{fitModel} used on the
  original dataset.  Due to copyright issue we could not include the
  census dataset in the package, but the simulated dataset
  \code{pochard} of the package has the same properties (see
  Examples).
}
\usage{data("pochard")}
\format{
  
  This object is an object of class
  \code{modelPochard}, containing the following elements:
  \describe{
    \item{\code{fixed}}{A list containing the value of the fixed effects
      in the model (one element = one parameter)}
    \item{\code{se.fixed}}{A list containing the value of the standard error of
      the fixed effects (one element = one parameter)}
    \item{\code{random}}{A list containing the values of the random effects}
    \item{\code{rawTMB}}{the results of the function \code{sdreport} of the
      package \code{TMB} used to fit the model.}
  } 
}
\references{
  
  Folliot, B., Calenge, C., Caizergues, A., Tableau, A., Souchay, G.,
  Guillemain, M. and Champagnon, J. (in prep.) Assessing spatio-temporal
  variation in abundance: a flexible framework accounting for sampling
  bias with an application to Vulnerable Common Pochard (Aythya ferina).
  
}
\examples{

\dontrun{

## The following command was used to fit the model to the dataset pochard
## Warning: extremely slow !!!!

fittedModel <- fitModel(pochard$count, pochard$site, pochard$year,
                        pochard$quadrat, pochard$latQuadrat)

}

## Which gives an object similar to
fittedModel

## Note that this is not exactly the same object, as fittedModel was
## fitted to the original dataset, and as because of copyright issues
## we could not include the original dataset in the package. The
## dataset pochard is a simulated dataset with similar properties.

}
\keyword{datasets}
