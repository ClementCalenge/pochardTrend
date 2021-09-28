\name{pochardTrend-package}
\alias{pochardTrend-package}
\alias{pochardTrend}
\docType{package}
\title{
  \packageTitle{pochardTrend}
}
\description{
  \packageDescription{pochardTrend}
}
\details{

  The most important function in this package is \code{fitModel}, which
  describes how to fit the model. See also the vignette of the package.

}
\author{
\packageAuthor{pochardTrend}

Maintainer: \packageMaintainer{pochardTrend}
}
\references{

  Folliot, B., Calenge, C., Caizergues, A., Tableau, A., Souchay, G.,
  Guillemain, M. and Champagnon, J. (in prep.) Assessing spatio-temporal
  variation in abundance: a flexible framework accounting for sampling
  bias and an application to Vulnerable Common Pochard (Aythya ferina).


}
\keyword{ package }
\seealso{

  \code{\link{fitModel}}
}
\examples{

\dontrun{

## The following command was used to fit the model to the dataset pochard
## Warning: extremely slow !!!!

fittedModel <- fitModel(pochard$count, pochard$site, pochard$year,
                        pochard$idg, pochard$latq)

}

## Which gives the following dataset (available
## as a dataset of the package)
fittedModel

}
