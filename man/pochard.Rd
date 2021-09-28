\name{pochard}
\alias{pochard}
\docType{data}
\title{
 Count Data of Pochard in Northern Europe
}
\description{
  This is the dataset used in Folliot et al. (in prep.).
}
\usage{data("pochard")}
\format{
  A data frame with 9446 observations on the following 6 variables.
  \describe{
    \item{\code{count}}{A vector containing the results of the counts}
    \item{\code{site}}{A factor indicating the site corresponding to
      each count in \code{count}}
    \item{\code{year}}{A numeric vector containing the year corresponding
      to each count in \code{count}}
    \item{\code{idg}}{A factor indicating the quadrat (called
      "grid cell" in the paper) corresponding to
      the site in \code{site}. This id can be used to identify the
      quadrat in the \code{quadratMap} dataset.}
    \item{\code{longq}}{A numeric vector containing the longitude of
      the quadrat (called "grid cell" in the paper) in \code{quadrat}}
    \item{\code{latq}}{A numeric vector containing the latitude of
      the quadrat (called "grid cell" in the paper) in \code{quadrat}}
  }
}
\source{
  
  International Waterbird Census, Wetlands International.  P.O. Box 471,
  6700 AL Wageningen.  The Netherlands.

}
\references{

  Folliot, B., Calenge, C., Caizergues, A., Tableau, A., Souchay, G.,
  Guillemain, M. and Champagnon, J. (in prep.) Assessing spatio-temporal
  variation in abundance: a flexible framework accounting for sampling
  bias and an application to Vulnerable Common Pochard (Aythya ferina).

}
\examples{

head(pochard)

}
\keyword{datasets}
