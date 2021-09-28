\name{quadratMap}
\alias{quadratMap}
\docType{data}
\title{
  Map of the Quadrats ("Grid Cells") Used to Fit the Model of Folliot et al. (in prep.).
}
\description{
  This dataset is an object of class \code{sf} from the package sf,
  containing the map of Europe.
}
\usage{data("quadratMap")}
\format{
  A data frame with 193 observations on the following 2 variables.
  \describe{
    \item{\code{idg}}{the ID of the grid cell}
    \item{\code{gr}}{a sfc_POLYGON}
  }
}
\references{

  Folliot, B., Calenge, C., Caizergues, A., Tableau, A., Souchay, G.,
  Guillemain, M. and Champagnon, J. (in prep.) Assessing spatio-temporal
  variation in abundance: a flexible framework accounting for sampling
  bias with an application to Vulnerable Common Pochard (Aythya ferina).

}
\examples{

plot(euromap, col="grey")
plot(quadratMap, col="yellow", add=TRUE)

}
\keyword{datasets}
