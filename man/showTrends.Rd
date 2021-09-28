\name{showTrends}
\alias{showTrends}
\title{
  Show the Trends Estimated in an Object of Class modelPochard
}
\description{
  This function shows the trends estimated in an object of class modelPochard
}
\usage{
showTrends(model, year, lat, nrep = 1000, levelIC = c(0.025, 0.975),
           addSite = TRUE, addQuadrat = TRUE)
}
\arguments{
  \item{model}{
    An object of class \code{modelPochard}.
  }
  \item{year}{
    a numeric vector containing the years over which the trends should
    be displayed.
  }
  \item{lat}{
    a vector of latitudes for which the trends should be displayed.
  }
  \item{nrep}{
    a numeric value, indicating how many simulations of the random
    effects should be carried out to calculate the limits of the
    confidence intervals.
  }
  \item{levelIC}{
    a vector of length 2 indicating the probability limits of the
    confidence intervals. 
  }
  \item{addSite}{
    logical value indicating whether the plot should display a
    prediction interval including the
    between sites variation of the estimated trend.
  }
  \item{addQuadrat}{
    logical value indicating whether the plot should display a
    prediction interval including the
    between grid cell ("quadrat") variation of the estimated trend.
    
  }
}
\value{
  a graphical object of class \code{ggplot}
}
\references{

  Folliot, B., Calenge, C., Caizergues, A., Tableau, A., Souchay, G.,
  Guillemain, M. and Champagnon, J. (in prep.) Quantifying
  spatio-temporal variation in abundance: a flexible framework
  accounting for sampling bias.

}
\author{

    Clement Calenge \email{clement.calenge@ofb.gouv.fr}
  
}
\seealso{
  \code{\link{fitModel}} for a description of models of class \code{modelPochard}.
}
\examples{

showTrends(fittedModel, 2002:2012, c(46, 50,55, 60), nrep=100)

}
\keyword{hplot }
