\name{fitModel}
\alias{fitModel}
\alias{summary.modelPochard}
\alias{print.modelPochard}
\title{
  Fit the Mixed Model Used to Estimate Pochard Population Trends
}
\description{
  This is the main function of the package: it estimates all the
  parameters from the model of Folliot et al. (in prep.).
}
\usage{

fitModel(count, site, year, quadrat, latQuadrat, sv, control = list())

\method{summary}{modelPochard}(object, \dots)

\method{print}{modelPochard}(x, \dots)

}
\arguments{
  \item{count}{
    A vector containing the results of the counts
  }
  \item{site}{
    A factor indicating the site corresponding to each count in \code{count}
  }
  \item{year}{
    A numeric vector containing the year corresponding to each count in \code{count}
  }
  \item{quadrat}{
    A factor indicating the quadrat (called "grid cell" in the paper)
  corresponding to the site in \code{site} 
  }
  \item{latQuadrat}{
    A numeric vector containing the latitude of the quadrat (called
  "grid cell" in the paper) in \code{quadrat}
  }
  \item{sv}{
    Optionally, a list with named elements giving the starting values
    for the parameters of the model (see details for more information on
    the name of the parameters).
  }
  \item{control}{
    A list can be passed to the function \code{optim} (see the help page
    of this function), controlling the optimization algorithm.
  }
  \item{object,x}{
    An object of class \code{modelPochard} returned by the function
    \code{fitModel}. 
  }
  \item{\dots}{
    Additional arguments to be passed to and from other functions.
  }
}
\details{

  This function fits the model described in Folliot et al. (in prep.) to
  a dataset with a structure similar to the dataset \code{pochard} (see
  the help page of this dataset).  The parameters of the model of counts
  are the following:

  \itemize{
    \item \code{site_effect}: site-specific intercept;
    \item \code{beta0}: fixed slope for the year;
    \item \code{di}: random site effect for the slope of the year;
    \item \code{bqi}: mean random effect for the grid cell;
    \item \code{eit}: overdispersion residual;
    \item \code{latitude_effect}: effect of the latitude on the bqi;
    \item \code{log_sigmabqi}: logarithm of the standard deviation of
    the random effects \code{bqi};
    \item \code{log_sigmadi}: logarithm of the standard deviation of the
    random effects \code{di};
    \item \code{log_sigmae}: logarithm of the standard deviation of the
    overdispersion residuals;    
  }

  
}
\value{
  The function \code{fitModel} returns an object of class
  \code{modelPochard}, containing the following elements:
  \item{fixed}{A list containing the value of the fixed effects (one
    element = one parameter)}
  \item{se.fixed}{A list containing the value of the standard error of
    the fixed effects (one element = one parameter)}
  \item{random}{A list containing the values of the random effects}
  \item{rawTMB}{the results of the function \code{sdreport} of the
    package \code{TMB} used to fit the model.}
  
}
\references{

  Folliot, B., Calenge, C., Caizergues, A., Tableau, A., Souchay, G.,
  Guillemain, M. and Champagnon, J. (in prep.) Assessing spatio-temporal
  variation in abundance: a flexible framework accounting for sampling
  bias with an application to Vulnerable Common Pochard (Aythya ferina).

}
\author{
  Clement Calenge \email{clement.calenge@ofb.gouv.fr}
}
\seealso{
  \code{\link{pochard}} for a description of the dataset used in the
  paper.
}
\examples{

\dontrun{

## Fit the model to the dataset
## Warning: extremely slow !!!!

fittedModel <- fitModel(pochard$count, pochard$site, pochard$year,
                        pochard$idg, pochard$latq)

}

## The result is available as a dataset of the package
fittedModel

}
\keyword{model}
