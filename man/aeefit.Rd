\name{aeefit}
\alias{aeefit}

\title{
Fits semiparametric regression models for irregularly observed physical activity data
}
\description{
Fits semiparametric regression models for irregularly observed physical activity data.
}
\usage{
aeefit(formula, data, weight, control=list())
}

\arguments{
  \item{formula}{A formula object as returned by aee.}
  \item{data}{A data frame which includes individuals' ID, observation times, and minutes of physical activity since the last observation time.}
  \item{weight}{A vector of sampling weights, for each individual.By default, no sampling weights are applied.}
  \item{control}{A list of control parameters. See 'Details'.}
}

\details{
The control argument is a list athat can supply any of the following components:
\itemize{
\item betaInit: Initial value for covariate coefficient, default is 0.

\item interval: Initial search interval for solving beta. Default is (-5,5).

\item maxIter: Maximum iterations allowed. Default is 150.

\item absTol: Absolute tolerance. Default is 1e-6.

\item relTol: Relative tolerance. Default is 1e-6.

\item a: A tune parameter. Default is .1. In case of gamma fraility, "a" corresponds to the value of both shape and rate parameters.
}

}

\references{
Wang, X. and Yan, J. (2011). Fitting semiparametric regressions for panel count survival data with an R package spef. Computer Methods and Programs in Biomedicine, 104, 278-285.

Wang, X., Ma, S., and Yan, J. (2013). Augmented estimating equations for semiparametric panel count regression with informative observation times and censoring time. Statistica Sinica, 23, 359-381.
}
\author{
Jaejoon Song <jjsong2@mdanderson.org>
}


\examples{
\dontrun{
data(NHANES)
# Example of analyzing NHANES data 
# Example 1: Not adjusted for sampling weights
nhanesToFit <- NHANES[ which(NHANES$mvpaMinutes!=0), ]
formula <- aee(ID, Day, mvpaMinutes) ~ Age+raceBi+VO2max+Gender
fitted1 <- aeefit(formula=formula, data=nhanesToFit) 
summary(fitted1)

# Example 2: Adjusted for sampling weights
data(weight)
nhanesToFit <- NHANES[ which(NHANES$mvpaMinutes!=0), ]
nhanesUnique <- nhanesToFit[!duplicated(nhanesToFit$ID),]
weightUnique <- merge(nhanesUnique, weight, by="ID")
selProb=(1/weightUnique$weight)
formula <- aee(ID, Day, mvpaMinutes) ~ Age+raceBi+VO2max+Gender
fitted2 <- aeefit(formula=formula, data=nhanesToFit, weight = selProb) 
summary(fitted2)
}
}
\keyword{ aeefit }
