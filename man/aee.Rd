\name{aee}
\alias{aee}

\title{
Creates an aee object
}
\description{
Creates an aee object for semiparametric regression with augmented estimating equation. 
}
\usage{
aee(ID, time, minutes)
}

\arguments{
  \item{ID}{Individual identifier}
  \item{time}{Observation time}
  \item{minutes}{Minutes of physical activity}
}

\value{
\item{psDF}{A data frame, part of original input data frame with variable "ID", "time" and "count"}
\item{timeGrid}{Ordered distinct observation times in the set of all observation times}
\item{panelMatrix}{a matrix representation of panel count data, one row per subject, one column per time point in "timeGrid"}
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
\keyword{ aee }
