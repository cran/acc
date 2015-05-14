\name{plotAcc}
\alias{plotAcc}
\title{
plotAcc
}
\description{
Plots accelerometer data
}

\usage{
plotAcc(object,date)
}

\arguments{
  \item{object}{An object returned from either the function readRaw or from acc.}
  \item{date}{User can choose to mark a date. Date is specified in Y-m-d format. i.e., date="2007-03-29". Default is no date. This option is only available when choosing to plot object from readRaw.}
}
\value{
If the object is from readRaw:
A plot of counts is returned.

If the object is from acc:
A plot representing non-wear time, sedentary and moderate-vigorous activity is presented, along with a bout indicator. "In bout" means that the observation is counted as being in the bout.
}

\author{
Jaejoon Song <jjsong2@mdanderson.org>
}

\keyword{ accelerometer }

\examples{

##
## Simulate a dataset for two days, for an individual with low MVPA level.
##
mvpaLowData <- simAcc(minutes=(60*24*2),mvpaLevel='low')
summary <- acc(data=mvpaLowData)
summary$validDates
plotAcc(summary)

##
## Simulate a dataset for two days, for an individual with moderate MVPA level.
##
mvpaModData <- simAcc(minutes=(60*24*2),mvpaLevel='moderate')
summary <- acc(data=mvpaModData)
summary$validDates
plotAcc(summary)

##
## Simulate a dataset for two days, for an individual with high MVPA level.
##
mvpaHighData <- simAcc(minutes=(60*24*2),mvpaLevel='high')
summary <- acc(data=mvpaHighData)
summary$validDates
plotAcc(summary)

}

