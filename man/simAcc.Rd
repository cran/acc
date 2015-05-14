\name{simAcc}
\alias{simAcc}
\title{
simAcc
}
\description{
Simulates accelerometer data
}

\usage{
simAcc(minutes,mvpaLevel,seedset)
}

\arguments{
  \item{minutes}{Number of observations to be generated.}
  \item{mvpaLevel}{Level of moderate-vigorous activity. Options: 'low', 'moderate', or 'high'.}
  \item{seedset}{Sets seed for random data generation. Defaults to 1234.}
}


\value{
A simulated dataset is returned with two columns:
[TimeStamp, counts] 
}

\author{
Jaejoon Song <jjsong2@mdanderson.org>
}


\keyword{ accelerometer}

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
mvpaLowData <- simAcc(minutes=(60*24*2),mvpaLevel='moderate')
summary <- acc(data=mvpaLowData)
summary$validDates
plotAcc(summary)

##
## Simulate a dataset for two days, for an individual with high MVPA level.
##
mvpaLowData <- simAcc(minutes=(60*24*2),mvpaLevel='high')
summary <- acc(data=mvpaLowData)
summary$validDates
plotAcc(summary)

}
