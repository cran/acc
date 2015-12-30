\name{simAcc}
\alias{simAcc}
\title{
Simulates accelerometer data based on a hidden Markov model
}
\description{
Simulates accelerometer data. The simulation function is based on a hidden Markov model, as described in the example for function acc. This function is provided for convenience to generate data from a pre-specified transition probabilities to mimic activity levels of low, moderate and high. To generate data from a specific transition probabilities and distributions, please refer to the example for function acc.
}

\usage{
simAcc(minutes,mvpaLevel,seedset)
}

\arguments{
  \item{minutes}{Number of observations to be generated.}
  \item{mvpaLevel}{Level of moderate-vigorous activity. Options: `low', `moderate', or `high'. Low specifies a hidden markov model with transition probabilities 0.95, 0.04, 0.01, 0.09, 0.9, 0.01, 0.1, 0.2, 0.7, respectively for P11, P12, P13, P21, P22, P23, P31, P32, P33, respectively. Moderate specifies a hidden markov model with transition probabilities 0.95, 0.04, 0.01, 0.09, 0.8, 0.11, 0.1, 0.1, 0.8 respectively for P11, P12, P13, P21, P22, P23, P31, P32, P33, respectively. High specifies a hidden markov model with transition probabilities 0.95, 0.04, 0.01, 0.09, 0.7, 0.21, 0.1, 0.1, 0.8, respectively for P11, P12, P13, P21, P22, P23, P31, P32, P33, respectively. For all levels, it is assumed that the counts are realized from a mixture of two normal distributions (for sedentary activity and mvpa)  and a constant at zero (for non-wear time), with means mu = c(0, 30, 2500) and variance  sigma = c(0, 30, 1000).}
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
## Example: Simulate a dataset for two days, for an individual with low MVPA level.
##
mvpaLowData <- simAcc(minutes=(60*24*2),mvpaLevel='low')
summary <- acc(data=mvpaLowData, tri='FALSE', axis=NULL,
                     spuriousDef=20, nonwearDef=60, minWear=600, 
                     patype=c('Sedentary','MVPA'),pacut=c(c(0,99),c(1952,Inf)), 
                     boutsize=c(10,10), tolerance=c('FALSE','TRUE'))
summary

##
## Example: Simulate a dataset for two days, for an individual with moderate MVPA level.
##
mvpaLowData <- simAcc(minutes=(60*24*2),mvpaLevel='moderate')
summary <- acc(data=mvpaLowData, tri='FALSE', axis=NULL,
                     spuriousDef=20, nonwearDef=60, minWear=600, 
                     patype=c('Sedentary','MVPA'),pacut=c(c(0,99),c(1952,Inf)), 
                     boutsize=c(10,10), tolerance=c('FALSE','TRUE'))
summary

##
## Example: Simulate a dataset for two days, for an individual with high MVPA level.
##
mvpaLowData <- simAcc(minutes=(60*24*2),mvpaLevel='high')
summary <- acc(data=mvpaLowData, tri='FALSE', axis=NULL,
                     spuriousDef=20, nonwearDef=60, minWear=600, 
                     patype=c('Sedentary','MVPA'),pacut=c(c(0,99),c(1952,Inf)), 
                     boutsize=c(10,10), tolerance=c('FALSE','TRUE'))
summary

}
