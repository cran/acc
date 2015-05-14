\name{acc}
\alias{acc}
\title{
acc
}
\description{
Summarized accelerometer data by day
}

\usage{
acc(data, tri, spuriousDef, nonwearDef, boutsizeSed, 
    toleranceSed, minWear, mvpa, boutsizeMVPA, toleranceMVPA)
}

\arguments{
  \item{data}{Data which consists of two columns [TimeStamp,counts]}
  \item{tri}{Whether the data is from a tri-axial accelerometer. Default is tri='FALSE'.}
  \item{spuriousDef}{Definition of spurious observation. Defined as minutes of consecutive zeros. For example, if spuriousDef = 20, this means that an observation point will be determined as a spurious observation if there are consequtive counts of at least 20 zeros before and after the single non-zero observation. Default is  spuriousDef = 20.}
  \item{nonwearDef}{Definition of non-wear time. Defined as minutes of consecutive zeros. For example, if nonwearDef=60, this means that a period will be defined as non-wear time if there are at least 60 consecutive zeros. Default is nonwearDef=60. To consider all observations as wear time specify nonwearDef='inf'}
  \item{boutsizeSed}{Bout size used in summarizing sedentary minutes per day. Default is boutsizeSed=10.}
  \item{toleranceSed}{Whether to allow some observations outside the cut point for sedentary activity within the bout. Default is toleranceSed='FALSE'.}
  \item{minWear}{Minimum wear time definition. Defined as minutes of wear time. or example, if minWear = 600, this means that a day will be considered valid only if the wear time is at least 600 minutes. Default is minWear = 600.}
  \item{mvpa}{Cut point for moderate-vigorous physical activity (MVPA). Options are   'Freedson'(mvpa = 1952), 'Swartz'(mvpa = 574), 'Copland' (mvpaCP = 1041), 'Hall' (mvpa = 809), or user can specify a numeric value instead (e.g. 1952). Default is mvpa='Freedson'.}
  \item{boutsizeMVPA}{Bout size used in summarizing MVPA minutes per day. Default is boutsizeMVPA = 10.}
  \item{toleranceMVPA}{Whether to allow up to two observations outside the cut point for MVPA within the bout. Default is toleranceMVPA='TRUE'.}
}


\value{
A list of three items are returned:

$totalDates
Lists all dates available in data

$validDates
Returns a summary of physical activity for valid days which consists of columns
[Date, SedentaryMinutes, wearTime, numberOfBoutsSed, mvpaMinutes, numberOfBoutsMVPA] 

$sedentary
Returns the original two columns in the input data along with indicators of whether the observation is considered as a sedentary activity, non-wear time, or in sedentary bout.This output can be used to track specific observations in the data to understand why the observation was flagged as a non-wear time or marked as being in bout. 
[TimeStamp counts inSedentary nonwear inboutSedentary]

$MVPA
Returns the original two columns in the input data along with indicators of whether the observation is considered as a MVPA activity, non-wear time, or in MVPA bout.This output can be used to track specific observations in the data to understand why the observation was flagged as a non-wear time or marked as being in bout. 
[TimeStamp counts inMVPA nonwear inboutMVPA]
}

\author{
Jaejoon Song <jjsong2@mdanderson.org>
}
\references{Choi, L., Liu, Z., Matthews, C.E. and Buchowski, M.S.  (2011). Validation of Accelerometer Wear and Nonwear Time Classification Algorithm. Med Sci Sports Exerc, 43(2):357-64.

Hall, K. S., Howe, C. A., Rana, S. R., Martin, C. L., and Morey, M. C. (2013). METs and Accelerometry of Walking in Older Adults: Standard versus Measured Energy Cost. Medicine and Science in Sports and Medicine, 45(3). 574-82.

Freedson, P., Melanson, E.,  and Sirard, J. (1998). Calibration of the Computer Sciences and Applications, Inc. accelerometer. Medicine and Science in Sports and Exercercise, 30(5):777-81.

Swartz, A. M., Strath, S. J., Bassett, D. R. Jr., O'Brien, W. L., King, G. A., and Ainsworth, B. E. (2000). Estimation of energy expenditure using CSA accelerometers at hip and wrist sites. Medicine and Science in Sports and Exercercise, 32: S450-456. 

Copeland, J. L., and Esliger, D. W. (2009). Accelerometer assessment of physical activity in active, healthy older adults. J Aging Phys Act, 17: 17-30. }

\keyword{ accelerometer}

\examples{
##
## Example 1: How to load the raw accelerometer data using readCountsData function
##            in the PhysicalActivity package
#library(PhysicalActivity)
#infile <- "RawDataName.dat"
#counts <- readCountsData(infile, ctPerSec = 1/60)


##
## Example 2: Summarizing, and plotting accelerometer data.
##            "A simulated data for a sedentary individual"  
##        

# For this example, data is generated using a Hidden Markov model
# First, a sequence of time is generated
randomTime <- seq(ISOdate(2015,4,1),ISOdate(2015,4,3),"min")
# Load the mhsmm package to generate data using a Hidden Makov model
library(mhsmm)
# It is assumed that the counts are generated from a Hidden Markov model 
# with three states, being non-wear, sedentary, and moderate-vigorous activity
J <- 3; initial <- rep(1/J, J)
# Set up a transition matrix for the Hidden Markov model.
P <- matrix(c(0.95, 0.04, 0.01, 
             0.09, 0.9, 0.01, 
             0.1, 0.2, 0.7), byrow='TRUE',nrow = J)
# It is assumed that the counts are realized from a mixture of
# two normal distributions (for sedentary activity and mvpa) 
# and a constant at zero (for non-wear time).
b <- list(mu = c(0, 30, 2500), sigma = c(0, 30, 1000))
model <- hmmspec(init = initial, trans = P, parms.emission = b,dens.emission = dnorm.hsmm)
# Generate data!
train <- simulate(model, nsim = (60*24*2), seed = 1234, rand.emis = rnorm.hsmm)
# Now set up a dataset that mimicks the accelerometry data
counts <- data.frame(TimeStamp = randomTime[1:length(train$x)], counts = train$x)
library(acc)
# summarize the data using the acc function
summary <- acc(data=counts)
summary$validDates
# plot it
plotAcc(summary)
# Can try out other options for the acc function
# By specifing a very high cut-point for mvpa
summary <- acc(data=counts,mvpa=3000)
summary$validDates
# By specifying nonwearDef='inf', to consider all observations as wear time
summary <- acc(data=counts,mvpa="Freedson",nonwearDef='inf')
summary$validDates


##
## Example 3: Summarizing, and plotting accelerometer data
##            "A simulated data for an active individual"  
##

randomTime <- seq(ISOdate(2015,4,1),ISOdate(2015,4,3),"min")
library(mhsmm)
J <- 3; initial <- rep(1/J, J)
P <- matrix(c(0.95, 0.04, 0.01, 
             0.09, 0.7, 0.21, 
             0.1, 0.1, 0.8), byrow='TRUE',nrow = J)
b <- list(mu = c(0, 30, 2500), sigma = c(0, 30, 1000))
model <- hmmspec(init = initial, trans = P, parms.emission = b,dens.emission = dnorm.hsmm)
train <- simulate(model, nsim = (60*24*2), seed = 1234, rand.emission = rnorm.hsmm)

counts <- data.frame(TimeStamp = randomTime[1:length(train$x)], counts = train$x)
library(acc)
summary <- acc(data=counts)
summary$validDates
plotAcc(summary)

}
