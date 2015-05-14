\name{rtb}
\alias{rtb}
\title{
rtb
}
\description{
Summarizes accelerometer data by real time bouts
}

\usage{
rtb(data, type, tri, spuriousDef, nonwearDef, 
                boutsizeSed, toleranceSed,  
                mvpa, boutsizeMVPA, toleranceMVPA)
}

\arguments{
  \item{type}{Whether to return bouts information for sedentary or MVPA. Choose either 'sedentary' or 'MVPA'}
  \item{data}{Data which consists of two columns [TimeStamp,counts]}
  \item{tri}{Whether the data is from a tri-axial accelerometer. Default is tri='FALSE'.}
  \item{spuriousDef}{Definition of spurious observation. Defined as minutes of consecutive zeros. For example, if spuriousDef = 20, this means that an observation point will be determined as a spurious observation if there are consequtive counts of at least 20 zeros before and after the single non-zero observation. Default is  spuriousDef = 20.}
  \item{nonwearDef}{Definition of non-wear time. Defined as minutes of consecutive zeros. For example, if nonwearDef=60, this means that a period will be defined as non-wear time if there are at least 60 consecutive zeros. Default is nonwearDef=60. To consider all observations as wear time specify nonwearDef='inf'}
  \item{boutsizeSed}{Bout size used in summarizing sedentary minutes per day. Default is boutsizeSed=10.}
  \item{toleranceSed}{Whether to allow some observations outside the cut point for sedentary activity within the bout. Default is toleranceSed='FALSE'.}
  \item{mvpa}{Cut point for moderate-vigorous physical activity (MVPA). Options are   'Freedson'(mvpa = 1952), 'Swartz'(mvpa = 574), 'Copland' (mvpaCP = 1041), 'Hall' (mvpa = 809), or user can specify a numeric value instead (e.g. 1952). Default is mvpa='Freedson'.}
  \item{boutsizeMVPA}{Bout size used in summarizing MVPA minutes per day. Default is boutsizeMVPA = 10.}
  \item{toleranceMVPA}{Whether to allow up to two observations outside the cut point for MVPA within the bout. Default is toleranceMVPA='TRUE'.}
}


\value{
Two columns are returned, consisting of:

TimeStamp
Time stamp of date and time

Minutes
Minutes of either sedentary activity or MVPA for the observed bout
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
summarySed <- rtb(data=mvpaLowData,type='sedentary')
summarySed
summaryMVPA <- rtb(data=mvpaLowData,type='mvpa')
summaryMVPA


##
## Simulate a dataset for two days, for an individual with high MVPA level.
##
mvpaHighData <- simAcc(minutes=(60*24*2),mvpaLevel='high')
summarySed <- rtb(data=mvpaHighData,type='sedentary')
summarySed
summaryMVPA <- rtb(data=mvpaHighData,type='mvpa')
summaryMVPA


}
