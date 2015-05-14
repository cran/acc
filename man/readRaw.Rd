\name{readRaw}
\alias{readRaw}
\title{
readRaw
}
\description{
Reads raw files in .dat format for Actigraph GT1M and GT3X devices. Device type and epoch is automatically detected and reported in the console.
}

\usage{
readRaw(filename)
}

\arguments{
  \item{filename}{Speficy full file path and file name. e.g. C:/mydata.dat}
}


\value{

For uni-axial accelerometer (GT1M), two columns are returned, consisting of: [TimeStamp,Counts]
For tri-axial accelerometer (GT3X), four columns are returned, consisting of: [TimeStamp,x,y,z]
}

\author{
Jaejoon Song <jjsong2@mdanderson.org>
}


\keyword{ accelerometer}

\examples{

##
## A example to read raw data
##
# readRaw("C:/mydata.dat")


}
