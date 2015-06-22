\name{readRawBatch}
\alias{readRawBatch}
\title{
Reads multiple raw accelerometer data in a folder
}
\description{
Reads multiple raw accelerometer data in a folder. 
}

\usage{
readRawBatch(path)
}

\arguments{
  \item{path}{Path to a folder which contains accelerometer data in .dat format.}
}


\value{
A folder `readfiles' is created inside the specified 'path'. In the folder, files are saved by the same filenames as in the raw data.

For uni-axial accelerometer (GT1M), two columns are returned, consisting of: [TimeStamp,Counts]
For tri-axial accelerometer (GT3X), four columns are returned, consisting of: [TimeStamp,x,y,z]
}

\author{
Jaejoon Song <jjsong2@mdanderson.org>
}


\keyword{ accelerometer}

\examples{
##
## Example
##
# filepath to locate the raw accelerometer files
\dontrun{
mypath <- "C:/Accelerometry files"
readRawBatch(mypath)
}
}
