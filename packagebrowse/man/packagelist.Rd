\name{packagelist}
\alias{packagelist}
\title{A demo for downloading a frame of R packages.}
\description{
This package provides a demo for downloading a frame of R packages from <https://cran.r-project.org/web/packages/> sorted by date of publication or name.
}
\usage{
packagelist(package_list_order,keyword)
}
\arguments{
  \item{package_list_order}{order of the frame,"date" is selected by default,creating an R object.If you choose "name", the frame will be sorted by name.}
  \item{keyword}{make the frame screen by keyword,hold the ranks including keyword}
}
\details{
In this version ,there may be duplicate value in the data.frame.
}
\value{
A data.frame of R packages sorted by name
}
\references{
https://cran.r-project.org/web/packages/
}
\author{
Summer Zhang
}
\note{
This is a function which I making for practicing ,  pls forgive my poor coding skill and english,thx.
}
\seealso{
nothing
}
\examples{
packagelist()
}
\keyword{ ~package }

