% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildPackage.R
\name{writeCollate}
\alias{writeCollate}
\title{write cpllate field}
\usage{
writeCollate(package, path, trace = TRUE)
}
\arguments{
\item{package}{the name of the package}

\item{path}{the path to the directory containing the package}

\item{trace}{should the execution of the function be traced}
}
\description{
write thecpllate field in the DESCRIPTION file of a package
}
\examples{
\dontrun{
writeCollate("butils", path = pathGitHub())
}
}
