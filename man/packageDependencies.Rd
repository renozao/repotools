\name{packageDependencies}
\alias{packageDependencies}
\title{List Package Dependencies}
\usage{
packageDependencies(x, all = FALSE, available = NULL, missing.only = FALSE, recursive = FALSE, as.list = FALSE, names.only = TRUE, rm.base = TRUE)
}
\arguments{
\item{x}{path to package source directory or file.}

\item{all}{logical that indicates if all dependencies should be returned,
or only the required ones.}

\item{available}{a matrix of available packages (as returned by \code{\link{available.packages}}),
from which the dependencies are retrieved.
This means that there must be a row for the package \code{x}.}

\item{missing.only}{logical that indicates if only non-installed dependencies should be included in the
result}

\item{recursive}{logical that indicates if indirect dependencies should also be included.}

\item{as.list}{logical that indicates if the result should be a list with one element
per type of dependency.}

\item{names.only}{logical that indicates if the result should only include the dependencies package names}

\item{rm.base}{logical that indicates if base packages -- that come installed with any R installation --
should be exlcuded from the result.}
}
\description{
List Package Dependencies
}

