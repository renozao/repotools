% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remote-auth.R
\name{remote_auth}
\alias{remote_auth}
\title{Resolve Authentication Credentials from Remotes}
\usage{
remote_auth(x, ...)
}
\arguments{
\item{x}{a \code{remote} S3 object as built by \code{devtools:::remote}.}

\item{...}{extra arguments passed to \link{url_auth}}
}
\value{
the remote object with updated credentials.
}
\description{
Looks for suitable authentication tokens in environment or .netrc file for
some type of remotes.
}
