% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/url.R
\name{url_auth}
\alias{url_auth}
\title{Gets URL Authentication Token}
\usage{
url_auth(url, default = NA_character_, quiet = TRUE, ..., full = FALSE)
}
\arguments{
\item{url}{character vector of URLs}

\item{default}{default value to use for URLs for which no authentication token
can be found.}

\item{quiet}{logical that indicates to mute messages showing the matched machine
authentication}

\item{...}{other parmeters passed to internal function \code{match_url}.}

\item{full}{logical that indicates if the function should return the full
authentication details (machine, login, password) or only the password (i.e., token)}
}
\value{
a character vector of authentication tokens.
URLs for which no token was found get \code{NA} values.
}
\description{
Gets URL Authentication Token
}
