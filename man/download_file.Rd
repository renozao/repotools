\name{download_file}
\alias{download_file}
\title{Downloading Files From Password Protected Directories}
\usage{
  download_file(url, destfile, ...)
}
\arguments{
  \item{...}{arguments passed to
  \code{\link{download.file}}.}

  \item{url}{A character string naming the URL of a
  resource to be downloaded.}

  \item{destfile}{A character string with the name where
  the downloaded file is saved.  Tilde-expansion is
  performed.}
}
\description{
  Downloads files using a custom \code{curl} binary that
  uses \pkg{RCurl} and is able to download files from
  password protected directories.
}

