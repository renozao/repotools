\name{repos.getOption}
\alias{repos.getOption}
\alias{repos.options}
\title{Package Options for repotools}
\usage{
  repos.getOption(x, default = NULL)

  repos.options(...)
}
\arguments{
  \item{x}{a character string holding an option name.}

  \item{default}{if the specified option is not set in the
  options list, this value is returned.  This facilitates
  retrieving an option and checking whether it is set and
  setting it separately if not.}

  \item{...}{any options can be defined, using \code{name =
  value}.  However, only the ones below are used in base
  \R.

  Options can also be passed by giving a single unnamed
  argument which is a named list.  }
}
\description{
  These functions behave like the base functions
  \code{\link{options}} and \code{\link{getOption}}, and
  provide access to options that are specific to the
  \pkg{repotools} package.
}

