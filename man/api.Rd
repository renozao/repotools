\name{available.pkgs}
\alias{available.pkgs}
\alias{download.pkgs}
\alias{exists.pkgs}
\alias{Library}
\alias{old.pkgs}
\alias{update.pkgs}
\title{Working with Packages}
\usage{
  available.pkgs(...)

  download.pkgs(pkgs, destdir, available = NULL, ...)

  exists.pkgs(pkgs, repos = getOption("repos"), ...,
    value = FALSE, fixed = TRUE)

  old.pkgs(lib.loc = NULL, repos = getOption("repos"),
    available = NULL, ..., type = getOption("pkgType"),
    verbose = TRUE)

  update.pkgs(lib.loc = NULL, repos = getOption("repos"),
    instlib = NULL, ask = TRUE, available = NULL,
    oldPkgs = NULL, ..., type = getOption("pkgType"),
    dry.run = NULL, verbose = TRUE)

  Library(package, lib = NULL, ...)
}
\arguments{
  \item{fixed}{logical that indicates if the elements of
  \code{x} should match exactly (\code{TRUE}) or
  interpreted and matched as regular expressions.}

  \item{value}{logical, used only when \code{fixed=FALSE},
  that indicates if the name of the matched packages should
  be returned (instead of a \code{TRUE/FALSE} logical
  value.}

  \item{ask}{logical that specifies if the user should be
  asked before installling the available updates, or if
  these should be directly installed.}

  \item{...}{ additional arguments to be passed to
  \code{\link{download.file}}.  }

  \item{pkgs}{ character vector of the names of packages
  whose latest available versions should be downloaded from
  the repositories.  }

  \item{destdir}{ directory where downloaded packages are
  to be stored.  }

  \item{available}{ an object as returned by
  \code{\link{available.packages}} listing packages
  available at the repositories, or \code{NULL} which makes
  an internal call to \code{available.packages}.  }

  \item{repos}{ character vector, the base URL(s) of the
  repositories to use, i.e., the URL of the CRAN master
  such as \code{"http://cran.r-project.org"} or its Statlib
  mirror, \code{"http://lib.stat.cmu.edu/R/CRAN"}.  }

  \item{type}{ character string, indicate which type of
  packages: see \code{\link{install.packages}}.  }

  \item{lib.loc}{a character vector describing the location
  of \R library trees to search through, or \code{NULL}.
  The default value of \code{NULL} corresponds to all
  libraries currently known to \code{\link{.libPaths}()}.
  Non-existent library trees are silently ignored.}

  \item{verbose}{a logical.  If \code{TRUE}, additional
  diagnostics are printed.}

  \item{package}{the name of a package, given as a
  \link{name} or literal character string, or a character
  string, depending on whether \code{character.only} is
  \code{FALSE} (default) or \code{TRUE}).}
}
\description{
  \code{available.pkgs} returns a matrix of the packages
  available in given repositories.

  \code{download.pkgs} downloads packages.

  \code{exists.pkgs} tells if packages are available from
  repositories.

  \code{Library} tries loading packages and install them if
  needed.
}

