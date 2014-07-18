\name{install.dependencies}
\alias{install.dependencies}
\title{Installing All Package Dependencies}
\usage{
  install.dependencies(pkg, dependencies = NA, ...,
    verbose = TRUE, dry.run = FALSE)
}
\arguments{
  \item{pkg}{package name, path or source file}

  \item{...}{extra arguments passed to
  \code{\link{install.pkgs}}.}

  \item{dependencies}{logical indicating to also install
  uninstalled packages which these packages depend on/link
  to/import/suggest (and so on recursively).  Not used if
  \code{repos = NULL}.  Can also be a character vector, a
  subset of \code{c("Depends", "Imports", "LinkingTo",
  "Suggests", "Enhances")}.

  Only supported if \code{lib} is of length one (or
  missing), so it is unambiguous where to install the
  dependent packages.  If this is not the case it is
  ignored, with a warning.

  The default, \code{NA}, means \code{c("Depends",
  "Imports", "LinkingTo")}.

  \code{TRUE} means to use \code{c("Depends", "Imports",
  "LinkingTo", "Suggests")} for \code{pkgs} and
  \code{c("Depends", "Imports", "LinkingTo")} for added
  dependencies: this installs all the packages needed to
  run \code{pkgs}, their examples, tests and vignettes (if
  the package author specified them correctly).

  In all of these, \code{"LinkingTo"} is omitted for binary
  packages.  }

  \item{verbose}{verbosity level (logical or numeric)}

  \item{dry.run}{logical that indicates if one should only
  return the computed set of packages and dependencies to
  install. If \code{NULL}, then it is internally set to
  \code{TRUE} only when there is a mismatch between the
  requested and the OS binary package types (e.g., if
  \code{type = 'win.both'} on a Unix/Mac host).}
}
\description{
  Install all dependencies from a package source directory
  or package source file.
}
\examples{
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

try( install.dependencies('Matrix', dep = "!", dry.run = TRUE) )
\dontrun{
install.dependencies("mypackage_1.0.tar.gz", dry.run=TRUE)
}
}

