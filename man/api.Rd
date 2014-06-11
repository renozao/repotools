\name{install.pkgs}
\alias{available.pkgs}
\alias{download.pkgs}
\alias{exists.pkgs}
\alias{install.pkgs}
\alias{Library}
\title{Enhanced Package Installation}
\usage{
  install.pkgs(pkgs, lib = NULL, siteRepos = NULL,
    type = getOption("pkgType"), dependencies = NA,
    available = NULL, ..., dry.run = FALSE, devel = FALSE,
    verbose = TRUE)

  available.pkgs(...)

  download.pkgs(pkgs, destdir, available = NULL, ...)

  exists.pkgs(pkgs, repos = getOption("repos"), ...,
    value = FALSE, fixed = TRUE)

  Library(package, lib = NULL, ...)
}
\arguments{
  \item{siteRepos}{extra user-defined CRAN-like package
  repository}

  \item{...}{extra parameters eventually passed to the
  corresponding base function.}

  \item{dry.run}{logical that indicates if one should only
  return the computed set of packages and dependencies to
  install.}

  \item{devel}{indicates if development packages hosted on
  GRAN (GitHub) should be preferred to versions available
  in regular repositories. The following values are
  allowed: \itemize{ \item \code{FALSE}: package versions
  on regular repositories have priority over all other
  versions. \item \code{TRUE}: 'release' GRAN versions
  (i.e. from master branches) are preferred to versions on
  regular repositories (if their version is number is
  larger) and 'devel' development versions (i.e. on
  branches that start with 'devel'). \item \code{2}:
  'devel' GRAN versions are preferred over all other
  versions. }

  In any case, packages not found in regular repositories
  are looked up on GRAN release, then GRAN devel if still
  not found.}

  \item{verbose}{verbosity level}

  \item{fixed}{logical that indicates if the elements of
  \code{x} should match exactly (\code{TRUE}) or
  interpreted and matched as regular expressions.}

  \item{value}{logical, used only when \code{fixed=FALSE},
  that indicates if the name of the matched packages should
  be returned (instead of a \code{TRUE/FALSE} logical
  value.}

  \item{pkgs}{character vector of the names of packages
  whose current versions should be downloaded from the
  repositories.

  If \code{repos = NULL}, a character vector of file paths
  of #ifdef windows not active \file{.tar.gz} files.  These
  can be source archives or binary package archive files
  (as created by \command{R CMD build --binary}).  On a
  CRAN build of \R for OS X these can be \file{.tgz} files
  containing binary package archives.  Tilde-expansion will
  be done on the file paths.

  If this is missing or a zero-length character vector, a
  listbox of available packages is presented where possible
  in an interactive \R session.  }

  \item{lib}{ character vector giving the library
  directories where to install the packages.  Recycled as
  needed.  If missing, defaults to the first element of
  \code{\link{.libPaths}()}.  }

  \item{type}{character, indicating the type of package to
  download and install.

  Possible values are (currently) \code{"source"},
  \code{"mac.binary"} and \code{"win.binary"}: the binary
  types can be listed and downloaded but not installed on
  other platforms.

  The default is the appropriate binary type on Windows and
  on the CRAN binary OS X distribution, otherwise
  \code{"source"}.  For the platforms where binary packages
  are the default, an alternative is \code{"both"} which
  means \sQuote{try binary if available, otherwise try
  source}.  (This will only choose the binary package if
  its version number is no older than the source version.
  In interactive use it will ask before attempting to
  install source packages.)  }

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

  \item{available}{ an object as returned by
  \code{\link{available.packages}} listing packages
  available at the repositories, or \code{NULL} when the
  function makes an internal call to
  \code{available.packages}.  }

  \item{destdir}{ directory where downloaded packages are
  stored.  If it is \code{NULL} (the default) a
  subdirectory \code{downloaded_packages} of the session
  temporary directory will be used (and the files will be
  deleted at the end of the session).  }

  \item{repos}{ character vector, the base URL(s) of the
  repositories to use, e.g., the URL of a CRAN mirror such
  as \code{"http://cran.us.r-project.org"}.  For more
  details on supported URL schemes see \code{\link{url}}.

  Can be \code{NULL} to install from local files (with
  extension \file{.tar.gz} for source packages).  }

  \item{package}{the name of a package, given as a
  \link{name} or literal character string, or a character
  string, depending on whether \code{character.only} is
  \code{FALSE} (default) or \code{TRUE}).}
}
\description{
  These functions are enhanced versions of the base
  functions \code{\link{install.packages}},
  \code{\link{available.packages}} and
  \code{link{download.packages}} (see \emph{Details}).

  \code{available.pkgs} returns a matrix of the packages
  available in given repositories.

  \code{download.pkgs} downloads packages.

  \code{exists.pkgs} tells if packages are available from
  repositories.

  \code{Library} tries loading packages and install them if
  needed.
}
\details{
  The main differences with the base functions are that:
  \itemize{ \item if necessary, it uses a custom download
  method based on \pkg{RCurl} that can access password
  protected repositories. \item Bioconductor dependencies
  are automatically resolved, without the need to enable
  the BioC repositories (soft, data/annotation, etc..). }

  \code{install.pkgs} installs packages, which can be
  local, remote or in a CRAN-like repository, possibly
  password protected. Packages and their dependencies are
  automatically search in Bioconductor if needed.
}

