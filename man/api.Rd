\name{install.pkgs}
\alias{available.pkgs}
\alias{download.pkgs}
\alias{exists.pkgs}
\alias{install.pkgs}
\alias{Library}
\title{Enhanced Package Installation}
\usage{
  install.pkgs(pkgs, lib = NULL,
    repos = getOption("repos"),
    type = getOption("pkgType"), dependencies = NA,
    available = NULL, ..., quick = FALSE, dry.run = NULL,
    devel = FALSE, verbose = TRUE)

  available.pkgs(...)

  download.pkgs(pkgs, destdir, available = NULL, ...)

  exists.pkgs(pkgs, repos = getOption("repos"), ...,
    value = FALSE, fixed = TRUE)

  Library(package, lib = NULL, ...)
}
\arguments{
  \item{repos}{URL or specification of CRAN-like package
  repository (see section \emph{Repositories}). Use
  \code{repos = .('http://myrepo.org')} to append
  repositories to the default ones.}

  \item{...}{extra parameters eventually passed to the
  corresponding base function.}

  \item{dry.run}{logical that indicates if one should only
  return the computed set of packages and dependencies to
  install. If \code{NULL}, then it is internally set to
  \code{TRUE} only when there is a mismatch between the
  requested and the OS binary package types (e.g., if
  \code{type = 'win.both'} on a Unix/Mac host).}

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

  \item{verbose}{verbosity level (logical or numeric)}

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

  If \code{repos = NULL}, a character vector of #ifdef
  windows not active file paths.  These can be source
  directories or archives or binary package archive files
  (as created by \command{R CMD build --binary}).
  (\code{http://} and \code{file://} URLs are also accepted
  and the files will be downloaded and installed from local
  copies.)  On a CRAN build of \R for OS X these can be
  \file{.tgz} files containing binary package archives.
  Tilde-expansion will be done on file paths.

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
  \code{"mac.binary"}, \code{"mac.binary.mavericks"} and
  \code{"win.binary"}: the binary types can be listed and
  downloaded but not installed on other platforms.

  The default is the appropriate binary type on Windows and
  on the CRAN binary OS X distributions, otherwise
  \code{"source"}.  For the platforms where binary packages
  are the default, an alternative is \code{"both"} which
  means \sQuote{try binary if available and current,
  otherwise try source}.  (This will only choose the binary
  package if its version number is no older than the source
  version.  In interactive use it will ask before
  attempting to install source packages.)  }

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

  \item{quick}{if \code{TRUE} skips docs,
  multiple-architectures, demos, and vignettes, to make
  installation as fast as possible.}

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
  \itemize{ \item Bioconductor (soft, data/annotation,
  etc..) and Omegahat dependencies are automatically
  resolved, without the need to enable these repositories;
  \item if necessary, it uses a custom download method
  based on \pkg{RCurl} that can access password protected
  repositories; \item it supports the mixed-type
  installation, of binary and source packages; \item it can
  install packages hosted on GitHub, if those have been
  hooked to the GRAN repository. }

  \code{install.pkgs} installs packages, which can be
  local, remote or in a CRAN-like repository, possibly
  password protected. Packages and their dependencies are
  automatically search in Bioconductor, Omegahat and GRAN
  repositories if needed.
}
\section{Repositories}{
  Respositories can be specified as a character vector that
  is processed in the following way:

  \itemize{ \item \code{repos = NULL}, then the default set
  of repositories defined in option \code{'repos'} are used
  (see \code{getOption('repos')}); \item \code{repos =
  .('http://one.repo.org', 'http://two.repo.org')} appends
  one or more repositories to the default set of
  repositories. \item if an element of \code{repos} is
  \code{'@CRAN@'}, then the user is asked to choose a CRAN
  mirror, except if in non-interactive mode, where RStudio
  mirror is used (\url{http://cran.rstudio.com}); \item
  Full URL, that can be remote (start with 'http://') or
  local (start with file://), and may include
  authentication credentials in the form
  \code{'http://username:password@cran.domain.org'}, for
  password protected repositories (Basic, Digest, etc..);
  \item Repo URL shortcut, defined as a string prefixed
  with \code{'@'}, e.g., \code{'@myHOST/path/to/repo'},
  which match credentials stored in an \emph{.netrc} file
  in the user's home directory -- as returned by
  \code{Sys.getenv('HOME')}. Credentials in the .netrc file
  must be defined by sections like this:

  \preformatted{ repos myHOST url
  www.myhost.org/~somebody/a/b/c login user password 1234 }

  In this case, the repository specification
  \code{'@myHOST/path/to/repo'} will be substituted by the
  URL
  \code{'http://user:1234@www.myhost.org/~somebody/a/b/c/path/to/repo'}
  and the actual contrib URL eventually used by
  \code{link{available.packages}} will be
  \code{'http://user:1234@www.myhost.org/~somebody/a/b/c/path/to/repo/src/contrib/'}
  (or another variation on Windows and Mac).

  }
}

