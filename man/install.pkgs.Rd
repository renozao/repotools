\name{install.pkgs}
\alias{install.pkgs}
\title{Enhanced Package Installation}
\usage{
  install.pkgs(pkgs, lib = NULL,
    repos = getOption("repos"),
    type = getOption("pkgType"), dependencies = NA,
    available = NULL, ..., quick = FALSE, dry.run = NULL,
    devel = FALSE, verbose = TRUE)
}
\arguments{
  \item{pkgs}{character vector of the names of packages whose current versions should be downloaded
from the repositories or of path to source/binary package files (or a mixed of both).}

  \item{repos}{URL or specification of CRAN-like package repository (see section \emph{Repositories}).
Use \code{repos = '+http://myrepo.org'} to append repositories to the default ones.}

  \item{...}{extra parameters eventually passed to the corresponding base function.}

  \item{dry.run}{logical that indicates if one should only return the computed set of
packages and dependencies to install.
If \code{NULL}, then it is internally set to \code{TRUE} only when there is a mismatch between
the requested and the OS binary package types (e.g., if \code{type = 'win.both'} on a Unix/Mac host).}

  \item{devel}{indicates if development packages hosted on GRAN (GitHub) should be preferred to
versions available in regular repositories.
The following values are allowed:
\itemize{
\item \code{FALSE}: package versions on regular repositories have priority over all other versions.
\item \code{TRUE}: 'release' GRAN versions (i.e. from master branches)
are preferred to versions on regular repositories (if their version is number is larger) and
'devel' development versions (i.e. on branches that start with 'devel').
\item \code{2}: 'devel' GRAN versions are preferred over all other versions.
}

In any case, packages not found in regular repositories are looked up on GRAN release, then GRAN devel
if still not found.}

  \item{verbose}{verbosity level (logical or numeric)}

  \item{lib}{
    character vector giving the library directories where to
    install the packages.  Recycled as needed.  If missing, defaults to
    the first element of \code{\link{.libPaths}()}.
  }

  \item{type}{character, indicating the type of package to download and
    install.

    Possible values are (currently) \code{"source"},
    \code{"mac.binary"}, \code{"mac.binary.mavericks"} and
    \code{"win.binary"}: the binary types can be listed and downloaded
    but not installed on other platforms.

    The default is the appropriate binary type on Windows and on the
    CRAN binary OS X distributions, otherwise \code{"source"}.  For the
    platforms where binary packages are the default, an alternative is
    \code{"both"} which means \sQuote{try binary if available and
    current, otherwise try source}.  (This will only choose the binary
    package if its version number is no older than the source version.
    In interactive use it will ask before attempting to install source
    packages.)
  }

  \item{dependencies}{logical indicating to also install uninstalled
    packages which these packages depend on/link to/import/suggest
    (and so on recursively).  Not used if \code{repos = NULL}.
    Can also be a character vector, a subset of
    \code{c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")}.

    Only supported if \code{lib} is of length one (or missing),
    so it is unambiguous where to install the dependent packages.  If
    this is not the case it is ignored, with a warning.

    The default, \code{NA}, means
    \code{c("Depends", "Imports", "LinkingTo")}.

    \code{TRUE} means to use
    \code{c("Depends", "Imports", "LinkingTo", "Suggests")} for
    \code{pkgs} and
    \code{c("Depends", "Imports", "LinkingTo")} for added dependencies:
    this installs all the packages needed to run \code{pkgs}, their
    examples, tests and vignettes (if the package author specified them
    correctly).

    In all of these, \code{"LinkingTo"} is omitted for binary packages.
  }

  \item{available}{
    an object as returned by \code{\link{available.packages}}
    listing packages available at the repositories, or \code{NULL} when
    the function makes an internal call to \code{available.packages}.
  }

  \item{quick}{if \code{TRUE} skips docs, multiple-architectures,
demos, and vignettes, to make installation as fast as possible.}
}
\description{
  These functions are enhanced versions of the base functions \code{\link{install.packages}},
\code{\link{available.packages}} and \code{link{download.packages}} (see \emph{Details}).
}
\details{
  The main differences with the base functions are that:
\itemize{
\item Bioconductor (soft, data/annotation, etc..) and Omegahat dependencies
are automatically resolved, without the need to enable these repositories;
\item if necessary, they use an enhanced \emph{curl} binary that internally uses the \pkg{RCurl}
package, which is configured to support authentication for password protected repositories.
Credentials can either passed embbedded within the URL or transparently taken from the
user's \emph{.netrc} file;
\item it supports the mixed-type installation, of binary and source packages;
\item it can install packages and recursive dependencies hosted on GitHub,
if these have been hooked to the GRAN repository.
}

\code{install.pkgs} installs packages, which can be local, remote or in a CRAN-like repository,
possibly password protected.
Packages and their dependencies are automatically search in Bioconductor, Omegahat and GRAN
repositories if needed.
}
\section{Repositories}{


Respositories can be specified as a character vector that is processed in the following way:

\itemize{
\item \code{repos = NULL}, then the default set of repositories defined in option \code{'repos'} are used
(see \code{getOption('repos')});
\item \code{repos = c('+http://one.repo.org', 'http://two.repo.org')} appends one or
more repositories to the default set of repositories.
\item if an element of \code{repos} is \code{'@CRAN@'}, then the user is asked to choose a CRAN mirror,
except if in non-interactive mode, where RStudio mirror is used (\url{http://cran.rstudio.com});
\item Full URL, that can be remote (start with 'http://') or local (start with file://), and
may include authentication credentials in the form \code{'http://username:password@cran.domain.org'},
for password protected repositories (Basic, Digest, etc..);
\item Repo URL shortcut key [+ path], defined as a string prefixed with \code{'@'}, e.g., \code{'@myRepo/path/to/repo'},
that matches a repository entry in file \code{'.netrc'} in the user's home directory -- as returned by
\code{Sys.getenv('HOME')}.
It is internally substituted into a full repository base URL using by \code{repos_url} (see details in
\code{\link{read_netrc}} and \code{\link{repos_url}} for details on how repository entries are defined
and substituted respectively.
}
}

