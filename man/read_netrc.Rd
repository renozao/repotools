\name{read_netrc}
\alias{read_netrc}
\title{Parsing netrc files}
\usage{
  read_netrc(x = .netrc_path(), std = TRUE,
    line.number = FALSE, quiet = FALSE)
}
\arguments{
  \item{x}{path or connection to an netrc file. Default is
  to look for file \code{'.netrc'} into the user's home
  directory.}

  \item{std}{indicates if the file should be parsed as a
  standard netrc file (\code{TRUE}), searched for R
  repository specifications (see \emph{Details})
  (\code{FALSE}), or if entries associated with R
  repositories should be skipped (\code{NA}).}

  \item{line.number}{logical that indicates if the starting
  and ending line number of each credential definition
  should also be returned.}

  \item{quiet}{logical that indicates if the function
  should return \code{NULL} if the file is not found, or
  throw an error.}
}
\value{
  a character matrix with one row per credential and
  columns \code{'machine'}, \code{'login'} and
  \code{'password'} if \code{std} is \code{TRUE} or
  \code{NA}, and additional columns \code{'repos'} and
  \code{'path'} if \code{std = FALSE}.
}
\description{
  Parses an netrc file in different ways and return the
  credential specifications it contains.
}
\details{
  Entries in the netrc file can be associated to R
  repositories, which are looked up by \code{install.pkgs}
  via \code{\link{repos_url}}, by adding a comment line to
  a standard entry the following format:

  \preformatted{ # standard host credential entry machine
  www.somehost.org login tata password toto

  #repos @myRepo/a/b/c machine www.myrepo.org login user
  password 1234 }

  The first entry is standard and provide credentials for a
  named host.

  The second entry starts with \code{'#repos '}, which
  indicates it is an entry for an R repository with
  shortcut key (\code{'@myRepo'}) and an -- optional --
  path to the actual repository on the host machine
  \code{'www.myrepo.org'}.

  Note that it is not mandatory to start the repository
  entry key with a \code{'@'}, but it makes it clearer that
  the first part of the path is a key to be substituted
  rather than being part of the path.
}
\examples{
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

\dontrun{
try( read_netrc() )
read_netrc(quiet = TRUE)
}

# using toy example
f <- system.file('netrc', package = 'repotools')
if( interactive() ) file.show(f) else cat(readLines(f), sep = "\\n")

# standard parsing
read_netrc(f)
read_netrc(f, line.number = TRUE)
# parse repo information
read_netrc(f, std = FALSE)
# skip repo entries
read_netrc(f, std = NA)
}

