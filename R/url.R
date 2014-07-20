# Project: repotools
# 
# Author: renaud
# Created: Jul 18, 2014
###############################################################################

#' Parsing netrc files
#' 
#' Parses an netrc file in different ways and return the credential specifications it contains.
#' 
#' Entries in the netrc file can be associated to R repositories, which are looked up 
#' by \code{install.pkgs} via \code{\link{repos.url}}, by adding a comment line 
#' to a standard entry the following format:
#' 
#' \preformatted{
#' # standard host credential entry
#' machine www.somehost.org
#' login tata
#' password toto
#' 
#' #repos @@myRepo/a/b/c
#' machine www.myrepo.org
#' login user
#' password 1234
#' }
#' 
#' The first entry is standard and provide credentials for a named host.
#' 
#' The second entry starts with \code{'#repos '}, which indicates it is an entry for an R repository 
#' with shortcut key (\code{'@@myRepo'}) and an -- optional -- path
#' to the actual repository on the host machine \code{'www.myrepo.org'}.
#' 
#' Note that it is not mandatory to start the repository entry key with a \code{'@@'}, but it makes it clearer
#' that the first part of the path is a key to be substituted rather than being part of the path.
#'  
#' @param x path or connection to an netrc file.
#' Default is to look for file \code{'.netrc'} into the user's home directory.
#' @param std indicates if the file should be parsed as a standard netrc file (\code{TRUE}), 
#' searched for R repository specifications (see \emph{Details}) (\code{FALSE}), 
#' or if entries associated with R repositories should be skipped (\code{NA}).
#' @param quiet logical that indicates if the function should return \code{NULL} if the file is not found, 
#' or throw an error.
#' 
#' @return a character matrix with one row per credential and columns \code{'machine'}, \code{'login'} 
#' and \code{'password'} if \code{std} is \code{TRUE} or \code{NA}, and additional columns 
#' \code{'repos'} and \code{'path'} if \code{std = FALSE}.
#' 
#' @export
#' @examples
#' 
#' \dontrun{
#' try( read_netrc() )
#' read_netrc(quiet = TRUE)
#' }
#' 
#' # using toy example
#' f <- system.file('netrc', package = 'repotools')
#' if( interactive() ) file.show(f) else cat(readLines(f), sep = "\n")
#' 
#' # standard parsing
#' read_netrc(f)
#' # parse repo information
#' read_netrc(f, std = FALSE)
#' # skip repo entries
#' read_netrc(f, std = NA)
#'  
#' 
read_netrc <- function(x = file.path(Sys.getenv('HOME'), '.netrc'), std = TRUE, quiet = FALSE){
    
    if( is.null(x) ) x <- file.path(Sys.getenv('HOME'), '.netrc')
    if( is.character(x) && !file.exists(x) ){
        if( !quiet ) stop("netrc file '", x, "' does not exist.")
        return()
    }
    
    l <- readLines(x, warn = FALSE)
    if( !length(l) ) return()
    
    chunk <- grep("^\\s*$", l)
    if( tail(chunk, 1L) != length(l) ){
        chunk <- c(chunk, length(l)+1)
    }
    chunk <- c(0L, chunk)
    chunk <- chunk[!chunk %in% (chunk+1)]
    if( identical(chunk, 0L) ) return()
    
    fields <- std_fields <- c('machine', 'login', 'password')
    comment_pattern <- ''
    if( !isTRUE(std) ){
        fields <- c(std_fields, 'repos')
        comment_pattern <- '#?\\s*'
    }
    pattern <- sprintf("^\\s*%s([^ ]+)\\s([^ ]+)", comment_pattern)
    
    res <- lapply(seq_along(chunk)[-1L], function(i){
            # parse each line in the definition
            def <- l[seq(chunk[i-1L]+1L, chunk[i]-1L)]
            m <- str_match(def, pattern)
            def <- setNames(setNames(m[, 3L], m[, 2L])[fields], fields)
            
            # skip entry if associated with a repos
            if( is_NA(std) && !is_NA(def['repos']) ) return() 
            
            # add trailing path if not standard parsing
            if( isFALSE(std) ){
                if( is_NA(def['repos']) ){
                    def['repos'] <- def['machine']
                    def['path'] <- ''
                }else{
                    p <- str_match(unname(def['repos']), '^@?([^ /]+)(/[^ ]+)?')
                    def['repos'] <- paste0('@', p[, 2L])
                    def['path'] <- p[, 3L]    
                }
                
            }else def <- def[std_fields]
            
            def
        })
    res <- Filter(length, res)
    do.call(rbind, res)
}

#' Repository URLs
#' 
#' Builds full repository URLs based on shortcut repository keys and host names, adding credentials if necessary.  
#' 
#' @param repos character vector of repository specifications, either a full URLs,
#' e.g., \code{'http://repo.host.org'} or \code{'file:///path/to/local/repo'}, or
#' using repository shortcut keys, e.g., \code{'@@myRepo/path/to/repo/root'}, which is
#' substituted based on the entries in \code{netrc} file.
#' @param ... other repository specification appended to \code{repos}.
#' @param .netrc path or connection to a \emph{netrc} file, which is looked up for
#' matching repository/host entries.
#' Default is to look for file \code{'.netrc'} in the user's home directory.
#' 
#' Repository and host credential entries are defined in an \emph{netrc} file format (see \code{\link{read_netrc}} for more details).
#' 
#' Repository shortcut keys start a \code{'@@'}, e.g., \code{'@@myRepo/path/to/repo'} and are substituted by the URL
#' \code{'http://<login>:<password>@@<machine><path>/path/to/repo'}, where \emph{<machine>}, \emph{<path>},
#' \emph{<login>} and \code{<password>} are taken from the matched \emph{netrc} specification.
#' 
#' Regular hosts are also matched against the \emph{netrc} specification to add credentials if necessary.
#'  
#' @export
#' @examples
#' 
#' # appending to default repos
#' repos.url('+')
#' repos.url('+http://another.repo.org')
#' 
#' # using toy example
#' f <- system.file('netrc', package = 'repotools')
#' if( interactive() ) file.show(f) else cat(readLines(f), sep = "\n")
#' 
#' # test different substitutions
#' spec <- c('@@myRepo', '@@myRepo/extra/path', 'http://www.public.org', 'http://www.privatehost.org/repo/dir', '@@CRAN@@')
#' cbind(Specification = spec, URL = repos.url(spec, .netrc = f))
#' 
repos.url <- function(repos = getOption('repos'), ..., .netrc = NULL){
    
    x <- c(repos, unlist(list(...), use.names = FALSE))
    
    # check if appending repos to default repos
    if( length(grep("^\\+", x)) ){
        x <- gsub("^\\+", '', x)
        x <- Filter(nzchar, x)
        x <- c(getOption('repos'), x)
    }
    
    if( length(i <- which(x == '@CRAN@')) ){
        if( !interactive() ) x[i] <- 'http://cran.rstudio.com' 
        else if( !is.na(cran_url <- getOption('repos')['CRAN']) ) x[i] <- cran_url
    }
    
    # load credentials and repo keys from netrc file
    if( !is.null(net <- read_netrc(x = .netrc, std = FALSE, quiet = TRUE)) ){
        # complete repo or add credentials
        url <- str_match(x, "^(((http)|(file))://)?((@)?([^/]+))(.*)")
        j <- rep(0L, nrow(url))
        rkeys <- nzchar(url[, 7L])
        j[rkeys] <- match(url[rkeys, 6L], net[, 'repos'], nomatch = 0L)
        j[!rkeys] <- match(url[!rkeys, 6L], net[, 'machine'], nomatch = 0L)
        url <- url[j>0, , drop = FALSE]
        net <- net[j, , drop = FALSE]
        x[j>0] <- sprintf("http://%s:%s@%s%s%s", net[, 'login'], net[, 'password'], net[, 'machine'], net[, 'path'], url[, 9L])       
    }
    
    if( length(i <- setdiff(grep("^@", x), which(x == '@CRAN@'))) ){
        warning("Some repositories could not be resolved (check .netrc file): ", str_out(x[i], Inf))
        x <- x[-i]
    }
    
    x
}

