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
#' by \code{install.pkgs} via \code{\link{repos_url}}, by adding a comment line 
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
#' @param line.number logical that indicates if the starting and ending line number of each 
#' credential definition should also be returned.
#' @param quiet logical that indicates if the function should return \code{NULL} if the file is not found, 
#' or throw an error.
#' 
#' @return a character matrix with one row per credential and columns \code{'machine'}, \code{'login'} 
#' and \code{'password'} if \code{std} is \code{TRUE} or \code{NA}, and additional columns 
#' \code{'repos'} and \code{'path'} if \code{std = FALSE}.
#' 
#' @family auth
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
#' read_netrc(f, line.number = TRUE)
#' # parse repo information
#' read_netrc(f, std = FALSE)
#' # skip repo entries
#' read_netrc(f, std = NA)
#'  
#' 
read_netrc <- function(x = netrc_path(), std = TRUE, line.number = FALSE, quiet = FALSE){
    
    x <- x %||% netrc_path()
    if( is.character(x) ){
        if( !file.exists(x) ){
            if( !quiet ) stop("netrc file '", x, "' does not exist.")
            return()
        }
        netrc_file <- normalizePath(x)
    }else netrc_file <- x
    
    l <- readLines(netrc_file, warn = FALSE)
    if( !length(l) ) return()
    
    chunk <- grep("^\\s*$", l)
    if( !length(chunk) || tail(chunk, 1L) != length(l) ){
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
    pattern <- sprintf("^\\s*%s([^ ]+)\\s+([^ ]+)", comment_pattern)
    
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
                    def['repos'] <- NA #def['machine']
                    def['path'] <- ''
                }else{
                    p <- str_match(unname(def['repos']), '^@?([^ /]+)(/[^ ]+)?')
#                    def['repos'] <- paste0('@', p[, 2L])
                    def['repos'] <- p[, 2L]
                    def['path'] <- p[, 3L]    
                }
            }else def <- def[std_fields]
            
            # add start/end index
            if( line.number ) def[c('start', 'end')] <- c(chunk[i-1L]+1L, chunk[i]-1L)
            
            def
        })
    res <- Filter(length, res)
    res <- do.call(rbind, res)
    rownames(res) <- res[, 'machine']
    # attach file name
    attr(res, '.netrc') <- netrc_file
    
    res
}

#' Function `netrc_path` returns the default location of the `.netrc` file, 
#' i.e. in the user home directory.
#' 
#' @rdname read_netrc
#' @export
netrc_path <- function(){
  prefix <- if( .Platform$OS.type == 'windows' ) '_' else '.'
  file.path(Sys.getenv('HOME'), paste0(prefix, 'netrc'))
}

url_credential_split <- function(x){
    p <- "^(((https?)|(file))://)?(([^:/]*):([^@:/]*))?((@)?([^/]*))(.*)"
    url <- str_match(x, p)
    res <- cbind(machine = url[, 11L], login = url[, 7L], password = url[, 8L]
                , repos = ifelse(!grepl(".", url[, 11L], fixed = TRUE), url[, 11L], NA)
                , path = url[, 12L])
    rownames(res) <- names(x)
    res 
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
#' @family auth
#' @export
#' @examples
#' 
#' # appending to default repos
#' repos_url('+')
#' repos_url('+http://another.repo.org')
#' 
#' # using toy example
#' f <- system.file('netrc', package = 'repotools')
#' if( interactive() ) file.show(f) else cat(readLines(f), sep = "\n")
#' 
#' # test different substitutions
#' spec <- c('@@myRepo', '@@myRepo/extra/path', 'http://www.public.org'
#' 				, 'http://www.privatehost.org/repo/dir', '@@CRAN@@')
#' cbind(Specification = spec, URL = repos_url(spec, .netrc = f))
#' 
repos_url <- function(repos = getOption('repos'), ..., .netrc = NULL){
    
    x <- c(repos, unlist(list(...), use.names = FALSE))
    
    # check if appending repos to default repos
    if( length(grep("^\\+", x)) ){
        x <- gsub("^\\+", '', x)
        x <- Filter(nzchar, x)
        x <- c(getOption('repos'), x)
    }
    
    # CRAN
    if( length(i <- which(toupper(x) == '@CRAN@')) ){
        if( !interactive() ) x[i] <- 'http://cran.rstudio.com' 
        else if( !is.na(cran_url <- getOption('repos')['CRAN']) ) x[i] <- cran_url
    }
    # Bioconductor
    if( length(i <- which(toupper(x) == '@BIOC')) ){
        x <- x[-i]
        x <- .biocinstallRepos(x)
    }
    # Omegahat
    if( length(i <- which(toupper(x) == '@OMEGAHAT')) ){
        x[i] <- 'http://www.omegahat.org/R'
    }
    
    # load credentials and repo keys from netrc file
    if( !is.null(net <- read_netrc(x = .netrc, std = FALSE, quiet = TRUE)) ){
        # complete repo or add credentials
        #url <- str_match(x, "^(((http)|(file))://)?((@)?([^/]+))(.*)")
        url <- url_credential_split(x)
        j <- rep(0L, nrow(url))
        rkeys <- !is.na(url[, 'repos'])
        j[rkeys] <- match(url[rkeys, 'repos'], net[, 'repos'], nomatch = 0L)
        j[!rkeys] <- match(url[!rkeys, 'machine'], net[, 'machine'], nomatch = 0L)
        url <- url[j>0, , drop = FALSE]
        net <- net[j, , drop = FALSE]
        x[j>0] <- sprintf("http://%s:%s@%s%s%s", net[, 'login'], net[, 'password'], net[, 'machine'], net[, 'path'], url[, 'path'])       
    }
    
    if( length(i <- setdiff(grep("^@", x), which(x == '@CRAN@'))) ){
        warning("Some repositories could not be resolved (check .netrc file): ", str_out(x[i], Inf))
        x <- x[-i]
    }
    
    x
}

override_matrix <- function(x, y){
    if( !length(x) ) y
    else{
        im <- match(rownames(y), rownames(x), nomatch = 0L)
        x[im[im>0], ] <- y[im>0, , drop = FALSE]
        rbind(x, y[im==0L, , drop = FALSE])
    }
} 

#' CRAN-style Repository Authentication Credentials
#' 
#' Gets/sets authentication credentials for CRAN-style.
#' 
#' @param ... repository keys or credential specifications
#' @param save indicates if the new credentials are to be saved on disk,
#' by updating in the \code{.netrc} file, or in a cache -- which expires at the end of 
#' the \pkg{R} session.
#' It can also be a character string, which then specifies the path where to save the 
#' result (cached entries are not written to disk).
#' 
#' Use \code{save=""} to print to the console.
#'  
#' @param cache specifies how the temporary cached credentials should be 
#' loaded: 
#' 
#' \itemize{
#' \item \code{NULL} loads both the cache and netrc data, with cache override matching 
#' netrc credentials;
#' \item \code{TRUE} loads only the cache data; 
#' \item \code{FALSE} loads only the netrc data.
#' }
#' 
#' The special value \code{cache = NA} will delete the cache, prior to any other operation.
#' 
#' @param exact logical that indicates if the the repository keys should be matched 
#' exactly. If \code{FALSE}, then keys are matched independently of possible trailing 
#' path specification.
#' @inheritParams repos_url
#' 
#' @family auth
#' @export
repos_auth <- local({
    
    .auth_cache <- NULL
    function(..., save = FALSE, cache = NULL, exact = FALSE, .netrc = NULL){
        
        x <- list(...)
        
        if( !is.matrix(.netrc) ){
            net <- read_netrc(x = .netrc, std = FALSE, line.number = TRUE, quiet = TRUE) 
        }else net <- .netrc
        
        # pre-process netrc data
        netrc_file <- attr(net, '.netrc')
        
        if( !is.null(net) ){
            net <- net[!is.na(net[, 'repos']), , drop = FALSE]
        }
        # extract credential data 
        auth <- net[, !colnames(net) %in% c('start', 'end'), drop = FALSE]
        rownames(auth) <- auth[, 'repos']
        
        # append/override cache
        if( is_NA(cache) ){
            old <- .auth_cache
            .auth_cache <<- NULL
            cache <- NULL
            
            # return old cache if nothing else to do
            if( !length(x) ) return(invisible(old))
        }
        
        if( isTRUE(cache) ) auth <- .auth_cache
        else if( !isFALSE(cache) && !is.null(.auth_cache) && nrow(.auth_cache) ){
            auth <- override_matrix(auth, .auth_cache)
        }
        
        # list of auths
        if( !length(x) ) return(auth)
        
        if( length(x) == 1L && is.null(names(x)) ) x <- x[[1L]]
        
        # build empty matrix if necessary
        if( !length(auth) ){
            fields <- c('machine', 'login', 'password', 'repos', 'path')
            auth <- matrix(NA, nrow=0, ncol=length(fields), dimnames = list(NULL, fields))
        }
        
        if( !is.matrix(x) ){
            # read access
            if( is.null(names(x)) ){
                
                if( exact ) ia <- match(x, auth[, 'machine'])
                else{
                    ix <- pmatch(auth[, 'machine'], x)
                    ia <- match(seq_along(x), ix)
                }
                # force the repos query
                irepo <- grep("^((@.+)|([^.]+))$", x)
                ia[irepo] <- match(gsub("^@", '', x[irepo]), auth[, 'repos'])
                
                res <- auth[ia, , drop = FALSE]
                if( is.matrix(res) ) rownames(res) <- x 
                
                return(res)
            }
            
            # split into a matrix
            if( is.list(x) || is.character(x) ){
                x[sapply(x, length) == 0L] <- NA
                x <- unlist(x)
                ok <- !is.na(x)
                x <- url_credential_split(x)
                # re-format
                x[ok, 'machine'] <- paste0(x[ok, 'machine'], x[ok, 'path'])     
                x[, 'path'] <- gsub("^[^/]+", '', rownames(x))
                x[, 'repos'] <- gsub("^([^/]+).*", '\\1', rownames(x))
                rownames(x) <- x[, 'repos'] 
            }
        }
        
        
        if( !is.matrix(x) ) 
            stop("Unexpected error: invalid processed input [", class(x), "]")
        
        new_auth <- x
        old_auth <- repos_auth(unname(rownames(new_auth)), .netrc = net)
        
        # force no credentials
        if( length(i_nocred <- which(!nzchar(new_auth[, 'machine']))) ){
            new_auth[i_nocred, c('machine', 'path')] <- old_auth[i_nocred, c('machine', 'path')]
        } 
        
        # store in cache
        if( isFALSE(save) ){
            # remove NA flagged rows
            if( is.null(.auth_cache) ) .auth_cache <<- new_auth
            else .auth_cache <<- override_matrix(.auth_cache, new_auth)
            .auth_cache <<- .auth_cache[!is.na(.auth_cache[, 'machine']), , drop = FALSE]
            
            return(invisible(old_auth))
        }
        
        flag_delete <- digest(tempfile())
        fields <- c('machine', 'login', 'password')
        l <- if( !is.null(netrc_file) ) readLines(netrc_file) 
        sapply(rownames(new_auth), function(r){
            
            # new entry
            new_data <- new_auth[r, ]
            to_delete <- is.na(new_data['machine'])
            new_entry <- sprintf("#repos @%s%s\n%s", new_data['repos'], new_data['path'], paste0(sprintf("%s %s", fields, new_data[fields]), collapse = "\n"))
            
            if( length(net) && (i <- match(r, net[, 'repos'], nomatch = 0L)) ){
                idx <- seq(as.integer(net[i,'start']), as.integer(net[i,'end']))
                if( to_delete ){ # delete
                    l[idx] <<- flag_delete
                    if( idx[1L] > 1L && !nzchar(l[idx[1L] - 1L]) ) l[idx[1L] - 1L] <<- flag_delete
                }else{ # modify
                    l[idx] <<- flag_delete
                    l[idx[1L]] <<- new_entry
                }
                
            }else if( !to_delete ){ # add
                if( length(l) && nzchar(tail(l, 1L)) ) new_entry <- c("", new_entry) 
                l <<- c(l, new_entry)
            }
        })
        
        # remove flagged lines
        l <- l[l != flag_delete]
        # add last empty line
        if( length(l) && nzchar(tail(l, 1L)) ) l <- c(l, "")
        
        # update file
        if( is.character(save) ) netrc_file <- save
        else if( is.null(netrc_file) || !file.exists(netrc_file) ){
            netrc_file <- netrc_file %||% netrc_path()
            if( askUser(paste0("Your .netrc file [", netrc_file, "] does not exist. Do you want to create it? "), idefault = 'y') != 'y' ){
                stop('Aborted saving repository credentials: user did not allowed creation of file ', netrc_file)
            }
        }
        action <- if( file.exists(netrc_file) ) 'Updating' else 'Saving'
        message(sprintf("%s %s repos credentials in file %s ... ", action, nrow(new_auth), netrc_file), appendLF = FALSE)
        cat(l, file = netrc_file, sep = "\n")
        message('OK')
        
    
        # return old value
        invisible(old_auth)
    }
})


# match a url with a machine pattern (from extended .netrc file format)
match_url <- function(url, machine, nomatch = NA_integer_, last = TRUE, ignore.protocol = FALSE){
  
  
  .local <- function(u, m){
    u <- tolower(u)
    m <- tolower(m)
    if( ignore.protocol ){
      u <- sub("^[^/]+://", '', u)
      m <- sub("^[^/]+://", '', m)
    }
    
    # check for regular expression
    i <- integer()
    if( any(regs <- grepl("[*)(+?$]", m)) ){
      i <- seq_along(m)[regs][sapply(m[regs], function(x) grepl(x, u) | grepl(x, paste0(u, '/')))]
    }
      
    i <- c(i, which(!is.na(pmatch(paste0(gsub("/*$", '', m[!regs]), '/'), paste0(gsub("/*$", '', u), '/')))))
    i <- sort(i)
    
    # return tail or head according to request
    if( !length(i) ) nomatch
    else if( last ) tail(i, 1L) else head(i, 1L)
  }
  
  sapply(url, .local, machine)
  
}


#' Gets URL Authentication Token
#' 
#' @param url character vector of URLs
#' 
#' @return a character vector of authentication tokens.
#' URLs for which no token was found get `NA` values. 
#' @param default default value to use for URLs for which no authentication token
#' can be found.
#' @param quiet logical that indicates to mute messages showing the matched machine
#' authentication
#' @param ... other parmeters passed to internal function `match_url`.
#' @param full logical that indicates if the function should return the full 
#' authentication details (machine, login, password) or only the password (i.e., token)
#' 
#' @export
url_auth <- function(url, default = NA_character_, quiet = TRUE, ..., full = FALSE){
  
  # process url
  stripped_url <- sub("^[^/]+://", '', url)
  
  # load data form .netrc file
  netrc <- read_netrc(quiet = TRUE)[, c('machine', 'login', 'password'), drop = FALSE]
  
  # prepend personal authentication token for Github URLs from environment variable GITHUB_PAT (if defined)
  if( !is.na(gpat <- Sys.getenv('GITHUB_PAT', unset = NA_character_)) ){
    netrc <- rbind(cbind(machine = '.*\\.github\\.com', login = 'GITHUB_PAT', password = gpat)
                  , netrc)
  }
  
  .local <- function(url, ...){
    
    res <- default
    if( full ){
      if( !is.null(default) ) res <- setNames(rep(res, 3L), c('machine', 'login', 'password'))
    }
    
    # override with last suitable token from .netrc file (if different from current value)
    if( !is.null(netrc) ){
      i <- match_url(url, netrc[, 'machine'], last = TRUE, ...)

      if( !is.na(i) ){
        netrc_token <- netrc[i, 'password']
        if( !netrc_token %in% res ){
          res <- netrc_token
          if( full ) res <- netrc[i, ]
          if( !quiet ) message(sprintf("Using .netrc authentication token [%s@%s]", netrc[i, 'login'], netrc[i, 'machine']))
        }
      }    
    }
    
    # return result
    res
  }
  
  res <- sapply(setNames(stripped_url, url), .local, ..., simplify = !is.null(default))
  if( full ) res <- t(res)
  res
      
}
