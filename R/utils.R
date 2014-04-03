# Project: GEOdb
# 
# Author: renaud
# Created: Nov 18, 2013
###############################################################################

#' @import pkgmaker
NULL

# set package options
.opts <- setupPackageOptions(auth = NULL)
           
#' Package Options for repotools
#'
#' These functions behave like the base functions \code{\link{options}}
#' and \code{\link{getOption}}, and provide access to options that are specific
#' to the \pkg{repotools} package. 
#' 
#' @inheritParams base::options
#' @inheritParams base::getOption
#'           
#' @rdname options
#' @export 
repos.getOption <- .opts$getOption

#' @rdname options
#' @export 
repos.options <- .opts$options

str_trim <- function(x) gsub(' ', '', x, fixed = TRUE)

.silenceF <- function(f, verbose=FALSE){
    
    if( verbose ) f
    else{
        function(...){
            capture.output(suppressPackageStartupMessages(suppressMessages(res <- f(...)))); 
            invisible(res)
        }
    }
}

qlibrary <- .silenceF(library, verbose = FALSE)



is.annpkg <- function(x) is.character(x) && length(x)>0L && all(grepl("\\.db$", x))

askUser <- function (msg, allowed = c("y", "n"), idefault = "n", default = "n", case.sensitive = FALSE) 
{
    if ( !interactive() )  return(default)
    fallowed <- allowed
    # add extra info on answer options
    if( !is.null(nm <- names(allowed)) ){
        allowed[nm != ''] <- nm[nm != '']  
    }
    if( !isFALSE(idefault) )
        fallowed[fallowed == idefault] <- toupper(idefault)
    repeat {
        allowMsg <- paste("[", paste(fallowed, collapse = "/"), 
                "]: ", sep = "")
        outMsg <- paste(msg, allowMsg)
        cat("\n", outMsg, sep='')
        if (case.sensitive) 
            ans <- readLines(n = 1)
        else ans <- tolower(readLines(n = 1))
        if( !isFALSE(idefault) && !nchar(ans) ) 
            ans <- idefault
        if (ans %in% allowed) 
            break
        else cat(paste(ans, "is not a valid response, try again.\n"))
    }
    # return answer
    ans
}

# internal source function to play well with CNTLM proxies
sourceURL <- function(url){
    
    file <- url
    if( grepl("^http", url) ){
        dest <- tempfile(basename(url), fileext='.R')
        download_file(url, dest)
        file <- dest
        on.exit( file.remove(file) )
    }
    source(file)
}

uq_requirePackage <- function(package, lib=NULL, ..., load=TRUE, msg=NULL, quiet=TRUE, prependLF=FALSE
        , ptype=c('CRAN-like', 'BioC', 'BioCsoft', 'BioCann')
        , autoinstall = FALSE ){
    
    .reqpkg <- if( quiet ) require.quiet else{
                if( prependLF ) message()
                require
            }
    reqpkg <- function(...){
        .reqpkg(..., lib=lib, character.only=TRUE)
    }
    
    # vectorized version
    if( length(package) >1L ){
        return( all(sapply(package, uq_requirePackage, lib = lib, ...
                                , load = load, msg = msg, quiet = quiet
                                , prependLF = prependLF)) )
    }
    # try loading it
    if( load && reqpkg(package) ) return( TRUE )
    # try finding it without loading
    else if( length(find.package(package, lib.loc=lib, quiet=TRUE)) ) return( TRUE )
    
    # package was not found: ask to install
    msg <- paste0("Package '", package, "' is required",
            if( is.null(msg) ) '.' else msg)
    
    # stop if not interactive
    if( !interactive() && !autoinstall ) stop(msg)
    
    # non-interactive mode: force CRAN mirror if not already set
    if( !interactive() && length(iCRAN <- grep("@CRAN@", getOption('repos'))) ){
        repos <- getOption('repos')
        repos[iCRAN] <- 'http://cran.rstudio.com'
        op <- options(repos = repos)
        on.exit(options(op), add = TRUE)
    }
    
    # detect annotation packages
    if( missing(ptype) && is.annpkg(package) ) ptype <- 'BioCann'
    ptype <- match.arg(ptype)
    
    if( !autoinstall ){
        msg <- paste0(msg, "\nDo you want to install it from known repositories [", ptype, "]?\n"
                , " Package(s) will be installed in '", if(is.null(lib) ) .libPaths()[1L] else lib, "'")
        if( quiet && prependLF ) message()
        repeat{
            ans <- askUser(msg, allowed = c('y', 'n', r='(r)etry'), idefault='y', default = 'y')
            if( ans == 'n' ) return( FALSE )
            if( ans == 'y' ) break
            if( ans == 'r' && reqpkg(package) ) return(TRUE)
        }
    }
    
    ## install
    # check Bioconductor repositories
    hasRepo <- function(p){ any(grepl(p, getOption('repos'))) }
    install_type <- ptype
    if( ptype == 'CRAN-like' 
            || ( ptype == 'BioC' && hasRepo('/((bioc)|(data/annotation))/?$') )
            || ( ptype == 'BioCsoft' && hasRepo('/bioc/?$') )
            || ( ptype == 'BioCann' && hasRepo('/data/annotation/?$') )
            ){
        install_type <- 'CRAN'
    }
    
    if( install_type == 'CRAN' ){
        pkginstall <- install.packages
    }else{ # Bioconductor 
        # use enhanced installer
        pkginstall <- install.pkgs
    }
    message()
    pkginstall(package, lib=lib, ...)
    #
    
    # try  reloading
    if( load ) reqpkg(package)
    else length(find.package(package, lib.loc=lib, quiet=TRUE))
}
