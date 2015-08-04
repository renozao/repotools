# Project: repotools
# 
# Author: renaud
# Created: Apr 3, 2014
###############################################################################

#' @importFrom RCurl getBinaryURL
NULL

path.pkg <- function(x, ...){
    f <- attr(packageDescription(x), 'file')
    
    extra <- file.path(...)
    # different handling for source and installed packages
    if( basename(f) != 'DESCRIPTION' ){
        extra <- gsub("^inst/", "", extra)
        f <- dirname(f)
    }
    path <- dirname(f)
    if( length(extra) && nzchar(extra) ) path <- file.path(path, extra)
    else path <- dirname(path)
    path
} 

has_userpwd <- function(x){
    any(grepl("://[^@/]+@", x))
}

.setup_rcurl_exec <- function(verbose = FALSE){
    
    # script name
    exefile <- "curl"
    if( .Platform$OS.type == 'windows' ){
        exefile <- file.path(.Platform$r_arch, paste0(exefile, '.exe'))
    }
    
    pkg_file <- function(...){
        if( isDevNamespace() ){
            # this does not work anymore due to changes in devtools
            # => hardcode dev-compiled executable file instead
            #file.path(tempdir(), 'repotools', ...)
            file.path(system.file(package = 'repotools'), '../src/rcurl', exefile)
        }else system.file(..., package = 'repotools')
    }
    
    # use pre-built binary stored in binaries/ if necessary
    if( !file.exists(cmd_file <- pkg_file(file.path('bin', exefile))) ){
        if( !file.exists(cmd_file <- pkg_file(file.path('binaries', exefile))) )
            stop("repotools - Could not find rcurl executable")
    }
        
    if( verbose ) message("Using curl executable: ", cmd_file)
    # return location of executable
    cmd_file
}

.setup_rcurl <- local({
    .settings <- list()
    function(reset = FALSE){
        
        # only setup if necessary
        if( is.character(reset) ){
            if( !has_userpwd(reset) ) return(FALSE); 
            reset <- FALSE
        }
        
        if( isFALSE(reset) ){ # setup
            .settings$options <<- options(download.file.method = 'curl')
            # define custom curl executable to handle protected repo
            .settings$curl_exec <<- .setup_rcurl_exec(FALSE)
            rscript <- file.path(R.home('bin'), "Rscript")
            if( .Platform$OS.type == 'windows' ) rscript <- paste0(rscript, ".exe")
            # set environment variable read by custom rcurl binary
            Sys.setenv(`R_REPOTOOLS_RSCRIPT` = rscript)
            Sys.setenv(`R_REPOTOOLS_RCURL` = path.pkg('RCurl'))
            Sys.setenv(`R_REPOTOOLS_RCURL.r` = path.pkg('repotools', 'exec/rcurl.R'))
            # prepend binary path to system PATH
            .settings$PATH <<- Sys.getenv('PATH')
            Sys.setenv(PATH = paste(dirname(.settings$curl_exec), .settings$PATH, sep = .Platform$path.sep))
            # return backup list of previous settings
            .settings
            TRUE    
        }else{ # cleanup
            old <- if( is.list(reset) ) reset else .settings
            options(old$options)
            if( !is.null(old$PATH) ) Sys.setenv(PATH = old$PATH)
            # clean up repotools environment variables
            Sys.unsetenv('R_REPOTOOLS_RSCRIPT')
            Sys.unsetenv('R_REPOTOOLS_RCURL')
            Sys.unsetenv('R_REPOTOOLS_RCURL.r')
            # reset settings backup list
            .settings <<- list()
        }
    }
})

#' Downloading Files From Password Protected Directories
#' 
#' Downloads files using a custom \code{curl} binary that uses  
#' \pkg{RCurl} and is able to download files from password protected 
#' directories.
#' 
#' @inheritParams utils::download.file
#' @param ... arguments passed to \code{\link{download.file}}.
#' 
download_file <- function(url, destfile, ...){
    
    x <- url
    dest <- destfile
    # setup
    if( .setup_rcurl(x) ) on.exit( .setup_rcurl(TRUE) )
    
    dest <- gsub("^file://", "", dest)
    tmpdest <- tempfile(basename(x))
    on.exit( if( !is.null(tmpdest) ) unlink(tmpdest), add = TRUE)
    download.file(x, tmpdest, ..., cacheOK = FALSE)
    if( !file.exists(tmpdest) ) 
        stop("Failed to download file '", x, "'")
    res <- file.copy(tmpdest, dest, overwrite = TRUE)
    if( !res ){
        tmp <- tmpdest
        tmpdest <- NULL
        stop("Failed to copy downloaded file to target '", dest, "' [download: ", tmp, "]")
    }
    
    invisible(res)
}

#' \code{with_rcurl} executes an expression with the custom \code{curl} binary 
#' setup as the default download method.
#' 
#' @param expr an expression
#' @rdname download_file
#' @export
with_rcurl <- function(expr){
    
    if( .setup_rcurl() ) on.exit( .setup_rcurl(TRUE) )
    e <- parent.frame()
    eval(expr, env = e)
}


readURL <- function(x){
    tmp <- tempfile()
    on.exit( unlink(tmp) )
    if( download_file(x, tmp) ){
        paste0(readLines(tmp), collapse = "\n")
    }
}

url.copy <- function(x, dest){
    
    dest <- gsub("^file://", "", dest)
    if( length(x) > 1L && !is.dir(dest) )
        stop("Invalid destination path for multiple files: must be an existing directory")
    
    sapply(x, function(x){
        if( is.dir(dest) ) dest <- file.path(dest, basename(x))
        if( grepl("^http:", x) ) download_file(x, dest)
        else file.copy(x, dest, overwrite = TRUE)
    })
    
}

#' Sourcing Remote Files 
#' 
#' This function plays well with CNTLM proxies, because it download the complete file,
#' before sourcing it locally.
#' 
#' @param url URL
#' 
#' @export
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
