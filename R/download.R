# Project: repotools
# 
# Author: renaud
# Created: Apr 3, 2014
###############################################################################

#' @importFrom RCurl getBinaryURL
NULL

path.pkg <- function(x){
    f <- attr(packageDescription(x), 'file')
    
    # different handling for source and installed packages
    f <- if( basename(f) != 'DESCRIPTION' ) dirname(f)
    dirname(dirname(f))
} 

has_userpwd <- function(x){
    any(grepl("://[^@/]+@", x))
}

.setup_rcurl_exec <- function(verbose = FALSE){
    
    # script directory
    tmpdir <- tempfile("curl_")
    dir.create(tmpdir)
    file0 <- "curl"
    if( .Platform$OS.type == 'windows' ){
        file0 <- file.path(.Platform$r_arch, paste0(file0, ".exe"))
    }
    file0 <- file.path('bin', file0)
    ok <- if( isDevNamespace() ) file.copy(file.path(tempdir(), 'repotools', file0), tmpdir)
    else file.copy(system.file(file0, package = 'repotools'), tmpdir)
    if( !ok )
        warning("repotools - Could not copy rcurl executable '", file0, "' into '", tmpdir, "'")
    cmd_file <- file.path(tmpdir, basename(file0))
    Sys.chmod(cmd_file, mode = "0777", use_umask = TRUE)
    if( verbose ) message("Temporary curl [", tmpdir, "]: ", str_out(list.files(tmpdir), Inf))
    # return temporary directory
    tmpdir
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
            .settings$tmpdir <<- .setup_rcurl_exec(FALSE)
            rscript <- file.path(R.home('bin'), "Rscript")
            if( .Platform$OS.type == 'windows' ) rscript <- paste0(rscript, ".exe")
            # set environment variable read by custom rcurl binary
            Sys.setenv(`R_REPOTOOLS_RSCRIPT` = rscript)
            Sys.setenv(`R_REPOTOOLS_RCURL` = path.pkg('RCurl'))
            # prepend binary path to system PATH
            .settings$PATH <<- Sys.getenv('PATH')
            Sys.setenv(PATH = paste(.settings$tmpdir, .settings$PATH, sep = .Platform$path.sep))
            # return backup list of previous settings
            .settings
            TRUE    
        }else{ # cleanup
            old <- if( is.list(reset) ) reset else .settings
            options(old$options)
            if( !is.null(old$PATH) ) Sys.setenv(PATH = old$PATH)
            if( !is.null(old$tmpdir) ) unlink(old$tmpdir, recursive = TRUE)
            # clean up repotools environment variables
            Sys.unsetenv('R_REPOTOOLS_RSCRIPT')
            Sys.unsetenv('R_REPOTOOLS_RCURL')
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
