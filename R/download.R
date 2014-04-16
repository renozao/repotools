# Project: repotools
# 
# Author: renaud
# Created: Apr 3, 2014
###############################################################################


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
    .old <- list()
    function(reset = FALSE){
        if( isFALSE(reset) ){ # setup
            .old$options <<- options(download.file.method = 'curl')
            # define custom curl executable to handle protected repo
            .old$tmpdir <<- .setup_rcurl_exec(FALSE)
            rscript <- file.path(R.home('bin'), "Rscript")
            if( .Platform$OS.type == 'windows' ) rscript <- paste0(rscript, ".exe")
            Sys.setenv(`_CURL_PASSTHROUGH_RSCRIPT` = rscript )
            .old$PATH <<- Sys.getenv('PATH')
            Sys.setenv(PATH = paste(.old$tmpdir, .old$PATH, sep = .Platform$path.sep))
            .old    
        }else{ # cleanup
            old <- if( is.list(reset) ) reset else .old
            options(old$options)
            if( !is.null(old$PATH) ) Sys.setenv(PATH = old$PATH)
            if( !is.null(old$tmpdir) ) unlink(old$tmpdir, recursive = TRUE)
            .old <<- list()
        }
    }
})

download_file <- function(x, dest, ...){
    
    # setup
    .setup_rcurl()
    on.exit( .setup_rcurl(TRUE) )
    
    dest <- gsub("^file://", "", dest)
    tmpdest <- tempfile(basename(x))
    on.exit( unlink(tmpdest) )
    download.file(x, tmpdest, ..., cacheOK = FALSE)
    if( !file.exists(tmpdest) ) 
        stop("Failed to download file '", x, "'")
    res <- file.copy(tmpdest, dest, overwrite = TRUE)
    if( !res ){
        on.exit()
        stop("Failed copy downloaded file to target '", dest, "' [download: ", tmpdest, "]")
    }
    
    invisible(res)
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
