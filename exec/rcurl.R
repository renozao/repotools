#!/usr/bin/env Rscript
# Project: repotools
# 
# Author: renaud
# Created: Jun 11, 2014
###############################################################################

# process command line arguments
args <- commandArgs(TRUE)
getArg <- function(x, default = NULL){
    res <- if( length(i <- which(args == x)) ) args[i+1L]
    else default
    if( !is.null(res) && res == 'NULL' ) res <- NULL
    res
}

debug_mode <- function() nzchar(Sys.getenv('R_REPOTOOLS_DEBUG'))

src <- args[1L]
dest <- args[2L]
quiet <- '--quiet' %in% args
lib.loc <- getArg('--lib')
httpheader <- getArg('--httpheader')
userpwd <- getArg('--userpwd')
        
# progress bar
rcurl_progress_func <- NULL
if( !quiet ){

    rcurl_progress_func <- function(total, now){
        if( isTRUE(now) ) total <- c(100, 100)
        if( is.null(now) ) total <- c(0, 100)
        TotalToDownload <- total[1L]; NowDownloaded <- total[2]
        if( !TotalToDownload ) return()
        totaldotz=20
        fractiondownloaded = NowDownloaded / TotalToDownload
        dotz = round(fractiondownloaded * totaldotz)
        cat("[")
        if( dotz > 0 ) replicate(dotz, cat("="))
        if( totaldotz - dotz > 0 ) replicate(totaldotz - dotz, cat(" "))
        cat(sprintf("] %3.0f%%",fractiondownloaded*100))
        flush.console()
        if( !isTRUE(now) ) replicate(totaldotz + 7, cat("\b"))
	}
}

# should eventually go in repotools
file.fsize <- function(x, size = file.size(x)){
    
    y <- t(sapply(size, '/', 10^c(0, 3, 6, 9)))
    i <- max.col((y >= 1) + (y > 0), 'last')
    v <- sapply(seq(nrow(y)), function(j) y[j, i[j]])
    res <- sprintf("%.2f %s", v, c('bytes', 'Kb', 'Mb', 'Gb')[i])
    res[is.na(v)] <- ''
    res
}

rcurl <- function(){
    suppressMessages(library(RCurl, lib.loc = lib.loc))
    
    opts <- list(progressfunction = rcurl_progress_func
                  , noprogress = quiet
                  , followlocation = TRUE
                  , netrc = 1
              )
    # add credentials if needed
    # look for credentials in netrc file if not already provided in url
    if( !is.null(userpwd) ){
      opts$netrc <- NULL
      opts$userpwd <- userpwd
    }
    
    # build curl option list
    opts <- opts[!sapply(opts, is.null)]
    curl_opts <- do.call(curlOptions, opts)
    
    if( debug_mode() ){
      cat("Checking url:", src, "\n")
      cat("Options:\n")
      cat(sprintf("  - %s: '%s'", names(curl_opts), curl_opts), sep = "\n")
    }
    if( !url.exists(src, .opts = curl_opts) ){
        if( !quiet ){ 
            rcurl_progress_func(NULL, NULL)
            cat(" [ERROR: URL not found]\n") 
		}
        return(invisible())
    }
    
    # fetch URL data
    get_call <- call('getBinaryURL', url = src, .opts = curl_opts)
    if( length(httpheader) ) get_call[['httpheader']] <- httpheader
    raw <- eval(get_call)
    
    if( !quiet ){ 
        rcurl_progress_func(NULL, TRUE) 
        cat(" [OK"); 
	}
    writeBin(raw, dest)
    if( !quiet ){
        cat(sprintf(" - %s]\n", file.fsize(dest)))
        cat("Saved in '", dest, "'\n", sep = '')
    }
    invisible() 
}
rcurl()

