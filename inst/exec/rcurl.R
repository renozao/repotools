#!/usr/bin/env Rscript
# Project: repotools
# 
# Author: renaud
# Created: Jun 11, 2014
###############################################################################

# process command line arguments
args <- commandArgs(TRUE)
getArg <- function(x, default = NULL){
    if( length(i <- which(args == x)) ) args[i+1L]
    else default
}

src <- args[1L]
dest <- args[2L]
quiet <- '--quiet' %in% args
lib.loc <- getArg('--lib')
httpheader <- getArg('--httpheader', '')
userpwd <- getArg('--userpwd', '')
        
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
        replicate(dotz, cat("="))
        replicate(totaldotz - dotz, cat(" "))
        cat(sprintf("] %3.0f%%",fractiondownloaded*100))
        flush.console()
        if( !isTRUE(now) ) replicate(totaldotz + 7, cat("\b"))
	}
}

rcurl <- function(){
    suppressMessages(library(RCurl, lib.loc = lib.loc))
    curl_opts <- list(progressfunction = rcurl_progress_func
                    , userpwd = userpwd
                    , noprogress = quiet )
    if( !url.exists(src, .opts = curl_opts) ){
        if( !quiet ){ 
            rcurl_progress_func(NULL, NULL)
            cat(" [ERROR: URL not found]\n") 
		}
        return(invisible())
    }
    raw <- getBinaryURL(src, .opts = curl_opts, httpheader = httpheader)
    if( !quiet ){ 
        rcurl_progress_func(NULL, TRUE) 
        cat(" [OK]\n"); 
	}
    writeBin(raw, dest)
    invisible() 
}
rcurl()

