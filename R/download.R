# Project: repotools
# 
# Author: renaud
# Created: Apr 3, 2014
###############################################################################


.setup_rcurl_exec <- function(){
    
    # script directory
    tmpdir <- tempfile("curl_")
    cmd_file <- file.path(tmpdir, 'curl')
    if( .Platform$OS.type == 'windows' ) cmd_file <- paste0(cmd_file, ".exe")
    dir.create(tmpdir)
    if( isDevNamespace() ) file.copy(file.path(tempdir(), 'repotools/bin/rcurl'), cmd_file)
    else file.copy(system.file('bin/rcurl', package = 'repotools'), cmd_file)
    Sys.chmod(cmd_file, mode = "0777", use_umask = TRUE)
    
    # return temporary directory
    tmpdir
}

.setup_rcurl <- local({
    .old <- list()
    function(reset = FALSE){
        if( !reset ){ # setup
            .old$options <<- options(download.file.method = 'curl')
            # define custom curl executable to handle protected repo
            .old$tmpdir <<- .setup_rcurl_exec()
            rscript <- file.path(R.home('bin'), "Rscript")
            if( .Platform$OS.type == 'windows' ) rscript <- paste0(rscript, ".exe")
            Sys.setenv(`_CURL_PASSTHROUGH_RSCRIPT` = rscript )
            .old$PATH <<- Sys.getenv('PATH')
            Sys.setenv(PATH = paste(.old$tmpdir, .old$PATH, sep = .Platform$path.sep))
                
        }else{ # cleanup
            options(.old$options)
            if( !is.null(.old$PATH) ) Sys.setenv(PATH = .old$PATH)
            if( !is.null(.old$tmpdir) ) unlink(.old$tmpdir, recursive = TRUE)
            .old <<- list()
        }
    }
})

download_file <- function(x, dest, ...){
    
    # setup
    .setup_rcurl()
    on.exit( .setup_rcurl(TRUE) )
    
    dest <- gsub("^file://", "", dest)
    download.file(x, dest, ..., cacheOK = FALSE)
    if( !file.exists(dest) ) stop("Failed to download file '", x, "'")
    TRUE
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

