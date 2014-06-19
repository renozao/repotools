# Project: GEOdb
# 
# Author: renaud
# Created: Apr 3, 2014
###############################################################################


src_dir <- getwd()
f <- list.files("rcurl", full.names = TRUE)
execs <- if( WINDOWS ) grep("\\.exe$", f, value = TRUE) else f[tools::file_ext(f) == '']
if ( length(src <- execs[file.exists(execs)]) ) {
    
    .install <- function(){
          owd <- setwd(R_PACKAGE_DIR)
          on.exit( setwd(owd) )
          
          dest <- paste0('bin', R_ARCH) 
          message("  * Installing executable(s) in ", dest, "/")
          # create destination directory
          dir.create(dest, recursive = TRUE)
          sapply(src, function(src){
                d <- file.path(dest, basename(src))
                message("    - ", src, " -> ", d, " ... ", appendLF = FALSE)
                err <- NULL
                if( !file.copy(file.path(src_dir, src), d, overwrite = TRUE) ){
                    err <- c(err, 'copy')
                }else if( !Sys.chmod(d, mode = "0755", use_umask = TRUE) ){ 
                    err <- c(err, 'chmod') 
                }
                message(ifelse(!length(err), 'OK', paste0('ERROR [', paste0(err, collapse = ", "), ']')))
          })
    }
    .install()
}
