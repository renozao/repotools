# Project: GEOdb
# 
# Author: renaud
# Created: Apr 3, 2014
###############################################################################


execs <- file.path("rcurl/rcurl")
if(WINDOWS) execs <- paste0(execs, ".exe")
if ( any(w <- file.exists(execs)) ) {
      dest <- file.path(R_PACKAGE_DIR,  paste0('bin', R_ARCH))
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      file.copy(execs[w], dest, overwrite = TRUE)
}
