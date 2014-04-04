# Project: GEOdb
# 
# Author: renaud
# Created: Apr 3, 2014
###############################################################################


f <- list.files("rcurl", full.names = TRUE)
execs <- if( WINDOWS ) grep("\\.exe$", f, value = TRUE) else f[tools::file_ext(f) == '']
if ( any(w <- file.exists(execs)) ) {
      bindir <- paste0('bin', R_ARCH)
      dest <- file.path(R_PACKAGE_DIR,  bindir)
      message("  * Creating package directory: ", bindir)
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      message("  * Installing: ", paste0(basename(execs[w]), collapse = ", "))
      file.copy(execs[w], dest, overwrite = TRUE)
}
