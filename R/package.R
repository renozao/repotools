# Package hooks
# 
# Author: renaud
# Creation: 26 Jun 2012
###############################################################################

#' @include utils.R
#' @include patches.R
NULL


.onLoad <- function(libname=NULL, pkgname){
  
  set_shims()
  
  invisible()
}
