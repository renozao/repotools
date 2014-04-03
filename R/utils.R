# Project: GEOdb
# 
# Author: renaud
# Created: Nov 18, 2013
###############################################################################

#' @import pkgmaker
NULL

# set package options
.opts <- setupPackageOptions(auth = NULL)
           
#' Package Options for repotools
#'
#' These functions behave like the base functions \code{\link{options}}
#' and \code{\link{getOption}}, and provide access to options that are specific
#' to the \pkg{repotools} package. 
#' 
#' @inheritParams base::options
#' @inheritParams base::getOption
#'           
#' @rdname options
#' @export 
repos.getOption <- .opts$getOption

#' @rdname options
#' @export 
repos.options <- .opts$options
