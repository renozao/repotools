# Project: GEOdb
# 
# Author: renaud
# Created: Nov 18, 2013
###############################################################################

#' @import pkgmaker
NULL

smessage <- function(..., indent = 0L, item = NULL, appendLF = FALSE){
    if( is.null(item) ){ # choose item from indent
        .item <- c('*', '*', '-', '-', '>', '>') 
        item <- .item[indent+1]
    }
    indent <- if( indent ) paste0(rep(' ', indent), collapse='') else ''
    if( nzchar(item) ) item <- paste0(item, ' ')
    message(indent, item, ..., appendLF = appendLF)
}

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

# From example in ?toupper
capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s,1,1)),
                {s <- substring(s,2); if(strict) tolower(s) else s},
                sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}
