# Project: GEOdb
# 
# Author: renaud
# Created: Nov 18, 2013
###############################################################################

#' @import pkgmaker utils plyr
#' @importFrom methods is
#' @importFrom stats setNames relevel
#' @importFrom stringr str_match str_trim
NULL

# or-NULL operator (borrowed from Hadley Wickham)
'%||%' <- function(x, y) if( !is.null(x) ) x else y

smessage <- function(..., indent = 0L, item = NULL, appendLF = FALSE){
    if( is.null(item) ){ # choose item from indent
        .item <- c('*', '*', '-', '-', '>', '>') 
        item <- .item[indent+1]
    }
    indent <- if( indent ) paste0(rep(' ', indent), collapse='') else ''
    if( nzchar(item) ) item <- paste0(item, ' ')
    message(indent, item, ..., appendLF = appendLF)
}

## # set package options
## .opts <- setupPackageOptions(auth = NULL)
##            
## #' Package Options for repotools
## #'
## #' These functions behave like the base functions \code{\link{options}}
## #' and \code{\link{getOption}}, and provide access to options that are specific
## #' to the \pkg{repotools} package. 
## #' 
## #' @inheritParams base::options
## #' @inheritParams base::getOption
## #'           
## #' @rdname options
## #' @export 
## repos.getOption <- .opts$getOption
## 
## #' @rdname options
## #' @export 
## repos.options <- .opts$options

# From example in ?toupper
capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s,1,1)),
                {s <- substring(s,2); if(strict) tolower(s) else s},
                sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

bad_version <- function(x){
    sapply(x, function(v) is(try(package_version(v), silent = TRUE), 'try-error'))
}

messagef <- function(..., appendLF = TRUE){
    message(sprintf(...), appendLF = appendLF)
}

.yesno <- function(x) c('no', 'yes')[x + 1L]


sha1 <- function(x){
  substring(digest(x), 1, 7)
}

#' Fetch the Last N R Versions
#' 
#' This function uses \code{\link[rversions]{r_versions}} to fetch all the 
#' R versions from R main SVN server.
#' 
#' @param n Number of versions to return
#' @param digits Number of version digits: 1 = major version number, 
#' 2 = <major>.<minor1>, 3 = <major>.<minor1>.<minor2>
#' @param cache logical that indicates if the result should be retrieved from 
#' cache or (re-)fetched from the SVN server.
#' 
#' @importFrom rversions r_versions
#' @export
r_versions_n <- local({
  .versions <- NULL
  function(n = Inf, digits = 2L, cache = TRUE){
    # fetch versions if necessary
    .versions <<- {if( cache ) .versions} %||% r_versions(dots = TRUE)
    
    # process and limit
    vd <- sapply(strsplit(.versions[[1L]], ".", fixed = TRUE)
        , function(x) paste0(head(x, digits), collapse = "."))
    tail(unique(vd), n)
  }
})

# Adds a field to a DESCRIPTION/PACKAGE data
add_dcf_field <- function(x, name, value, force = FALSE){
  
  # early exit if no input data
  if( !nrow(x) %||% FALSE ) return(x)
  
  if( !name %in% colnames(x) ){
    x <- cbind(x, NA)
    colnames(x)[ncol(x)] <- name
  }
  
  if( missing(value) ) return(x)
  
  value <- rep(value, length.out = nrow(x))
  
  # set values
  if( force ){
    x[, name] <- value
    
  }else{
    i <- is.na(x[, name])
    x[i, name] <- value[i]
  }
  
  x
  
}

# Sets environment variables and returns old values
Sys_setenv <- function(...){
  vars <- list(...)
  if( length(vars) == 1L && is.list(vars[[1L]]) && is.null(names(vars)) ) vars <- vars[[1L]]
  
  old <- Sys.getenv(names(vars), unset = NA, names = TRUE)
  # unset NA values, set non-NA values
  sapply(names(vars)[is.na(vars)], Sys.unsetenv)
  if( any(!is.na(vars)) ) do.call(Sys.setenv, vars[!is.na(vars)])
  
  invisible(as.list(old))
}

