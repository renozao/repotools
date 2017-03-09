# Project: repotools
# 
# Author: Renaud Gaujoux
# Created: Jun 11, 2014
# Usage:
# source('http://renozao.github.io/repotools/install.R')
# 
###############################################################################

local({
  
WINDOWS <- .Platform$OS.type == 'windows' 
QUICK <- if( exists('QUICK') ) get('QUICK') 
          else if( '--quick' %in% commandArgs(TRUE) ) TRUE
          else WINDOWS

# setup up RStudio mirror if necessary
if( !interactive() ){
options(repos = gsub('@CRAN@', 'http://cran.rstudio.com', getOption('repos'), fixed = TRUE))
}

#' Requires Package of Given Version
#' Equivalent of `require` but enables imposing constraint on the version of 
#' the required package.
#' 
#' @param version required minimum/maximum/exact version depending on the value of `cmp`
#' @param cmp comparison operator that indicates if the version of the required package 
#' should be greater ('>='), lower ('<=') or equal ('==') to `version`. 
#' @inheritParams require
require_version <- function(package, version = NULL, lib.loc = NULL, cmp = ">="){

  # look for package
  if( !length(find.package(package, quiet = TRUE, lib.loc = lib.loc)) ) return(FALSE)
  
  req <- function(){
  suppressMessages(suppressWarnings(require(package, character.only = TRUE, lib.loc = lib.loc, quietly = TRUE)))
  }

  if( is.null(version) ) req()
  else{
    compare <- match.fun(cmp)
    if( compare(packageVersion(package, lib.loc = lib.loc), version) ) req()
    else FALSE
  }
}

# load/install devtools
if( !require_version('devtools', version = '1.12') ){
  install.packages('devtools')
  suppressMessages(library(devtools))

}

# load/install repotools
if( !require_version('repotools', version = '1.8.8') ){
  # install repotools
  install_github('renozao/repotools', quick = QUICK)
  library(repotools)

}

message("Loaded repotools version ", packageVersion('repotools'))

})
