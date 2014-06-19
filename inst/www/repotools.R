# Project: repotools
# 
# Author: Renaud Gaujoux
# Created: Jun 11, 2014
# Usage:
# source('http://tx.technion.ac.il/~renaud/GRAN/repotools.R')
# 
###############################################################################

require2 <- function(x, lib.loc = NULL, version = NULL, cmp = ">="){
    
    # look for package
    if( !length(find.package(x, quiet = TRUE, lib.loc = lib.loc)) ) return(FALSE)
    
    req <- function(){
        suppressMessages(suppressWarnings(require(x, character.only = TRUE, lib.loc = lib.loc, quietly = TRUE)))
    }
    
    if( is.null(version) ) req()
    else{
        compare <- match.fun(cmp)
        if( compare(packageVersion(x, lib.loc = lib.loc), version) ) req()
        else FALSE
    }
}

if( !require2('repotools', version = '1.3.11') ){
    
    WINDOWS <- .Platform$OS.type == 'windows' 
    # load devtools
    if( !require2('devtools') ){
        install.packages('devtools')
        suppressMessages(library(devtools))
    }
    
    # install required version of pkgmaker
    if( !require2('pkgmaker', version = '0.25') ){    
        install_github('pkgmaker', 'renozao', 'develop', quick = WINDOWS)
    }
    
    # install BiocInstaller
    if( !require2('BiocInstaller') ){
        source("http://www.bioconductor.org/biocLite.R")
    }
    
    # install repotools
    if( WINDOWS ){
        install.packages('repotools', repos = c(getOption('repos'), 'http://tx.technion.ac.il/~renaud/GRAN'))
    }else{
        install_github('repotools', 'renozao')
    }
    library(repotools)
}
message("Loaded repotools version ", packageVersion('repotools'))

