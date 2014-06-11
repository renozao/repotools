# Project: repotools
# 
# Author: Renaud Gaujoux
# Created: Jun 11, 2014
# Usage:
# source('http://tx.technion.ac.il/~renaud/GRAN/repotools.R')
# 
###############################################################################

require2 <- function(..., version = NULL, cmp = ">="){
    if( suppressWarnings(require(..., quietly = TRUE)) ){
        if( is.null(version) ) TRUE
        else{
            compare <- match.fun(cmp)
            compare(packageVersion(...), version)
        }
    }else FALSE
}

if( !require2('repotools') ){
    
    # load devtools
    if( !require2('devtools') ){
        install.packages('devtools')
        library(devtools)
    }
    
    # install required version of pkgmaker
    if( !require2('pkgmaker', version = '0.25') ){    
        install_github('pkgmaker', 'renozao', 'develop')
    }
    
    # install BiocInstaller
    if( !require2('BiocInstaller') ){
        source("http://www.bioconductor.org/biocLite.R")
    }
    
    # install repotools
    if( .Platform$OS.type == 'windows' ){
        install.packages('repotools', repos = c(getOption('repos'), 'http://tx.technion.ac.il/~renaud/GRAN'))
    }else{
        install_github('repotools', 'renozao')
    }
    library(repotools)
}
message("repotools version ", packageVersion('repotools'))

