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

 .biocinstallRepos <- function(siteRepos = NULL, lib = NULL){
     if( !require('BiocInstaller', character.only = TRUE, lib.loc = lib) ){
        source('http://www.bioconductor.org/biocLite.R')
     }
     library(BiocInstaller, lib.loc = lib)
     biocinstallRepos(siteRepos)
 }
 
# setup up RStudio mirror if necessary
if( !interactive() ){
    options(repos = gsub('@CRAN@', 'http://cran.rstudio.com', getOption('repos'), fixed = TRUE))
}

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

if( !require2('repotools', version = '1.5.4') ){
    
    
    # load devtools
    if( !require2('devtools') ){
        install.packages('devtools')
        suppressMessages(library(devtools))
    }
    
    # install required version of pkgmaker
    if( !require2('pkgmaker', version = '0.25.6') ){    
        install_github('pkgmaker', 'renozao', 'develop', quick = QUICK)
    }
    
    # install BiocInstaller
    if( !require2('BiocInstaller') ){
        source("http://www.bioconductor.org/biocLite.R")
    }
    
    # install repotools
    if( WINDOWS ){
        install.packages('repotools', repos = c(getOption('repos'), 'http://tx.technion.ac.il/~renaud/GRAN'))
        
    }else{
        # add BiocInstaller if needed
        # NB: do not setup repos tom make sure ReportingTools CANNOT be found (too big)
            if( !QUICK ) .biocinstallRepos()
            install_github('repotools', 'renozao', quick = QUICK)
        
    }
    library(repotools)
}
message("Loaded repotools version ", packageVersion('repotools'))

})
