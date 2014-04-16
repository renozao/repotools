# Project: GEOdb
# 
# Author: Renaud Gaujoux
# Created: Feb 25, 2014
###############################################################################

#' @include utils.R 
#' @include download.R
NULL

repo_auth <- function(...){
    
    auth <- repos.getOption('auth')
    # list of auths
    if( !nargs() ) return(auth)
    
    x <- list(...)
    if( length(x) == 1L && is.null(names(x)) ) x <- x[[1L]]
    
    # reset to default value
    if( is.null(x) ){
        repos.options(auth = NULL)
        return(auth)
    }
    
    # read access
    if( is.null(names(x)) ) return(auth[x])
    
    # write access
    old <- repos.options(auth = x)
    # return old value
    invisible(old[[1L]])
}

.biocinstallRepos <- function(siteRepos = NULL){
    if( !require(BiocInstaller) ) sourceURL('http://www.bioconductor.org/biocLite.R')
    biocinstallRepos(siteRepos)
}

package_name <- function(x){
    basename(gsub("_[0-9.]+\\.((tar\\.gz)|(zip)|(tgz))?$", "", x))
}

package_type <- function(x){
    t <- sapply(paste0(c("\\.tar\\.gz", "\\.zip", "\\.tgz"), "$"), grepl, x)
    if( !is.matrix(t) ) t <- t(t)
    it <- apply(t, 1L, which)
    .contrib_types[it]
}

.contrib_types <- c('source', 'win.binary', 'mac.binary')

#' @importFrom tools write_PACKAGES
create_repo <- function(dir = '.', pkgs = NULL, ..., clean = FALSE, verbose = FALSE){
    
    
    # clean root directory if requested
    if( clean ) unlink(dir, recursive = TRUE)
    
    # create root directory if needed
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    repo_dir <- normalizePath(dir)
    
    # create directory structure
    contribs <- sapply(.contrib_types, contrib.url, repos = repo_dir)
    sapply(contribs, dir.create, recursive = TRUE, showWarnings = FALSE)
    
    # fill repo with files
    x <- pkgs
    if( !is.null(x) ){
        if( verbose ) message('Copying package files into repository')
        mapply(url.copy, x, contribs[package_type(x)])
    }
    
    # change to repo base directory
    if( verbose ) message('Building repository in ', repo_dir)
    od <- setwd(repo_dir)
    on.exit( setwd(od) )
    
    # write PACKAGES files
    makePACKAGES <- function(dir = '.', ...){
        od <- setwd(dir)
        on.exit( setwd(od) )
        
        if( verbose ) message('Generating PACKAGES file for ', dir, ' ... ', appendLF = FALSE)
        n <- write_PACKAGES('.', ...)
        if( verbose ) message('OK [', n, ']')
        n
    }
    
    n <- makePACKAGES(contrib.url('.'), ...)
    if( verbose ) message()
    
    # return repo URL
    invisible(paste0('file://', if( .Platform$OS.type == 'windows' ) "/" , repo_dir))
}

#' Enhanced Package Installation
#' 
#' These functions are enhanced versions of the base functions \code{\link{install.packages} and  
#' \link{available.packages}}.
#' 
#' \code{install.pkgs} installs packages, which can be local, remote or in a CRAN-like repository, 
#' possibly password protected.
#' Packages and their dependencies are automatically search in Bioconductor if needed.
#' 
#' @inheritParams utils::install.packages
#' @param siteRepos extra user-defined CRAN-like package repository
#' @param ... extra parameters eventually passed to the corresponding base function.
#' 
#' @import devtools
#' @rdname api
#' @export
install.pkgs <- function(pkgs, lib = NULL, siteRepos = NULL, type = getOption('pkgType'), dependencies = NA, ...){
    
    x <- pkgs
    
    # handle source/binary packages
    if( length(i_src <- grep("_", x)) ){
        # create temporary local repo to install from
        sx <- x[i_src]
        lrepo_path <- tempfile("tmprepo_")
        lrepo <- create_repo(lrepo_path, pkgs = sx)
        on.exit( unlink(lrepo_path, recursive = TRUE), add = TRUE)
        # install including local repo in repos list
        install.pkgs(package_name(sx), lib = lib, siteRepos = c(lrepo, siteRepos), type = type, dependencies = dependencies, ...)
        x <- x[-i_src]    
    }
    
    if( !length(x) ) return()
    
    # build complete repos list
    repos <- c(getOption('repos'), siteRepos)
    
    # check availability using plain repos list    
    p <- available.pkgs(contrib.url(repos, type = type))
    # update repos list (to get chosen CRAN mirror)
    repos <- c(getOption('repos'), siteRepos)
    
    # check missing dependencies
    deps <- NULL
    if( is_NA(dependencies) ) deps <- packageDependencies(x, all = FALSE, available = p)
    else if( isTRUE(dependencies) ) deps <- packageDependencies(x, all = TRUE, available = p)
    x_deps <- unique(c(x, deps[!is.na(deps)]))
    
    if( length(i_na <- which(!x_deps %in% p[, 'Package'])) ){ # try against Bioc repos
        message("Looking for dependencies in Bioconductor repos [", str_out(x_deps[i_na], total = TRUE), "]")
        p <- available.pkgs(contrib.url(.biocinstallRepos(repos), type = type))
        # use bioc repos list if more packages where found 
        if( any(x_deps[i_na] %in% p[, 'Package']) )
            repos <- .biocinstallRepos(repos)
    }
    
    # setup if needed
    .setup_rcurl()
    on.exit( .setup_rcurl(TRUE) )
    
    utils::install.packages(x, lib = lib, ..., dependencies = dependencies, repos = repos, type = type)
}

#' \code{available.pkgs} returns a matrix of the available packages.
#' The only difference with \code{\link{available.packages}} is that, if necessary, it uses a custom download 
#' method based on \pkg{RCurl} that can access password protected repositories. 
#' 
#' @rdname api
#' @export
#' 
available.pkgs <- function(...){
    
    # internal function that detects the presence of userpwd specification in contrib urls 
    .need_rcurl <- function(contriburl = contrib.url(getOption("repos"), type), type = getOption("pkgType")){
        has_userpwd(contriburl)
    }
    # setup custom rcurl only if necessary
    if( .need_rcurl(...) ){
        .setup_rcurl()
        on.exit( .setup_rcurl(TRUE) )
    }
    
    available.packages(...)
}

#' \code{exists.pkgs} tells if packages are available from repositories.
#' 
#' @param fixed logical that indicates if the elements of \code{x} should match  
#' exactly (\code{TRUE}) or interpreted and matched as regular expressions.
#' @param value logical, used only when \code{fixed=FALSE}, that indicates if the 
#' name of the matched packages should be returned (instead of a \code{TRUE/FALSE} 
#' logical value.
#' 
#' @rdname api
#' @export
exists.pkgs <- function(pkgs, repos = getOption('repos'), ..., value = FALSE, fixed = TRUE){
    # load available packages
    p <- available.pkgs(repos, ...)
    
    # match GPL ids against package names
    pn <- p[, 'Package']
    res <- if( fixed ) pkgs %in% pn else sapply(pkgs, grep, pn, value = value, simplify = FALSE)

    # use original names
    setNames(res, pkgs)
}

#' \code{Library} tries loading packages and install them if needed. 
#' 
#' @inheritParams base::library
#' 
#' @rdname api
#' @export
Library <- function(package, lib = NULL, ...){
    
      x <- package
      # load/install packages
      ol <- .libPaths()
      on.exit( .libPaths(ol) )
      .libPaths( c(lib, ol) )
      if( length(miss <- which(!sapply(x, require, lib = lib, character.only = TRUE))) ){
        pkgs <- x[miss]
        if( !is.null(lib) && !file.exists(lib) ) dir.create(lib, recursive = TRUE)
        install.pkgs(pkgs, lib = lib, ...)
        sapply(pkgs, library, character.only = TRUE, lib = lib)
        invisible(pkgs)
      }
}

