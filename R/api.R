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

.biocinstallRepos <- function(siteRepos = NULL, lib = lib){
    if( !require.quiet('BiocInstaller', character.only = TRUE, lib.loc = lib) ){
        sourceURL('http://www.bioconductor.org/biocLite.R')
    }
    library(BiocInstaller, lib.loc = lib)
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
    if( !is.null(pkgs) && is.character(pkgs) ){
        if( verbose ) message('Copying package files into repository')
        mapply(url.copy, pkgs, contribs[package_type(pkgs)])
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
    
    n <- sapply(.contrib_types, function(t){
                makePACKAGES(contrib.url('.', type = t), type = t, ...)
    })
    if( verbose ) message()
    
    # return repo URL
    invisible(paste0('file://', if( .Platform$OS.type == 'windows' ) "/" , repo_dir))
}

#' Enhanced Package Installation
#' 
#' These functions are enhanced versions of the base functions \code{\link{install.packages}},   
#' \code{\link{available.packages}} and \code{link{download.packages}} (see \emph{Details}).
#' 
#' The main differences with the base functions are that:
#' \itemize{ 
#' \item if necessary, it uses a custom download method based on \pkg{RCurl} that 
#' can access password protected repositories.
#' \item Bioconductor dependencies are automatically resolved, without the need to 
#' enable the BioC repositories (soft, data/annotation, etc..).
#' } 
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
    
    repo_type <- if( is.null(siteRepos) ) 'base' else 'extended'
    message('Using ', repo_type, ' repository list')
    # check missing dependencies
    deps <- NULL
    ndeps <- 0L
    if( !isFALSE(dependencies) ){
        if( is_NA(dependencies) ){
            message("Listing required dependencies ... ", appendLF = FALSE)
            deps <- packageDependencies(x, all = FALSE, available = p)
        }else if( isTRUE(dependencies) ){
            message("Listing all dependencies ... ", appendLF = FALSE)
            deps <- packageDependencies(x, all = TRUE, available = p)
        }
        deps <- unique(deps[!is.na(deps)])
        message('OK [', str_out(deps, total = TRUE), ']')
    }
    x_deps <- unique(c(x, deps))
    
    i_notbase <- which(!x_deps %in% p[, 'Package'])
    deps_found <- x_deps[-i_notbase]
    message("* Packages found in ", repo_type, " repositories: ", str_out(deps_found, total = TRUE))
    if( length(i_notbase) ){ # try against Bioc repos
        deps_notbase <- x_deps[i_notbase]
        message("* Checking Bioconductor repositories for unresolved dependencies [", length(deps_notbase), "/", length(x_deps), "]")
        p_bioc <- available.pkgs(contrib.url(.biocinstallRepos(repos, lib = lib), type = type))
        # use bioc repos list if more packages where found 
        if( length(i_bioc <- which(deps_notbase %in% p_bioc[, 'Package'])) ){
            deps_found <- c(deps_found, deps_notbase[i_bioc])
            repos <- .biocinstallRepos(repos, lib = lib)
            message("* Packages found in Bioconductor repositories: ", str_out(deps_notbase[i_bioc], total = TRUE))
        }
    }
    deps_notfound <- setdiff(x_deps, deps_found)
    if( length(deps_notfound) ){
        message("* Packages not found in any repositories: ", str_out(deps_notfound, total = TRUE))
    }
    
    # setup if needed
    if( .setup_rcurl(contrib.url(repos, type = type)) ) on.exit( .setup_rcurl(TRUE) )
    
    utils::install.packages(x, lib = lib, ..., dependencies = dependencies, repos = repos, type = type)
}

#' \code{available.pkgs} returns a matrix of the packages available in given repositories.
#' @rdname api
#' @export
#' 
available.pkgs <- function(...){
    
    # internal function that detects the presence of userpwd specification in contrib urls 
    .urls <- function(contriburl = contrib.url(getOption("repos"), type), type = getOption("pkgType")){
        contriburl
    }
    # setup custom rcurl only if necessary
    if( .setup_rcurl(.urls(...)) ) on.exit( .setup_rcurl(TRUE) )
    
    available.packages(...) 
}

#' \code{download.pkgs} downloads packages.
#' 
#' @inheritParams base::download.packages
#' @rdname api
#' @export
#' 
download.pkgs <- function(pkgs, destdir, available = NULL, ...){
    
    if( is.null(available) ){
        available <- available.pkgs(...)
    }
    
    # internal function that detects the presence of userpwd specification in contrib urls 
    .urls <- function(contriburl = contrib.url(getOption("repos"), type), type = getOption("pkgType")){
        contriburl
    }
    # setup custom rcurl only if necessary
    if( .setup_rcurl(.urls(...)) ) on.exit( .setup_rcurl(TRUE) )
    
    download.packages(pkgs, destdir, available = available, ...)    
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
      if( length(miss <- which(!sapply(x, require.quiet, lib = lib, character.only = TRUE))) ){
        pkgs <- x[miss]
        if( !is.null(lib) && !file.exists(lib) ) dir.create(lib, recursive = TRUE)
        install.pkgs(pkgs, lib = lib, ...)
        sapply(pkgs, library, character.only = TRUE, lib = lib)
      }
      invisible(x)
}

