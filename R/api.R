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

.biocinstallRepos <- function(siteRepos = NULL, lib = NULL){
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
create_repo <- function(dir = '.', type = NULL, pkgs = NULL, ..., clean = FALSE, verbose = FALSE){
    
    
    # clean root directory if requested
    if( clean ) unlink(dir, recursive = TRUE)
    
    # create root directory if needed
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    repo_dir <- normalizePath(dir)
    
    if( !is.null(type) ) .contrib_types <- type
    
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
    
    n <- sapply(.contrib_types, function(t, ...){
                makePACKAGES(contrib.url('.', type = t), type = t, fields = fields, ...)
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
    
    if( !is.null(lib) ){
        ol <- .libPaths()
        .libPaths(c(lib, .libPaths()))
        on.exit( .libPaths(ol) )
    }
    
    # handle source/binary packages
    if( length(i_src <- grep("_", x)) ){
        # create temporary local repo to install from
        sx <- x[i_src]
        lrepo_path <- tempfile("tmprepo_")
        lrepo <- create_repo(lrepo_path, pkgs = sx)
        on.exit( unlink(lrepo_path, recursive = TRUE), add = TRUE)
        # install including local repo in repos list
        install.pkgs(package_name(sx), siteRepos = c(lrepo, siteRepos), type = type, dependencies = dependencies, ...)
        x <- x[-i_src]    
    }
    
    if( !length(x) ) return()
    
    # utils to list dependencies
    str_deps <- function(x, n = 5){    
        v <- ifelse(is.na(x$compare), '', sprintf(" (%s %s)", x$compare, x$version))
        str_out(paste0(x$name, v), n, total = TRUE)
    }
    
    message("* Dependencies installation: ", appendLF = FALSE)
    if( isFALSE(dependencies) ) message("none")
    else {
        if( isTRUE(dependencies) ) dependencies <- 'all'
        else if( is_NA(dependencies) ) dependencies <- 'required'
        stopifnot( isString(dependencies) )
        
        spec <- dependencies
        dependencies <- ifelse(grepl('^all', spec), TRUE, NA)
        missing.only <- !grepl('!', spec, fixed = TRUE)
        shallow.deps <- !grepl('*', spec, fixed = TRUE)
        dtype <- ifelse(isTRUE(dependencies), 'all', 'required') 
        message(dtype, " [", ifelse(missing.only, "missing only", "re-install") , " - ", ifelse(shallow.deps, "shallow", "deep"), "]")
    }
    
    # check that all dependencies are available in the current loaded repo
    check_repo <- local({
        .all_available <- NULL
        f <- c('package', 'name', 'compare', 'version', 'depLevel', 'depth', 'Source', 'idx')
        cNA <- as.character(NA)
        .pkgs <- data.frame(package = pkgs, name = pkgs, cNA, cNA, cNA, 0, cNA, as.integer(NA), stringsAsFactors = FALSE)
        colnames(.pkgs) <- f
        function(available, source, disjoint = FALSE){
            if( !nargs() ){
                
                res <- cbind(.pkgs, as.data.frame(.all_available[.pkgs$idx, ], stringsAsFactors = FALSE))
                return(res)
            }
            
            if( is.null(.all_available) ) .all_available <<- cbind(available, Source = source)
            else{
                # only add non-overlapping packages
                if( disjoint ) available <- available[!available[, 'Package'] %in% .all_available[, 'Package'], ]
                .all_available <<- rbind(.all_available, cbind(available, Source = source))   
            }
                
            if( !isFALSE(dependencies) ){
                if( is_NA(dependencies) ){
                    deps <- packageDependencies(pkgs, all = NA, recursive = TRUE, missing.only = missing.only, available = .all_available, names.only = FALSE)
                }else if( isTRUE(dependencies) ){
                    deps <- packageDependencies(pkgs, all = ifelse(shallow.deps, TRUE, '*'), recursive = TRUE, missing.only = missing.only, available = .all_available, names.only = FALSE)
                }
                
                if( !is.null(deps) && nrow(deps) ){
                    deps$Source <- NA
                    deps$idx <- as.integer(NA)
                    .pkgs <<- rbind(.pkgs, deps)
                }    
            }
            
            # MATCH MISSING
            i_avail <- match_available(.pkgs, .all_available)
            .pkgs$idx <<- i_avail 
#            message()
#            print(.pkgs)
#            print(i_avail)
#            print(.all_available[i_avail[!is.na(i_avail)], 1:3])
            i_missing <- which(is.na(i_avail))
            i_found <- which(!is.na(i_avail))
            nR <- sum(.pkgs$name == 'R')
            message("OK [Found ", length(i_found), "/", nrow(.pkgs) - nR, " package(s)"
                    , if( length(i_found) ) paste0(": ", str_deps(.pkgs[i_found, ]))
                    , "]")
            # save source name
            if( length(i_found) )
                .pkgs[i_found, 'Source'] <<- .all_available[i_avail[!is.na(i_avail)], 'Source']
            .pkgs[.pkgs$name == 'R', 'Source'] <<- ''
            
            found <- .pkgs[i_found, ]$name
            list(found = found, missing = sum(is.na(.pkgs$Source)))
        }
    })
    
    # build complete repos list
    repos <- c(getOption('repos'), siteRepos)
    
    .fields <- c("GHuser", "GHref")
    
    # check availability using plain repos list    
    p <- available.pkgs(contrib.url(repos, type = type), fields = .fields)
    # update repos list (to get chosen CRAN mirror)
    repos <- c(getOption('repos'), siteRepos)
    
    repo_type <- if( is.null(siteRepos) ) 'base' else 'extended'
    message('* Using ', repo_type, ' default repository list: ', str_out(repos, Inf))
    
    message("* Looking up available packages in ", repo_type, " default repositories ... ", appendLF = FALSE)
    check_res <- check_repo(p, 'REPOS')
    
    if( check_res$missing ){ # try against Bioc repos
        message("* Checking available packages in Bioconductor ... ", appendLF = FALSE)
        bioc_repo <- .biocinstallRepos(repos)
        p_bioc <- available.pkgs(contrib.url(setdiff(bioc_repo, repos), type = type), fields = .fields)
        # use Bioc repos if anything found (this includes CRAN)
        check_res <- check_repo(p_bioc, 'BioC', disjoint = TRUE)
        if( length(check_res$found) ) repos <- bioc_repo
    }

    if( check_res$missing ){ # try against Omega 
        message("* Looking up available packages in Omegahat ... ", appendLF = FALSE)
        p_omega <- available.pkgs(contrib.url(omega_repo <- "http://www.omegahat.org/R"), fields = .fields)
        # use Bioc repos if anything found (this includes CRAN)
        check_res <- check_repo(p_omega, 'Omega', disjoint = TRUE)
        if( length(check_res$found) ) repos <- omega_repo
    }
    
    # check GRAN repo
    if( check_res$missing ){
        message("* Looking up available packages in GRAN ... ", appendLF = FALSE)
        gran_repo <- GRAN.repo()
        p_gran <- available.pkgs(contrib.url(gran_repo, type = 'source'), fields = .fields)
        check_res <- check_repo(p_gran, 'GRAN')
        # add GRAN to repos list
        if( length(gran_pkg <- check_res$found) ){
            ##repos <- c(repos, gran_repo)
        }
    }
    
    # check GRAN-dev repo
    if( check_res$missing ){
        message("* Looking up available packages in GRAN-dev ... ", appendLF = FALSE)
        granD_repo <- GRAN.repo('devel')
        p_granD <- available.pkgs(contrib.url(granD_repo, type = 'source'), fields = .fields)
        check_res <- check_repo(p_granD, 'GRAN')
        # add GRAN to repos list
        if( length(granD_pkg <- check_res$found) ){
            ##repos <- c(repos, gran_repo)
        }
    }
    
    # retrieve pacakge list
    to_install <- check_repo()
    
    # check R version
    if( iR <- match('R', to_install$name, nomatch = 0L) ){
        Rspec <- to_install[iR, ]
        Rspec <- paste0(Rspec$compare, Rspec$version)
        if( !testRversion(Rspec) ){
            stop("Package or dependency requires R ", Rspec)
        }
        to_install <- to_install[-iR, ]
    }
    
    print(to_install[, 1:7])
    
    if( length(not_found <- which(is.na(to_install$Source))) ){
        warn <- paste0("Some packages could not be found in any repository: ", str_deps(to_install[not_found, ]))
        message("* ", warn)
        if( length(req_missing <- which(to_install$depLevel[not_found] != 'Suggests' & to_install$depth[not_found] == 1)) ){
            stop("The following required packages could not be found: ", str_deps(to_install[not_found[req_missing], ], Inf))
        }else{
            message("* ", warn)
            warning(warn)
        }
    }
    
    # store package hash before installing anything
    pkg_hash <- package.hash(to_install$name) 
    
    # install GRAN packages first
    if( length(i_gran <- which(to_install$Source == 'GRAN')) ){
        i_gran <- i_gran[order(to_install$depth[i_gran], decreasing = TRUE)]
        message("* Installing from GitHub: ", str_out(to_install$name[i_gran], Inf))
        sapply(i_gran, function(i){
            # temporary set repos
            op <- options(repos = repos)
            on.exit( options(op) )
            
            # install from GitHub
            pkg <- to_install[i, ]
            install_github(pkg$name, pkg$GHuser, pkg$GHref)
        })
        to_install <- to_install[-i_gran, ]
    }
    
    # re-evaluate which package still needs to be installed
    new_pkg_hash <- package.hash(to_install$name)
    to_install <- to_install[mapply(identical, pkg_hash, new_pkg_hash), ]
    
    return(to_install)
    
    # setup if needed
    if( .setup_rcurl(contrib.url(repos, type = type)) ) on.exit( .setup_rcurl(TRUE), add = TRUE)
    
    utils::install.packages(to_install$name, ..., dependencies = dependencies, repos = repos, type = type)
}

#' \code{available.pkgs} returns a matrix of the packages available in given repositories.
#' @rdname api
#' @export
#' 
available.pkgs <- function(...){
    
    # internal function that detects the presence of userpwd specification in contrib urls 
    .urls <- function(contriburl = contrib.url(getOption("repos"), type), type = getOption("pkgType"), ...){
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

