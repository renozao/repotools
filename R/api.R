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
.OS_contrib_types <- setNames(.contrib_types, c('unix', 'windows', 'mac'))

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
                makePACKAGES(contrib.url('.', type = t), type = t, ...)
    }, ...)
    if( verbose ) message()
    
    # return repo URL
    invisible(paste0('file://', if( .Platform$OS.type == 'windows' ) "/" , repo_dir))
}

contrib.url2 <- function(repos = getOption('repos'), type = getOption('pkgType')){
    
    os <- OS_type()
    if( type == 'both' && os != 'unix' ){
        btype <- paste0(substr(os, 1, 3), '.binary')
        type <- c(btype, 'source')
    }else if( type == 'win.both' ) type <- c('win.binary', 'source')
    else if( type == 'mac.both' ) type <- c('mac.binary', 'source')
    
    unname(sapply(type, function(t){
        res <- contrib.url(repos = repos, type = t)
        # update CRAN mirror if it was chosen in first round
        if( identical(unname(repos['CRAN']), '@CRAN@') ){
            cran_default <- getOption('repos')['CRAN']
            if( !is.na(cran_default) && !identical(unname(cran_default), '@CRAN@') )
                repos['CRAN'] <<- cran_default
        }
        res
    }))
}

contrib_bintype <- function(type = NULL){
    
    if( is.null(type) || type == 'both' ) .OS_contrib_types[OS_type()]
    else if( grepl('.both', type, fixed = TRUE) ){
        sprintf("%s.binary", substr(type, 1, 3))    
    }else if( type %in% .OS_contrib_types ) type
    else 'source'
}
    
OS_type <- function(){
    if( .Platform$OS.type == 'unix' ){
        if( length(grep("darwin", R.version$platform)) > 0 ) 'mac'
        else 'unix' 
    }else 'windows'
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
#' @param dry.run logical that indicates if one should only return the computed set of 
#' packages and dependencies to install.
#' @param devel indicates if development packages hosted on GRAN (GitHub) should be preferred to 
#' versions available in regular repositories.
#' The following values are allowed:
#' \itemize{
#' \item \code{FALSE}: package versions on regular repositories have priority over all other versions.
#' \item \code{TRUE}: 'release' GRAN versions (i.e. from master branches) 
#' are preferred to versions on regular repositories (if their version is number is larger) and 
#' 'devel' development versions (i.e. on branches that start with 'devel').
#' \item \code{2}: 'devel' GRAN versions are preferred over all other versions. 
#' }
#' 
#' In any case, packages not found in regular repositories are looked up on GRAN release, then GRAN devel 
#' if still not found.
#' @param verbose verbosity level
#' 
#' @import devtools
#' @importFrom tools md5sum
#' @rdname api
#' @export
install.pkgs <- function(pkgs, lib = NULL, siteRepos = NULL, type = getOption('pkgType'), dependencies = NA, available = NULL, ..., dry.run = FALSE, devel = FALSE, verbose = TRUE){
    
    # dump messages if requested
    if( !verbose ) message <- function(...) NULL
    
    x <- pkgs
    # fix type
    is.mac <- (length(grep("darwin", R.version$platform)) > 0)
    if( .Platform$OS.type == 'unix' && !is.mac && type == 'both' ){
        warning("WARNING: Switching to only package type allowed on nix machines ['source']", immediate. = TRUE)
        type <- 'source'
    } 
    
    # work with modified lib paths if requested
    if( !is.null(lib) ){
        ol <- .libPaths()
        .libPaths(c(lib, .libPaths()))
        on.exit( .libPaths(ol) )
    }
    
    # handle source/binary packages
    if( is.character(x) && length(i_src <- grep("_", x)) ){
        # create temporary local repo to install from
        sx <- x[i_src]
        lrepo_path <- tempfile("tmprepo_")
        lrepo <- create_repo(lrepo_path, pkgs = sx)
        on.exit( unlink(lrepo_path, recursive = TRUE), add = TRUE)
        # install including local repo in repos list
        install.pkgs(package_name(sx), siteRepos = c(lrepo, siteRepos), type = type, dependencies = dependencies, available = available, ..., dry.run = dry.run)
        x <- x[-i_src]    
    }
    
    if( !length(x) ) return()
    
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
    
    # build complete repos list
    repos <- c(getOption('repos'), siteRepos)
    
    if( is.data.frame(x) ){
        to_install <- x
        
    }else{
    
        .fields <- GRAN.fields()
        
        # check that all dependencies are available in the current loaded repo
        check_repo <- local({
            .all_available <- NULL
            f <- c('parent', 'name', 'compare', 'version', 'depLevel', 'depth', 'Source', 'idx')
            cNA <- as.character(NA)
            .pkgs <- data.frame(parent = pkgs, name = pkgs, cNA, cNA, cNA, 0, cNA, as.integer(NA), stringsAsFactors = FALSE)
            colnames(.pkgs) <- f
            .pkgs_init <- .pkgs 
            function(available, source, disjoint = FALSE, latest = FALSE){
                if( !nargs() ){
                    
                    if( all(is.na(.pkgs$idx)) ) res <- .pkgs
                    else{
                        .all_available <- .all_available[.pkgs$idx, , drop = FALSE]
                        if( all(is.na(.all_available[, .fields])) ) .fields <- NULL
                        #df <- as.data.frame(.all_available[, c('Package', 'Version', 'NeedsCompilation', .fields), drop = FALSE], stringsAsFactors = FALSE)
                        df <- as.data.frame(.all_available[, setdiff(unique(c(colnames(.all_available), .fields)), 'Source'), drop = FALSE], stringsAsFactors = FALSE)
                        res <- cbind(.pkgs, df) 	
                    }
                    
                    # order by depth 
                    res <- res[order(res[, 'depth'], decreasing = TRUE), , drop = FALSE]
                    # remove duplicates
                    h <- apply(res[, c('name', 'Source', 'idx')], 1L, digest)
                    res <- res[!duplicated(h), , drop = FALSE]
                    # re-order by depth 
                    res <- res[order(res[, 'depth']), , drop = FALSE]
                    if( !anyDuplicated(res$name) ){
                        rownames(res) <- res$name
                    }else if( !dry.run ) warning("Computed duplicated dependencies: installation will fail.")
                    return(res)
                }
                
                prev_hit <- setNames(.pkgs$Source, .pkgs$name)
                
                if( !nrow(available) ) return( list(found = character(), missing = sum(is.na(.pkgs$Source))) )
                
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
                        .pkgs <<- rbind(.pkgs_init, deps)
                    }    
                }
                
                # remove duplicates
                h <- apply(.pkgs[, c('name', 'compare', 'version')], 1L, digest)
                .pkgs <<- .pkgs[!duplicated(h), ]
                
                # MATCH MISSING
                i_avail <- match_available(.pkgs, .all_available, latest = latest)
                .pkgs$idx <<- i_avail
    #            message()
    #            print(.pkgs)
    #            print(i_avail)
    #            print(.all_available[i_avail[!is.na(i_avail)], 1:3])
                i_found <- which(!is.na(i_avail))
                # save source name
                if( length(i_found) )
                    .pkgs[i_found, 'Source'] <<- .all_available[i_avail[!is.na(i_avail)], 'Source']
                .pkgs[.pkgs$name == 'R', 'Source'] <<- ''

                found <- .pkgs[i_found, ]$name
                nR <- sum(.pkgs$name == 'R')
                i_changed <- which(!mapply(identical, unname(prev_hit[found]), unname(.pkgs$Source[i_found])))
                message("OK [Found ", length(i_found), "/", nrow(.pkgs) - nR, " package(s)"
                        , if( length(i_changed) ) paste0(": ", str_deps(.pkgs[i_found[i_changed], ]))
                        , "]")
                
                
                
                list(found = found, missing = sum(is.na(.pkgs$Source)))
            }
        })
        
        # check availability using plain repos list    
        p <- available.pkgs(contrib.url2(repos, type = type), fields = .fields)
        # update repos list (to get chosen CRAN mirror)
        repos <- c(getOption('repos'), siteRepos)
        
        repo_type <- if( is.null(siteRepos) ) 'default' else 'extended'
        message('* Using ', repo_type, ' repository list: ', str_out(repos, Inf))
        
        message("* Looking up available packages in ", repo_type, " repositories ... ", appendLF = FALSE)
        check_res <- check_repo(p, paste0('REPOS', if( !is.null(siteRepos) ) '*'))
        
        if( check_res$missing ){ # try against Bioc repos
            message("* Checking including Bioconductor repository ... ", appendLF = FALSE)
            bioc_repo <- .biocinstallRepos(repos)
            p_bioc <- available.pkgs(contrib.url2(setdiff(bioc_repo, repos), type = type), fields = .fields)
            # use Bioc repos if anything found (this includes CRAN)
            check_res <- check_repo(p_bioc, 'BioC', disjoint = TRUE)
            if( length(check_res$found) ) repos <- bioc_repo
        }
    
        if( check_res$missing ){ # try against Omega 
            message("* Checking including Omegahat repository ... ", appendLF = FALSE)
            p_omega <- available.pkgs(contrib.url2(omega_repo <- "http://www.omegahat.org/R", type = type), fields = .fields)
            # use Bioc repos if anything found (this includes CRAN)
            check_res <- check_repo(p_omega, 'Omega', disjoint = TRUE)
            if( length(check_res$found) ) repos <- c(repos, omega_repo)
        }
        
        # check GRAN repo (binary)
        if( type != 'source' && (check_res$missing || devel > 0) ){
            message("* Checking including binary packages in GRAN ... ", appendLF = FALSE)
            # select only the master versions
            p_gran <- GRAN.available(type = contrib_bintype(type), fields = .fields)
            check_res <- check_repo(p_gran, 'GRAN!', latest = TRUE)
            # add GRAN to repos list
            if( length(gran_pkg <- check_res$found) ){
               repos <- c(repos, GRAN.repos())
            }
        }
        
        # check GRAN repo
        if( check_res$missing || devel > 0 ){
            message("* Checking including source packages in GRAN ... ", appendLF = FALSE)
            # select only the master versions
            p_gran <- GRAN.available(type = 'source', fields = .fields, version = 'master')
            check_res <- check_repo(p_gran, 'GRAN', latest = devel > 0)
            # add GRAN to repos list
            if( length(gran_pkg <- check_res$found) ){
                ##repos <- c(repos, gran_repo)
            }
        }
        
        # check GRAN-dev repo
        if( check_res$missing || devel > 1 ){
            message("* Checking including source packages in GRAN (development version)... ", appendLF = FALSE)
            # select only the non-master versions
            p_granD <- GRAN.available(type = 'source', fields = .fields, version = '!master')
            check_res <- check_repo(p_granD, 'GRAN*', latest = devel > 1)
            # add GRAN to repos list
            if( length(granD_pkg <- check_res$found) ){
                ##repos <- c(repos, gran_repo)
            }
        }
        
        # retrieve pacakge list
        to_install <- check_repo()
    }
    
    # early exit if dry run
    if( dry.run ) return(to_install)
    
    to_install0 <- to_install
    
    # check R version
    if( iR <- match('R', to_install$name, nomatch = 0L) ){
        Rspec <- to_install[iR, ]
        Rspec <- paste0(Rspec$compare, Rspec$version)
        if( !testRversion(Rspec) ){
            stop("Package or dependency requires R ", Rspec)
        }
        to_install <- to_install[-iR, ]
    }
        
    if( length(not_found <- which(is.na(to_install$Source))) ){
        
        miss <- to_install[not_found, ]
        # missing packages
        warn <- paste0("Some packages could not be found in any repository: ")
        message("* ", warn)
        message("  - Missing packages: ", str_deps(miss[miss$parent == miss$name, ], Inf))
        # missing dependencies
        message("  - Missing dependencies: ", str_deps(miss[miss$parent != miss$name, ], Inf))
        if( length(req_missing <- which(miss$depth <= 1 & !miss$depLevel %in% 'Suggests')) ){
            stop("The following required packages could not be found: ", str_deps(miss[req_missing, ], Inf))
        }else{
            message("* ", warn)
            warning(warn)
        }
    }
    
    # install GRAN packages first
    if( !is.null(to_install$GHref) && length(i_gran <- which(grepl("GRAN", to_install$Source) & !is.na(to_install$GHref))) ){
        i_gran <- i_gran[order(to_install$depth[i_gran], decreasing = TRUE)]
        message("* Installing from GitHub: ", str_deps(to_install[i_gran, ], Inf))
        
        # store package hash before installing anything
        pkg_hash <- package.hash(to_install$name) 
        sapply(i_gran, function(i){
            # temporary set repos
            op <- options(repos = repos)
            on.exit( options(op) )
            
            # install from GitHub
            pkg <- to_install[i, ]
            install_github(pkg$name, pkg$GHuser, pkg$GHref)
        })
        to_install <- to_install[-i_gran, ]
        
        # re-evaluate which package still needs to be installed
        if( nrow(to_install) ){
            new_pkg_hash <- package.hash(to_install$name)
            pkg_hash <- pkg_hash[intersect(names(new_pkg_hash), names(pkg_hash))]
            to_install <- to_install[mapply(identical, pkg_hash, new_pkg_hash), ]
        }    
    }
    
    # install remaining packages from repositories
    if( nrow(to_install) ){
        
        # use the computed set of dependencies as available data  
        if( is.null(available) ) available <- to_install0
        
        # setup RCurl if needed
        if( .setup_rcurl(unique(as.character(available[, 'Repository']))) ) on.exit( .setup_rcurl(TRUE), add = TRUE)
        # setup repos
        op <- options(repos = repos)
        on.exit( options(op), add = TRUE)
        
        message("* Installing packages: ", str_deps(to_install, Inf))
        
        if( grepl('both', type, fixed = TRUE) ) type <- 'both'
        utils::install.packages(to_install$name, ..., dependencies = dependencies, available = available)
    }
    invisible(to_install0)
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
#' @inheritParams utils::download.packages
#' @rdname api
#' @export
#' 
download.pkgs <- function(pkgs, destdir, available = NULL, ...){
    
    if( is.null(available) ){
        available <- available.pkgs(...)
    }
    
    # internal function that detects the presence of userpwd specification in contrib urls 
    .urls <- function(contriburl = contrib.url(getOption("repos"), type), type = getOption("pkgType")){
        c(contriburl, unique(available[, 'Repository']))
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

