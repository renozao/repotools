# Project: GEOdb
# 
# Author: Renaud Gaujoux
# Created: Feb 25, 2014
###############################################################################

#' @include utils.R 
#' @include download.R
#' @include url.R
NULL

.biocinstallRepos <- function(siteRepos = NULL, lib = NULL){
    if( !qrequire('BiocInstaller', character.only = TRUE, lib.loc = lib) ){
        sourceURL('http://www.bioconductor.org/biocLite.R')
    }
    library(BiocInstaller, lib.loc = lib)
    biocinstallRepos(siteRepos)
}

package_name <- function(x){
    basename(gsub("_[0-9.]+\\.((tar\\.gz)|(zip)|(tgz))?$", "", x))
}

.package_type.reg <- c("\\.tar\\.gz", "\\.zip", "\\.tgz")
package_type <- function(x){
    t <- sapply(paste0(.package_type.reg, "$"), grepl, x)
    if( !is.matrix(t) ) it <- which(t) 
    else it <- apply(t, 1L, which)
    .contrib_types[it]
}
 
.contrib_types <- c('source', 'win.binary', 'mac.binary')
.contrib_ext <- setNames(c('tar.gz', 'zip', 'tgz'), .contrib_types)
.contrib_ext_types <- setNames(.contrib_types, c('tar.gz', 'zip', 'tgz'))
.contrib_url_types <- setNames(c(.contrib_types, 'mac.binary'), c('source', 'win.binary', 'mac.binary', 'mac.binary.mavericks'))
.OS_contrib_types <- setNames(.contrib_types, c('unix', 'windows', 'mac'))

contrib.url2 <- function(repos = getOption('repos'), type = getOption('pkgType')){
    
    os <- OS_type()
    if( type == 'both' && os != 'unix' ){
        btype <- paste0(substr(os, 1, 3), '.binary')
        type <- c(btype, 'source')
    }else if( type == 'win.both' ) type <- c('win.binary', 'source')
    else if( type == 'mac.both' ) type <- c('mac.binary', 'source')
    
    # non-interactive => use RStudio mirror if necessary
    if( identical(unname(repos['CRAN']), '@CRAN@') && !interactive() ){
        repos['CRAN'] <- 'http://cran.rstudio.com'
    }
    
    unname(sapply(type, function(t){
        res <- contrib.url(repos = repos, type = t)
        # update CRAN mirror if it was chosen in first round
        if( identical(unname(repos['CRAN']), '@CRAN@') ){
            cran_default <- getOption('repos')['CRAN']
            if( !is.na(cran_default) && !identical(unname(cran_default), '@CRAN@') )
                repos['CRAN'] <<- cran_default
        }
        # TODO: remove this (for debug)
#        res <- gsub("3.1", "3.0", res, fixed = TRUE)
        repos_url(res)
    }))
}

contrib_bintype <- function(type = NULL){
    
    if( is.null(type) || type == 'both' ) unname(.OS_contrib_types[OS_type()])
    else if( grepl('.both', type, fixed = TRUE) ){
        sprintf("%s.binary", substr(type, 1, 3))    
    }else if( type %in% .OS_contrib_types ) type
    else 'source'
}
    
#' Determinate Type of Operating System
#' 
#' Returns the type of OS. 
#' 
#' @return a single character string: code{'unix'}, code{'mac'}, or code{'windows'} 
#' depending on the OS of the calling machine.
#' 
#' @export
#' @examples 
#' 
#' OS_type()
#' 
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
#' \item Bioconductor (soft, data/annotation, etc..) and Omegahat dependencies 
#' are automatically resolved, without the need to enable these repositories; 
#' \item if necessary, they use an enhanced \emph{curl} binary that internally uses the \pkg{RCurl} 
#' package, which is configured to support authentication for password protected repositories.
#' Credentials can either passed embbedded within the URL or transparently taken from the 
#' user's \emph{.netrc} file;
#' \item it supports the mixed-type installation, of binary and source packages;
#' \item it can install packages and recursive dependencies hosted on GitHub, 
#' if these have been hooked to the GRAN repository.
#' } 
#' 
#' \code{install.pkgs} installs packages, which can be local, remote or in a CRAN-like repository, 
#' possibly password protected.
#' Packages and their dependencies are automatically search in Bioconductor, Omegahat and GRAN 
#' repositories if needed.
#' 
#' @inheritParams utils::install.packages
#' @param pkgs character vector of the names of packages whose current versions should be downloaded 
#' from the repositories or of path to source/binary package files (or a mixed of both).
#' @param repos URL or specification of CRAN-like package repository (see section \emph{Repositories}).
#' Use \code{repos = '+http://myrepo.org'} to append repositories to the default ones.
#' @param ... extra parameters eventually passed to the corresponding base function.
#' @inheritParams devtools::install
#' @param dry.run logical that indicates if one should only return the computed set of 
#' packages and dependencies to install.
#' If \code{NULL}, then it is internally set to \code{TRUE} only when there is a mismatch between
#' the requested and the OS binary package types (e.g., if \code{type = 'win.both'} on a Unix/Mac host).
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
#' @param verbose verbosity level (logical or numeric)
#' 
#' @section Repositories:
#' 
#' Respositories can be specified as a character vector that is processed in the following way:
#' 
#' \itemize{
#' \item \code{repos = NULL}, then the default set of repositories defined in option \code{'repos'} are used 
#' (see \code{getOption('repos')});
#' \item \code{repos = c('+http://one.repo.org', 'http://two.repo.org')} appends one or 
#' more repositories to the default set of repositories.
#' \item if an element of \code{repos} is \code{'@@CRAN@@'}, then the user is asked to choose a CRAN mirror, 
#' except if in non-interactive mode, where RStudio mirror is used (\url{http://cran.rstudio.com});
#' \item Full URL, that can be remote (start with 'http://') or local (start with file://), and 
#' may include authentication credentials in the form \code{'http://username:password@@cran.domain.org'}, 
#' for password protected repositories (Basic, Digest, etc..);
#' \item Repo URL shortcut key [+ path], defined as a string prefixed with \code{'@@'}, e.g., \code{'@@myRepo/path/to/repo'}, 
#' that matches a repository entry in file \code{'.netrc'} in the user's home directory -- as returned by 
#' \code{Sys.getenv('HOME')}.
#' It is internally substituted into a full repository base URL using by \code{repos_url} (see details in  
#' \code{\link{read_netrc}} and \code{\link{repos_url}} for details on how repository entries are defined 
#' and substituted respectively.
#' }
#' 
#' @import devtools
#' @importFrom tools md5sum
#' @export
install.pkgs <- function(pkgs, lib = NULL, repos = getOption('repos'), type = getOption('pkgType'), dependencies = NA, available = NULL, ..., quick = FALSE, dry.run = NULL, devel = FALSE, verbose = TRUE){
    
    mode <- 'install'

    # special evaluation for pkgs: NULL or .
    pkgs_symb <- substitute(pkgs)
    if( identical(pkgs_symb, as.symbol('.')) ) pkgs <- NULL
    if( is.null(pkgs) || is_NA(pkgs) ){ # all packages in available
        # dry.run shortcut
        if( is_NA(pkgs) ) dry.run <- TRUE 

        if( 'Package' %in% colnames(available) ){
            pkgs <- as.character(available[, 'Package'])
        }else stop("Could not find package names in `available` [", class(available), "]: must be a matrix or data.frame with a column 'Package'.")
    }
    
    substitute_q <- function(x, env) {
          call <- substitute(substitute(y, env), list(y = x))
          eval(call)
    }
    
    if( is.null(repos) ) repos <- getOption('repos')
    repos <- repos_url(repos)
    
    # detect situation where the package type(s) should be decided based on pkgs
    auto_type <- missing(type)
    
    # dump messages if requested
    if( !verbose ) message <- function(...) NULL
    if( is.infinite(verbose) ){ # enable rcurl debug mode
        Sys.setenv(R_REPOTOOLS_DEBUG='')
        on.exit(Sys.unsetenv('R_REPOTOOLS_DEBUG'), add = TRUE)
    }
    # infer dry.run if necessary: when there is mismatch between the requested and the OS binary types
    dry.run.show <- TRUE
    if( is.null(dry.run) ){
        dry.run <- contrib_bintype(type) != contrib_bintype()
        if( dry.run ) 
            message("NOTE: forcing dry run due incompatible binary package type [", contrib_bintype(type) ," vs. ", contrib_bintype(), " (OS)]")
    }else if( is_NA(dry.run) ){
        dry.run.show <- FALSE
        dry.run <- TRUE
    }
    
    x <- pkgs
    # fix type
    if( OS_type() == 'unix' && type == 'both' ){
        message("NOTE: Switching to the only package type allowed on nix machines ['source']")
        type <- 'source'
    } 
    
    # work with modified lib paths if requested
    if( !is.null(lib) ){
        ol <- .libPaths()
        .libPaths(c(lib, .libPaths()))
        on.exit( .libPaths(ol), add = TRUE)
    }
    
    # handle local source/binary packages
    loc_install <- NULL
    if( is.character(x) && length(i_src <- grep("((\\.tar\\.gz)|(\\.zip)|(\\.tgz))$", x)) ){
        # create temporary local repo to install from
        sx <- x[i_src]
        lrepo_path <- tempfile("tmprepo_")
        lrepo <- create_repo(lrepo_path, pkgs = sx)
        on.exit( unlink(lrepo_path, recursive = TRUE), add = TRUE)
        # check for source files and adapt type if necessary
        if( OS_type() != 'unix' && auto_type && any(grepl("\\.tar\\.gz$", sx)) ){
            type <- 'both'
        }
        # install including local repo in repos list
        loc_install <- install.pkgs(package_name(sx), repos = c(lrepo, repos), type = type
                                    , dependencies = dependencies, available = available, ...
                                    , devel = devel, verbose = verbose, dry.run = dry.run)
        # remove installed packages from query
        x <- x[-i_src]
        
        # early exit if everything is done
        if( !length(x) ) return(invisible(loc_install))
    }
    
    if( !length(x) ) return()
    
    if( dry.run && dry.run.show ) message("*** DRY RUN ***")
    
    message("* Dependency scope: ", appendLF = FALSE)
    if( isFALSE(dependencies) ) message("none")
    else {
        if( isTRUE(dependencies) ) dependencies <- 'all'
        else if( is_NA(dependencies) ) dependencies <- 'required'
        stopifnot( isString(dependencies) )
        
        # specify devel versions with '+' 
        dev.flag <- gregexpr('+', x, fixed = TRUE)[[1L]]
        if( !all(dev.flag == -1L) ){
            x <- gsub("+", "", x)
            dependencies <- paste0(dependencies, paste0(rep('+', length(dev.flag)), collapse = ''))
        }
        
        spec <- dependencies
        dependencies <- ifelse(grepl('^all', spec), TRUE, NA)
        missing.only <- !grepl('!', spec, fixed = TRUE)
        shallow.deps <- !grepl('*', spec, fixed = TRUE)
        # specify devel versions with '+' 
        dev.flag <- gregexpr('+', spec, fixed = TRUE)[[1L]]
        if( !all(dev.flag == -1L) ) devel <- min(length(dev.flag), 2L)
        dtype <- ifelse(isTRUE(dependencies), 'all', 'required') 
        message(dtype, " [", ifelse(missing.only, "missing only", "re-install") , " - ", ifelse(shallow.deps, "shallow", "deep"), "]")
    }
    
    # show details of some options
    opts <- c(mode = mode, type = ifelse(quick, 'quick', 'full'), version = ifelse(devel, ifelse(devel>1, 'development', 'stable'), 'release'))
    message("* Options: ", str_out(opts, Inf, quote = FALSE, sep = " | ", use.names = verbose > 1L))
    
    .fields <- GRAN.fields()
    
    # check that all dependencies are available in the current loaded repo
    check_repo <- local({
        pkgs <- x
        .all_available <- NULL
        f <- c('query', 'parent', 'name', 'compare', 'version', 'depLevel', 'depth', 'Source', 'idx', 'Hit')
        cNA <- as.character(NA)
        
        .pkgs <- data.frame(query = pkgs, parent = pkgs, name = pkgs, cNA, cNA, cNA, 0, cNA, as.integer(NA), cNA, stringsAsFactors = FALSE)
        colnames(.pkgs) <- f
        # add initial target version requirement if any
        if( length(iv <- grep("[? (]", pkgs)) ){
            m <- str_match(pkgs, "^[?]?([^ (]+)\\s*(\\(?\\s*([<>=]=?)\\s*([0-9.-]+).*)?")
            .pkgs[, c('parent', 'name', 'compare', 'version')] <- m[, c(2L, 2, 4:5)]
            pkgs <- m[, 2L]
        }
        
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
                
                if( !nrow(available) ){
                    message("NOTE [Empty]")
                    return( list(hit = character(), found = character(), missing = sum(is.na(.pkgs$Source))) )
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
                            deps$query <- NA
                            deps$Source <- NA
                            deps$idx <- as.integer(NA)
                            deps$Hit <- NA
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
                if( length(i_found) ){
                    p_found <- .all_available[i_avail[!is.na(i_avail)], , drop = FALSE]
                    .pkgs[i_found, 'Source'] <<- p_found[, 'Source']
                    .pkgs[i_found, 'Hit'] <<- p_found[, 'Version']
                }
                # R (fake non-NA source)
                .pkgs[.pkgs$name == 'R', 'Source'] <<- ''

                found <- .pkgs[i_found, ]$name
                nR <- sum(.pkgs$name == 'R')
                i_changed <- which(!mapply(identical, unname(prev_hit[found]), unname(.pkgs$Source[i_found])))
                new_hit <- .pkgs[i_found[i_changed], ]
                if( verbose <= 1 ){
                    message("OK [", if( !length(i_changed) ) "-" 
                                    else paste0("Hits: ", length(i_found), "/", nrow(.pkgs) - nR, " +", length(i_changed)), "]")
                }else message("OK ["
                                , if( length(i_changed) ){
                                    paste0("Hits: ", length(i_found), "/", nrow(.pkgs) - nR, " | ", str_deps(new_hit))
                                }else{ "-" } 
                                , "]")
                
                
                
                list(hit = new_hit$name, found = found, missing = sum(is.na(.pkgs$Source)))
        }
    })
    
    if( is.data.frame(x) ){
        to_install <- x
        
    }else if( !is.null(available) ){
        available <- as.matrix(available)
        check_repo(available, 'AVAIL', latest = devel > 0)
        to_install <- check_repo()
        repos <- unique(as.character(to_install$Repository))
        
    }else{
        
        # check availability using plain repos list
        p <- available.pkgs(contrib.url2(repos, type = type), fields = .fields, type = type)
        # update repos list with chosen CRAN mirror
        repos <- repos_url(repos)
        siteRepos <- setdiff(repos, getOption('repos'))
        default_repos <- setdiff(repos, siteRepos)
        
        message('* Initial lookup:\n  - Default repos: ', if( length(default_repos) ) str_repos(default_repos) else NA)
        repo_type <- 'default' 
        if( length(siteRepos) ){
            repo_type <- 'extended'
            message('  - Extra repos: ', str_repos(siteRepos))
        }
        repos <- repos_url(repos)
        
        message("* Looking up available packages in ", repo_type, " repositories ... ", appendLF = FALSE)
        check_res <- check_repo(p, paste0('REPOS', if( length(siteRepos) ) '*'), latest = devel > 0)
        
        if( check_res$missing ){ # try against Bioc repos
            message("* Checking including Bioconductor repository ... ", appendLF = FALSE)
            bioc_repo <- .biocinstallRepos(siteRepos)
            p_bioc <- available.pkgs(contrib.url2(setdiff(bioc_repo, repos), type = type), fields = .fields)
            # use Bioc repos if anything found (this includes CRAN)
            check_res <- check_repo(p_bioc, 'BioC', disjoint = TRUE, latest = devel > 0)
            if( length(check_res$hit) ) repos <- bioc_repo
        }
    
        # check Omegahat
        if( check_res$missing ){
            message("* Checking including Omegahat repository ... ", appendLF = FALSE)
            p_omega <- available.pkgs(contrib.url2(omega_repo <- "http://www.omegahat.org/R", type = type), fields = .fields)
            # use Bioc repos if anything found (this includes CRAN)
            check_res <- check_repo(p_omega, 'Omega', disjoint = TRUE)
            if( length(check_res$hit) ) repos <- c(repos, omega_repo)
        }
        
        # check GRAN repo (binary)
        if( type != 'source' && (check_res$missing || devel > 0) ){
            message("* Checking including binary packages in GRAN ... ", appendLF = FALSE)
            # select only the master versions
            p_gran <- GRAN.available(type = contrib_bintype(type), fields = .fields)
            check_res <- check_repo(p_gran, 'GRAN!', latest = TRUE)
            # add GRAN to repos list
            if( length(gran_pkg <- check_res$hit) ){
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
            if( length(gran_pkg <- check_res$hit) ){
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
            if( length(granD_pkg <- check_res$hit) ){
                ##repos <- c(repos, gran_repo)
            }
        }
        
        # retrieve pacakge list
        to_install <- check_repo()
    }
    
    to_install0 <- to_install
    # attache relevant repo list
    attr(to_install0, 'repos') <- repos
    
    # check R version
    if( iR <- match('R', to_install$name, nomatch = 0L) ){
        Rspec <- to_install[iR, ]
        Rspec <- paste0(Rspec$compare, Rspec$version)
        warn <- paste0("Package or dependency requires R ", Rspec)
        if( !testRversion(Rspec) ){
            if( !dry.run ) stop(warn)
            else{
                message("* WARNING: ", warn)
                warning(warn)
            }
        }
        to_install <- to_install[-iR, ]
    }
    
#    to_install[c(1, sample(nrow(to_install), 5)), 'Source'] <- NA
    # skip packages flagged as trials
    if( length(try_no_hit <- which(is.na(to_install$Source) & grepl("^[?]", to_install$query))) )
        to_install <- to_install[-try_no_hit,, drop = FALSE]
    
    # check not found
    if( length(not_found <- which(is.na(to_install$Source))) ){
        
        miss <- to_install[not_found, ]
        miss_pkg <- which(miss$parent == miss$name)
        miss_req <- setdiff(which(miss$depth <= 1 & !miss$depLevel %in% 'Suggests'), miss_pkg)
        miss_dep <- setdiff(which(miss$parent != miss$name), miss_req)
        warn <- paste0("repository lookup failed to locate some packages or dependencies: ")
        message("* WARNING: ", warn)
        # missing packages
        if( length(miss_pkg) ) message("  - Packages: ", str_deps(miss[miss_pkg, ], Inf))
        # missing required dependencies
        if( length(miss_req) ) message("  - Required dependencies: ", str_deps(miss[miss_req, ], Inf))
        # missing dependencies
        if( length(miss_dep) ) message("  - Indirect/optional dependencies: ", str_deps(miss[miss_dep, ], Inf))
        if( length(miss_req) ){
            msg <- paste0("The following required packages could not be found: ", str_deps(miss[miss_req, ], Inf))
            if( !dry.run ) stop(msg)
            else warning(msg)
        }
        to_install <- to_install[-not_found[c(miss_pkg, miss_req, miss_dep)], ]
    }
    
    # install remaining packages from repositories
    if( nrow(to_install) && !is.null(to_install$Repository) ){
        
        # use the computed set of dependencies as available data  
        if( is.null(available) ) available <- to_install0
        
        # setup RCurl if needed
        if( .setup_rcurl(unique(as.character(available[, 'Repository']))) ) on.exit( .setup_rcurl(TRUE), add = TRUE)
        # setup repos
        op <- options(repos = repos)
        on.exit( options(op), add = TRUE)
        
        # reorder with deepest dependencies first
        to_install <- to_install[order(to_install$depth, decreasing = TRUE), , drop = FALSE]
        
        # compute installation groups (source/binary/GRAN)
        # - on non-unix host, default install.packages does not handle mixed source/binary packages installed
        # - source GRAN packages need to be treated in a special way and installed last so that their dependencies
        # have already been installed
        install_groups <- list()
        # split by depth level
        dep_groups <- rev(split(seq(nrow(to_install)), to_install$depth))
        sapply(dep_groups, function(i, ...){
            to_install <- to_install[i, , drop = FALSE]
            # split by repo type
            repo_type <- ifelse(grepl('/src/contrib$', to_install[, 'Repository']), 'source', contrib_bintype(type))
            # add GRAN-src fake type
            if( !is.null(ghref <- to_install[['GithubRef']]) )
                repo_type[grepl("GRAN\\*?", to_install$Source) & !is.na(ghref)] <- 'zGRAN'
            repo_type <- factor(repo_type)
            # put last group's type first to allow optimal merging  
            if( length(install_groups) ){
                ltype <- tail(install_groups, 1L)[[1L]]$type
                if( ltype %in% levels(repo_type) )
                    repo_type <- relevel(repo_type, ltype)
            }
            type_groups <- split(seq(nrow(to_install)), repo_type)
            sapply(names(type_groups), function(t, ...){
                lg <- length(install_groups)
                addon <- to_install[type_groups[[t]], , drop = FALSE]
                if( lg && install_groups[[lg]]$type == t ){
                    install_groups[[lg]]$to_install <<- rbind(install_groups[[lg]]$to_install, addon)
                }else{
                    install_groups[[lg + 1L]] <<- list(to_install = addon, type = t)
                }
            }, ...)
        }, ...)
        ##

        ## Description of installation
        message("* Installing ", nrow(to_install), " package(s) as follows:")
        sapply(seq_along(install_groups), function(i){
                to_install <- install_groups[[i]]$to_install
                t <- install_groups[[i]]$type
                if( t == 'zGRAN' ) t <- 'GitHub'
                message("  - ", t, " package(s): ", str_deps(to_install, verbose > 1))
        })
        # list repositories from where packages are downloaded 
        all_repos <- to_install$Repository
        repos_desc <- str_repos(all_repos[!is.na(all_repos)], details = TRUE, repos = repos)
        if( length(repos_desc) ){
            message("* Using repositories: ")
            sapply(paste('  -', repos_desc), message)
        }
        #
        
        if( !dry.run ){
            message("")
            # install all groups
            sapply(seq_along(install_groups), function(i, ...){
                to_install <- install_groups[[i]]$to_install
                t <- install_groups[[i]]$type
                if( t == 'zGRAN' ) t <- 'GitHub'

                message("\n## Installing ", t, " package(s): ", str_deps(to_install, Inf))
                if( t == 'GitHub' ){
                    # store package hash before installing anything
                    apply(to_install, 1L, function(pkg){
                        # temporary set repos
                        op <- options(repos = repos)
                        on.exit( options(op) )
                        # install from GitHub
                        install_github(pkg['GithubRepo'], pkg['GithubUsername'], pkg['GithubRef'], quick = quick)
                    })   
                }else{
                    opts <- "--with-keep.source"
                    if (quick) {
                        opts <- c(opts, "--no-docs", "--no-multiarch", "--no-demo")
                    }
                    utils::install.packages(to_install$name, ..., dependencies = dependencies, available = available, type = t
                                            , INSTALL_opts = opts)
                }
            }, ...)
            message("\n# DONE")
        }
    }
    
    # add loc_install if necessary
    if( !is.null(loc_install) ){
        to_install0 <- rbind(loc_install, to_install0)
    }
    invisible(to_install0)
}

#' Working with Packages
#' 
#' 
#' \code{available.pkgs} returns a matrix of the packages available in given repositories.
#' @rdname api
#' @export
#' 
available.pkgs <- function(...){
    
    # internal function that detects the presence of userpwd specification in contrib urls 
    .local <- function(contriburl = NULL, type = getOption("pkgType"), ...){
        
        type_std <- ifelse(grepl('both', type), 'both', type)
        if( is.null(contriburl) ) contriburl <- contrib.url(getOption("repos"), type_std)
        
        # complete urls for user:passwd
        contriburl <- repos_url(contriburl)
        
        # setup custom rcurl only if necessary
        if( .setup_rcurl(contriburl) ) on.exit( .setup_rcurl(TRUE) )
        
        if( type_std == 'both' ){
            # load all versions
            available.packages(contriburl, ..., filters = c("R_version", "OS_type", "subarch"))
        }else available.packages(contriburl, ...)
        
    }
    .local(...)
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

#' @rdname api
#' @export
old.pkgs <- function(lib.loc = NULL, repos = getOption("repos"), available = NULL, ..., type = getOption("pkgType"), verbose = TRUE){
    
    # dump messages if requested
    if( !verbose ) message <- function(...) NULL
    
    # load installed packages
    inst <- installed.packages(lib.loc)
    if( is.null(available) ){
        # preform a fake installation available packages
        avail <- install.pkgs(rownames(inst), repos = repos, type = type, dry.run = NA, verbose = verbose)
        avail <- as.matrix(avail)
    }else{
        avail <- as.matrix(available)
        avail[avail[, 'Package'] %in% rownames(inst), , drop = FALSE]
    }
    
    ## cleanup/reformat
    # remove packages with no Hit
    avail <- avail[!is.na(avail[,'Package']),, drop = FALSE]
    # drop non-standard columns (repotools-specific)
    istd <- which(colnames(avail) == 'Package') - 1L
    extra <- avail[, 1:istd, drop = FALSE]
    avail <- avail[, -(1:istd), drop = FALSE]
    
    # call base function
    old <- old.packages(lib.loc = lib.loc, available = avail, instPkgs = inst)
    # re-attach extra fields
    cbind(extra[rownames(old), , drop = FALSE], old) 
}

#' @inheritParams install.pkgs
#' @inheritParams utils::update.packages
#' @param ask logical that specifies if the user should be asked before installling the available updates, 
#' or if these should be directly installed.
#'  
#' @rdname api
#' @export
update.pkgs <- function(lib.loc = NULL, repos = getOption("repos"), instlib = NULL, ask = TRUE, available = NULL, oldPkgs = NULL, ..., type = getOption("pkgType"), dry.run = NULL, verbose = TRUE){
    
    # load installed packages
    inst <- installed.packages(lib.loc, fields = GRAN.fields())
    # filter based on version
    inst <- inst[orderVersion(inst[, 'Version'], decreasing = TRUE), , drop = FALSE]
    inst <- inst[!duplicated(inst[, 'Package']), , drop = FALSE]
    
    if( !is.null(oldPkgs) ){
        old <- oldPkgs
        if( is.matrix(oldPkgs) ) old <- oldPkgs[, 'Package']
        inst <- inst[inst[, 'Package'] %in% old, ]
        
        if( !nrow(inst) ){
            warning("No installed package could be found in the set provided in argument `oldPkgs` [", str_out(old, total = TRUE), ']')
            return()
        }
    }
    
    # build query: request optional installation of package with version higher than the one installed
    query <- sprintf("?%s (> %s)", inst[, 'Package'], inst[, 'Version'])
    
    # force stopping if dry.run
    if( isTRUE(dry.run) ) ask <- TRUE
    
    if( !is.null(instlib) ){
        olib <- .libPaths()
        on.exit( .libPaths(olib) )
        .libPaths(c(instlib, olib))
    }
    if( is.null(available) ){
        # installation available packages
        up <- install.pkgs(query, repos = repos, type = type, ..., verbose = verbose, dry.run = ask)
        
    }else up <- install.pkgs(query, available = as.matrix(available), ..., verbose = verbose, dry.run = ask) 
    
    # only keep packages with a hit
    up <- up[!is.na(up[, 'Package']), , drop = FALSE]
        
    if( nrow(up) && !isTRUE(dry.run) && ask ){
        if( askUser(paste0("Do you want to proceed to the installation of the ", nrow(up), " package(s) as above specified?"), idefault = 'y') == 'n' ){
            message('Aborting...')
            return(invisible(up))
        }
        
        up <- install.pkgs(., lib = lib.loc, available = up, verbose = verbose)
    }
    
    invisible(up)
}


#' \code{Library} tries loading packages and install them if needed. 
#' 
#' @inheritParams base::library
#' 
#' @rdname api
#' @export
Library <- function(package, lib.loc = NULL, ...){
    
      x <- package
      # load/install packages
      ol <- .libPaths()
      on.exit( .libPaths(ol) )
      .libPaths( c(lib.loc, ol) )
      if( length(miss <- which(!sapply(x, require.quiet, lib = lib.loc, character.only = TRUE))) ){
        pkgs <- x[miss]
        if( !is.null(lib.loc) && !file.exists(lib.loc) ) dir.create(lib.loc, recursive = TRUE)
        install.pkgs(pkgs, lib = lib.loc, ...)
        sapply(pkgs, library, character.only = TRUE, lib = lib.loc)
      }
      invisible(x)
}

