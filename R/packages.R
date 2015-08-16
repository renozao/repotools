# Project: repotools
# 
# Author: Renaud Gaujoux
# Created: Jun 3, 2014
###############################################################################


#' @import digest
package.hash <- function(x){
        if( length(d <- find.package(x, quiet = TRUE)) ){
                digest(md5sum(list.files(d, full.names = TRUE, recursive = TRUE)))
        }else as.character(NA)
}
package.hash <- Vectorize(package.hash)

# match minimal/latest compatible version
match_available <- function(deps, available, latest = FALSE){
    
    if( is.integer(deps) ){
        available <- available[deps, , drop = FALSE]
        deps <- available[, 'Package']
    }

    dep_name <- if( is.character(deps) ) deps else deps$name

    # limit to packages in dependencies
    ia <- which(available[, 'Package'] %in% dep_name)
    if( !length(ia) ) return( setNames(rep(NA, length(dep_name)), dep_name))
    
    available <- available[ia, , drop = FALSE]
    
    # reorder always prioritizing the latest version
    if( latest ){
        ov <- order(package_version(available[, 'Version']), decreasing = TRUE)
        available <- available[ov, , drop = FALSE]
    } else ov <- seq(nrow(available))
    
    # case of a package name
    if( is.character(deps) ){
        i <- match(deps, available[, 'Package'])
        
    }else{
    
        # On Windows prefer versions that do not need compilation (i.e. binaries --- most likely)
        prefer_no_compile <- !latest && OS_type() == 'windows'

        #print(available)
        # check against version requirement
        i_available <- function(pkg, compare, version) {
            # NA if not found
            if ( !length(i <- which(available[, 'Package'] == pkg)) ) NA
            else if (is.na(compare)){ # no version requirement
                # limit to binary packages if necessary and possible
                if( prefer_no_compile && length(i) > 1L ){
                    compilation <- available[i, 'NeedsCompilation']
                    if( any(tolower(compilation) %in% 'yes') && length(i_bin <- which(is.na(compilation))) )
                        i <- i[i_bin]
                }
                unname(i[1L])
            }else{ # filter based on version requirement
                compare <- match.fun(compare)
                pass <- which(compare(package_version(available[i, 'Version']), version))
                if( length(pass) ){
                    i <- i[pass]
                    # limit to binary packages if necessary and possible
                    if( prefer_no_compile && length(i) > 1L ){
                        compilation <- available[i, 'NeedsCompilation']
                        if( any(tolower(compilation) %in% 'yes') && length(i_bin <- which(is.na(compilation))) )
                                i <- i[i_bin]
                    }
                    unname(i[1L])
                }else NA
            }
        }
        i <- as.integer(unlist(Map(i_available, deps$name, deps$compare, deps$version)))
    }

    # remap
    setNames(ia[ov[i]], dep_name)
}

#' @importFrom tools package.dependencies
list.package.dependencies <- function(db, all = FALSE, depth = 1L, reduce = TRUE){
    
    # empty result
    c0 <- character(0)
    cnames <- c('parent', 'name', 'compare', 'version', 'depLevel', 'depth', 'parentXDepends')
    empty <- matrix(NA, 0, length(cnames), dimnames = list(NULL, cnames))
    
    # early exit if no package passed
    if( !nargs() ) return(empty)
    
    dtype <- c('Depends', 'Imports')
    if( all ) dtype <- c(dtype, 'Suggests')
    deps <- sapply(dtype, function(x){
        d <- package.dependencies(db, check = FALSE, depLevel = x)
        d <- sapply(names(d), function(p){
                d <- d[[p]]
                if( is_NA(d) ) return()
                colnames(d) <- c('name', 'compare', 'version')
                d[, 'name'] <- str_trim(d[, 'name'])
                cbind(parent = p, d, depLevel = x)
            }, simplify = FALSE)
        d <- do.call(rbind, d)
    }
    , simplify = FALSE)
    
    # stick together
    deps <- do.call(rbind, deps)
#    str(deps)
    
    # set extra fields
    if( nrow(deps) ){
        deps <- add_dcf_field(deps, 'depth', depth, force = TRUE)
        # extended dependencies
        db <- add_dcf_field(db, 'XDepends')
        xdepends <- setNames(db[, 'XDepends'], db[, 'Package'])
        deps <- add_dcf_field(deps, 'parentXDepends', xdepends[deps[, 'parent']], force = TRUE)    
        
    }
    
    # only keep dependency with stricter requirement
    if( reduce ){
        deps <- reduce.dependencies(deps)
    }
    
    # return
    deps
}

reduce.dependencies <- function(deps){
    
    if( length(comp_i <- which(!is.na(deps[, 'compare']))) ){
        cdeps <- deps[comp_i, , drop = FALSE] 
        cdeps <- cdeps[order(cdeps[, 'compare'], cdeps[, 'name'], -order(package_version(cdeps[ , 'version']))), , drop = FALSE]
        deps <- rbind(cdeps, deps[-comp_i, , drop = FALSE])
    }
    deps <- deps[!duplicated(deps[, 'name']), , drop = FALSE]
    deps
}

gran_pattern <- function(x, close.path = TRUE){
    # espace dots
    x <- gsub(".", "\\.", x, fixed = TRUE)
    # add drat lookup and specification
    x <- sub("^([^/])/~$", "\\1/[^/]+/~.*", x)
    x <- sub("~$", "~.*", x)
    
    # ensure exact match on path component if requested
    if( close.path ) x <- sprintf("%s((/)|($))", x)
    
    x
}

# Finds dependencies honouring external repos weight 
match.dependencies <- function(deps, db, xdepends.only = FALSE){
    
    # limit lookup to packages whose parents have XDepends (if requested)
    if( xdepends.only )
        deps <- deps[!is.na(deps[, 'parentXDepends']), , drop = FALSE]
     
    # extract package names
    pkgs <- unique(deps[, 'name'])
    
    # limit db to lookup packages
    db <- db[db[, 'Package'] %in% pkgs, , drop = FALSE]
    
    # packages without XPath (main db) have top priority
    with.xpath <- which(!is.na(db[, 'XPath']))
    if( length(with.xpath) ){
        xdb <- db[with.xpath, , drop = FALSE]
        priority <- rep(NA, nrow(xdb))            
        db <- db[-with.xpath, , drop = FALSE]
    }
    
    .f <- c('Package', 'Version', 'XPath', 'XDepends')
    nomatch <- db[1L, ]
    nomatch[] <- NA
    res <- sapply(seq(nrow(deps)), function(i){
        
        d <- deps[i, ]
        # if parent has XDepends, then re-order the xdb accordingly
        # and append it to the main db
        if( nrow(xdb) && !is.na(r <- d[['parentXDepends']]) ){
            r <- strsplit(r, " +")[[1L]]
            r <- gran_pattern(r, close.path = FALSE)
            r <- r[nzchar(r)]
            w <- sapply(r, function(x) grepl(x, xdb[, 'XPath']))
            hit <- which(rowSums(w) > 0L) # a hit is when at least one XDepends pattern matched
            if( length(hit) ){
                xdb <- xdb[hit, , drop = FALSE][order(max.col(w)[hit]), , drop = FALSE]
                db <- rbind(db, xdb)
            }
            
            
        }
        
        # match package, taking into account version requirements
        db <- db[db[, 'Package'] %in% d[['name']], , drop = FALSE]
        if( !is.na(compare <- d[['compare']]) ){
            compare <- match.fun(compare)
            db <- db[which(compare(package_version(db[, 'Version']), d[['version']])), , drop = FALSE]
        }
        
        # return first match
        if( !nrow(db) ) return(nomatch)
        else db[1L, ]
                
    })

    res <- t(res)
    res <- cbind(deps, res, stringsAsFactors = FALSE)
#    print(res[, c(colnames(deps), .f)])
    res
}

# adapted from devtools::install_deps
list.dependencies <- function(pkg, available, all = NA, missing.only = FALSE, recursive = FALSE, reduce = TRUE, rm.base = TRUE) 
{
    
    # internal workhorse function that list direct dependencies
    .list_dep <- function(pkg, all, depth = 1L){
        
        # empty result
        c0 <- character(0)
        empty <- data.frame(parent = c0, query = c0, name = c0, compare = c0, version = c0, depLevel = c0, depth = numeric(0), stringsAsFactors = FALSE)
        
        # early exit if no package passed
        if( !nargs() ) return(empty)
        
        ipkg <- match_available(pkg, available)
        pkg_spec <- available[ipkg[!is.na(ipkg)], , drop = FALSE]
        # early exit if no package is found
        if( !nrow(pkg_spec) ) return(empty)
        
        # extract dependencies of each type
        dtype <- c('Depends', 'Imports')
        if( all ) dtype <- c(dtype, 'Suggests')
        deps <- sapply(dtype, function(x){
            d <- package.dependencies(pkg_spec, check = FALSE, depLevel = x)
            d <- sapply(names(d), function(p){
                    d <- d[[p]]
                    if( is_NA(d) ) return()
                    colnames(d) <- c('name', 'compare', 'version')
                    d[, 'name'] <- str_trim(d[, 'name'])
                    cbind(parent = p, d, depLevel = x)
                }, simplify = FALSE)
            d <- do.call(rbind, d)
        }
        , simplify = FALSE)
        # stick together
        deps <- as.data.frame(do.call(rbind, deps), stringsAsFactors = FALSE)
        # add depth
        if( nrow(deps) ) deps$depth <- depth
        
        #deps <- deps[deps$name != 'R', ]
        # filter already installed packages
        if( missing.only ){
            
            # NB: the lookup of the package location must use .libPaths otherwise loaded packages are used first
            # and obsolete packages may be installed in the first hit path but not be the one currently loaded
            needs_install <- function(pkg, compare, version) {
                if( pkg == 'R' ){
                    compare <- match.fun(compare)
                    !compare(Rversion(), version)
                }else if (length(pkg.loc <- find.package(pkg, lib.loc = .libPaths(), quiet = TRUE)) == 0) TRUE
                else if (is.na(compare)) FALSE
                else{
                    compare <- match.fun(compare)
                    !compare(packageVersion(pkg, lib.loc = dirname(pkg.loc)), version)
                }
            }
            needed <- as.logical(Map(needs_install, deps$name, deps$compare, deps$version))
            deps <- deps[needed, , drop = FALSE]
        }
        deps
    }
    
    pkg_list <- pkg
    deps <- .list_dep()
    depth <- 1L
    if( identical(all, '*') ){
        all_rec <- TRUE
        all <- TRUE 
    }else all_rec <- NA
    
    while(TRUE){
        
        # resolve direct dependencies
        n <- nrow(deps)
        new_deps <- .list_dep(pkg_list, isTRUE(all), depth)
        # update depth of previously looked-up dependencies
        deps$depth[deps$name %in% new_deps$parent] <- depth
        
        deps <- rbind(deps, new_deps)
        all <- all_rec
        depth <- depth + 1L
        # remove duplicated (keep larger depth)
        deps <- deps[!duplicated(paste0(deps$name, deps$compare, deps$version), fromLast = TRUE), ]
        
        if( n == nrow(deps) ) break;
        # resolve indirect dependencies
        if( !recursive ) break;

        pkg_list <- setdiff(deps$name, deps$parent)
        if( !length(pkg_list) ) break;
    }
    
    # remove base/recommended packages
    if( rm.base ){
        base_pkgs <- c("utils", "methods", "base", 'splines', 'graphics', 'stats', 'grDevices', 'tools', 'compiler', 'datasets', 'tcltk', 'stats4', 'grid', 'parallel') 
#        recom <- c("boot", "class", "cluster", "codetools", "foreign", "KernSmooth"
#                    , "lattice", "MASS", "Matrix", "mgcv", "nlme", "nnet", "rpart", "spatial", "survival")
        deps <- deps[! deps$name %in% base_pkgs, , drop = FALSE]
    }
    
    # reduce: keep max version and associate with larger depth (to allow inferring correct installation order)
    if( reduce && nrow(deps) ){
        if( anyDuplicated(deps$name) ){
            i <- split(seq(nrow(deps)), deps$name)
            sapply(i, function(i){
                if( length(i) > 1L ){
                    deps[i, 'depth'] <<- max(deps$depth[i], na.rm = TRUE)
                }
            })
            dname <- unique(deps$name)
            deps <- deps[orderVersion(deps$version, decreasing = TRUE), ]
            deps <- deps[!duplicated(deps$name), ]
            deps <- deps[order(match(deps$name, dname)), ]
        }
    }
    
    
    # return deps
    deps
}

#as.package_available <- function(x, available){
#    p <- available[, 'Package']
#    if( !x %in% p ) return(NA)
#    x <- structure(as.list(available[p == x, , drop = FALSE][1L, ]), class = 'package')
#    names(x) <- tolower(names(x))
#    x
#}


#' List Package Dependencies
#' 
#' @param x path to package source directory or file.
#' @param all logical that indicates if all dependencies should be returned,
#' or only the required ones.
#' @param available a matrix of available packages (as returned by \code{\link{available.packages}}), 
#' from which the dependencies are retrieved.
#' This means that there must be a row for the package \code{x}.
#' @param missing.only logical that indicates if only non-installed dependencies should be included in the 
#' result
#' @param recursive logical that indicates if indirect dependencies should also be included. 
#' @param as.list logical that indicates if the result should be a list with one element
#' per type of dependency.
#' @param names.only logical that indicates if the result should only include the dependencies package names
#' @param rm.base logical that indicates if base packages -- that come installed with any R installation -- 
#' should be exlcuded from the result.
#'  
#' @export
#' 
packageDependencies <- function(x, all = FALSE, available = NULL, missing.only = FALSE, recursive = FALSE, as.list = FALSE, names.only = TRUE, rm.base = TRUE){
    
    if( is_package_dir(x, check = TRUE) || is_NA(available) ){
        x <- available <- read.dcf(file.path(x, 'DESCRIPTION'))
    }else{
        if( is.null(available) ) available <- available.pkgs()
        p <- available[, 'Package']
        inp <- p %in% x
        if( !any(inp) ) return()
        x <- available[inp, , drop = FALSE]
    }
    
    deps <- list.dependencies(unique(x[, 'Package']), available, all = all, missing.only = missing.only, recursive = recursive, rm.base = rm.base)
    
    if( as.list ){
        
        ideps <- split(seq_along(deps$name), deps$depLevel)
        deps <- sapply(ideps, function(i){
                                    if( !names.only ) deps[i, , drop = FALSE]
                                    else unique(deps$name[i])
                            }, simplify = FALSE)
            
    }else if( names.only ) deps <- unique(deps$name)
    
    deps
    
    
}

# adapted from devtools::parse_deps
parse_deps <- function (string) 
{
	if (is.null(string)) 
		return()
	string <- gsub("\\s*\\(.*?\\)", "", string)
	pieces <- strsplit(string, ",")[[1]]
	pieces <- gsub("^\\s+|\\s+$", "", pieces)
	pieces[pieces != "R"]
}

pkg.dependencies <- function(pkg, dependencies = NA, ..., verbose = FALSE){
    
    if( length(pkg) == 1L && is_package_dir(pkg, check = TRUE) ){
        deps <- packageDependencies(pkg, available = NA, missing.only = FALSE, recursive = FALSE, names.only = FALSE)
        deps <- deps[deps$name != 'R', , drop = FALSE]
        pkg <- sprintf("%s (%s %s)", as.character(deps$name), deps$compare, deps$version)
        pkg[is.na(deps$compare)] <- as.character(deps$name)[is.na(deps$compare)]
    }
    install.pkgs(pkg, dependencies = dependencies, ..., dry.run = TRUE, verbose = verbose)
}

# utils to list dependencies
str_deps <- function(x, n = 5L){
    if( is.logical(n) ) n <- ifelse(n, Inf, 5L) 
        
    v <- ifelse(is.na(x$compare), paste0('-', x$Hit), sprintf(" (%s %s %s)", x$Hit, x$compare, x$version))
    str_out(paste0(x$name, ifelse(is.na(x$Hit), '', v)), n, total = TRUE)
}

mask_repos <- function(repos){
    gsub("(.+://)[^:]+:[^:]+@(.+)", "\\1\\2*", repos)
}
str_repos <- function(url, n = Inf, quote = FALSE, ..., mask.credentials = TRUE, details = FALSE, repos = NULL){
    
    out <- url
    if( details ){
        x <- strsplit(out, "/")
        #print(x)
        # extract versions
        v <- rep(NA, length(out))
        if( length(i <- grep("/([0-9.]+)/?$", out)) )
            v[i] <- gsub(".*/([0-9.]+)/?$", "\\1", out[i])
        # extract type
        t <- gsub(".*/([^/]+)/contrib/?.*", "\\1", out)
        # extract repos base
        b <- gsub("(.*)/((src)|(bin))/.*", "\\1", out)
        res <- data.frame(repos = b, base = b, type = t, version = v, stringsAsFactors = FALSE)
        if( !is.null(repos) ){
            repos <- sort(unique(repos), decreasing = TRUE)
            res$repos <- sapply(res$base, function(x){
                i <- pmatch(repos, x)
                repos[!is.na(i)][1L]
            })
        }
        i <- split(seq(nrow(res)), res$repos)
        if( mask.credentials ) res$repos <- mask_repos(res$repos)
        out <- sapply(i, function(i) sprintf("%s %s (%s total)", res$repos[i[1L]]
                                                                , paste0(unique(paste0(' [', res$type[i], ifelse(!is.na(res$version[i]), paste0('-', res$version[i]), ''), ']')), collapse = '')
                                                                , length(i)))
        return(out)
    }
    out <- unique(out)
    if( mask.credentials ) out <- mask_repos(out)
    
    # format output
    str_out(out, max = n, quote = quote, ...)
}

#' Installing All Package Dependencies
#' 
#' Install all dependencies from a package source directory or 
#' package source file. 
#' 
#' @param pkg package name, path or source file
#' @inheritParams install.pkgs
#' @param ... extra arguments passed to \code{\link{install.pkgs}}.
#' 
#' @export
#' @examples 
#' 
#' try( install.dependencies('Matrix', dep = "!", dry.run = TRUE) )
#' \dontrun{
#' install.dependencies("mypackage_1.0.tar.gz", dry.run=TRUE)
#' }
#' 
install.dependencies <- function(pkg, dependencies = NA, ..., verbose = TRUE, dry.run = FALSE) 
{
    # dump messages if requested
    miss_verb <- missing(verbose)
    if( !verbose ) message <- function(...) NULL
    
    # list dependencies
    deps <- pkg.dependencies(pkg, dependencies = dependencies, ..., verbose = (miss_verb && dry.run) || verbose > 1L)
    # remove R depends
    deps <- deps[deps$name != 'R', ]
    pkg_names <- deps$name[deps$depth == 0]
    deps <- deps[deps$depth > 0, , drop = FALSE]
    message("Package dependencies to install : ", str_deps(deps, Inf))
	if( !dry.run ){
        message("Installing ", nrow(deps), " dependencies")
		install.pkgs(., ..., available = deps, verbose = verbose)
	}
	invisible(deps)
}

write_PACKAGES_files <- function(x, path, ...){
    
    write.fun <- if( length(x) ) write.dcf else function(x, file, ...) cat('', file = file)
    
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    pfile <- file.path(path, 'PACKAGES')
    write.fun(x, pfile, ...)
    
    # create .gz version
    gzfile <- gzfile(paste0(pfile, '.gz'))
    write.fun(x, gzfile, ...)
    close(gzfile)
    
}

repotools_gh_rewrite <- function(db){

    # early exit if no Repository column
    if( !'Repository' %in% colnames(db) ) return(db)
    # substitute github and github.io URLs
#    message("Rewritting GRAN urls ... ", appendLF = FALSE)
    repo <- db[, 'Repository']
    repo <- gsub(".*/(github\\.com/.*)", "https://\\1", repo)
    repo <- gsub(".*/([^/]+\\.github\\.io/.*)", "http://\\1", repo)
#    message(sprintf("%i/%i", sum(db[, 'Repository'] != repo), nrow(db)))
    
    db[, 'Repository'] <- repo
    db
}

available.GRAN <- function(url = contrib.path(GRAN.repos(), type = type, version = release), type = getOption('pkgType'), release = NA
                            , fields = GRAN.fields(all = TRUE) 
                            , filters = .PACKAGES_filters_keep_all_versions, ..., drat.first = TRUE){
    
    # ensure the index is refreshed
    contrib_cache_clear(url)
                            
    gran <- available.packages(url, fields = fields, filters = c(repotools_gh_rewrite, filters), ...)
    
    # add internal repos depends
    gran <- add_dcf_field(gran, 'XDepends')
    depends_repos <- apply(gran, 1L, function(p){
            res <- p[['XDepends']]
            res <- if( !is.na(res) ) res
            u <- p[['GithubUsername']]
            if( !is.na(u) ) res <- c(file.path(u, '~'), u, res)
            paste0(unique(res, fromLast = TRUE), collapse = " ")
        })
    depends_repos[!nzchar(depends_repos)] <- NA
    gran <- add_dcf_field(gran, 'XDepends', depends_repos, force = TRUE)
    
    # add field XPath
    gran <- add_dcf_field(gran, 'XPath')
    gran <- add_dcf_field(gran, 'XPath'
                    , ifelse(gran[, 'GRANType'] %in% 'drat'
                            , sprintf("%s/%s/~%s", gran[, 'GithubUsername'], gran[, 'Package'], gran[, 'GithubRepo'])
                            , gran[, 'GRANPath'])
                    , force = TRUE)
    
    # re-order putting drat repos first if requested
    if( drat.first ){
        # original index
        i0 <- seq(nrow(gran))
        gran <- gran[order(gran[, 'Package'], -(gran[, 'GRANType'] %in% 'drat'), i0), ]
    }
    
    # add ID fields
    gran <- cbind(gran, UniqueID = apply(gran, 1L, digest))
    
    gran
}

repotools_gran <- function(db){
    
    config <- gran_target_config()
    
    gran.only <- config$gran.only %||% FALSE
    # toogle log messages
    verbose <- config$verbose %||% TRUE
    if( !verbose ) message <- function(...) NULL
    
    result <- function(){
        if( gran.only ) targets
        else db
    }
    
    targets <- config$gran
    message("* Checking GRAN packages ... ", appendLF = FALSE)
    # do nothing if no GRAN target
    if( is.null(config$gran) ){
        message("none")
        return( result() )
    }
    
    # remove GRAN targets from db
    db <- db[!db[, 'Package'] %in% targets[, 'name'], , drop = FALSE]
    
    message(sprintf("yes [%s]", str_out(rownames(targets), total = TRUE)))
        
    # infer index type from first db entry
    m <- str_match(db[1L, 'Repository'], "((src/contrib)|(bin/windows)|(bin/macosx))(/contrib/([0-9.]+))?")
    stopifnot( !is.na(m[, 2L]) )
    type <- .contrib_path2type[m[, 2L]]
    r_release <- m[, 7L]
    
    # retrieve GRAN index: result is ordered by Package, Drat version first, then original order
    message(sprintf("* Fetching GRAN index [%s] ... ", type), appendLF = FALSE)
    gran0 <- gran <- available.GRAN(type = type, release = r_release, drat.first = TRUE)
    breakdown <- summary(factor(na.omit(gran[, 'GRANType']))) + 0
    message(sprintf("OK [%i packages - %s]", nrow(gran), paste0(names(breakdown), ': ', breakdown, collapse = " | ")))
    
    # lookup for target GRAN packages
    hit <- sapply(gran_pattern(rownames(targets)), function(x) grep(x, gran[, 'XPath'])[1L])
    targets <- cbind(targets, gran[hit, , drop = FALSE])
    
    if( anyNA(hit) ){
        miss <- which(is.na(hit))
        warning(sprintf("Could not find package%s in GRAN: %s", ifelse(length(miss) > 1, 's', '')
                            , str_out(rownames(targets)[miss], Inf))
                            , call. = FALSE)
        # remove missing packages from targets
        targets <- targets[-miss, , drop = FALSE]
        hit <- hit[-miss]
    }
    
    # return main db if no GRAN hit
    if( !nrow(targets) ) return( result() )
    
    message("* Found GRAN packages: ", str_out(targets[, 'XPath'], Inf, total = TRUE))
    # remove alternative package versions from GRAN for matched full targets
    hit_full_packages <- targets[which(rownames(targets) == targets[, 'XPath']), 'Package']
    if( length(hit_full_packages) && length(alt_gran <- setdiff(which(gran[, 'Package'] %in% hit_full_packages), hit)) ){
        gran <- gran[-alt_gran, , drop = FALSE]
    }
    
    ## merge with db
    library(plyr)
    available <- rbind.fill.matrix(db, gran)
    # add ID fields
    available <- add_dcf_field(available, 'UniqueID', apply(available, 1L, digest))
    
    # compute GRAN dependencies
    .f <- c('Package', 'Version', 'XPath', 'XDepends', 'GRANType')
    GRAN_dep <- function(pkgs, available){
        
        gdb <- pkgs
        gdeps <- pkgs
        while(TRUE){
            # list dependencies
            deps <- list.package.dependencies(gdb, all = TRUE, reduce = TRUE)
            m <- match.dependencies(deps, available)
            # look for further GRAN dependencies
            gdb <- m[!is.na(m[, 'Package']) 
                            & !is.na(m[, 'XDepends']) 
                            & !m[, 'UniqueID'] %in% gdeps[, 'UniqueID'], , drop = FALSE]
            
            # add up but only keep latest version
            gdeps <- rbind.fill.matrix(gdeps, gdb)
            gdeps <- reduce.dependencies(gdeps)
            
            if( !nrow(gdb) ) break
        }
        
        rownames(gdeps) <- unname(gdeps[, 'Package'])
        gdeps
    }
    
    targets <- GRAN_dep(targets, available)
#    print(gran_pkgs[, .f])
    # merge/force GRAN targets and dependencies into main db
    if( !gran.only && nrow(targets) ){
        
        # remove target GRAN packages from main db
        db <- db[!db[, 'Package'] %in% targets[, 'Package'], , drop = FALSE]
        # merge
        db <- rbind(db, targets[, colnames(db), drop = FALSE])
    }
    
    result()
}

# Parse GRAN path from package specification
as_gran_spec <- function(pkgs){
    
    if( !length(pkgs) || is(pkgs, 'gran_spec') ) return(pkgs)
    
    stopifnot( is.character(pkgs) &&  is.null(ncol(pkgs)) )
    
    # init result
    pkgs <- unique(pkgs)
    res <- structure(list(pkgs = pkgs, gran = NULL), class = 'gran_spec')
    
    # split GRAN path
    PKGS <- t(sapply(strsplit(pkgs, "/", fixed = TRUE), function(x) x[1:3] ))
    colnames(PKGS) <- c('user', 'repo', 'ref')
    if( length(i_gran <- !is.na(PKGS[, 2L])) ){
        
        gran <- PKGS[i_gran, , drop = FALSE]
        # build GRAN path from specs
        rownames(gran) <- GRAN_key(gran)
        gran <- cbind(query = rownames(gran), gran, name = gran[, 'repo'])
        res$gran <- gran
        
        # replace GRAN path with package name
        pkgs[i_gran] <- gran[, 'name']
        res$pkgs <- pkgs
    }
    
    # return
    res
}

with_gran <- function(expr, pkgs = NULL){
    
    # setup installation targets and package filters if necessary
    pkgs <- as_gran_spec(pkgs)
    if( length(pkgs$gran) ){
        gran_target_config(pkgs)
        on.exit( gran_target_config(NULL) )
    }
    
    # override default package filters
    filters <- list("R_version", "OS_type", "subarch", repotools_gh_rewrite, repotools_gran, "duplicates")
    oo <- options(available_packages_filters = filters)
    on.exit( options(oo), add = TRUE)
    
    # setup RCurl custom download.file method
    if( .setup_rcurl() ) on.exit( .setup_rcurl(TRUE), add = TRUE)
    
    e <- parent.frame()
    eval(expr, env = e)
}


gran_target_config <- sVariable()

install_gran <- function(pkgs, lib, repos = getOption('repos'), ...){
    
    # detect gran packages: user/repo/ref
    specs <- as_gran_spec(pkgs)
    if( !length(specs$gran) ) 
        return(install.packages(pkgs, lib = lib, repos = repos, ...))
    
    # replace gran key with package name
    pkgs <- specs$pkgs
    
#    # append GRAN repo (not now: index will be fetched in GRAN filter 
#    repos <- c(repos, GRAN.repos())
    
    # ensure GRAN fields are loaded by hooking in
    # tools:::.get_standard_repository_db_fields
#    restore <- ns_hook('.get_standard_repository_db_fields', function(...){
#                res <- c(.object(...), GRAN.fields())
#                print(res)
#                res
#            }, 'tools')
#    on.exit( restore() ) 

    res <- with_gran({
        install.packages(pkgs, lib = lib, repos = repos, ...)
    }, pkgs = specs)
    
}

#' Computes GRAN Package Dependencies
#' 
#' @param pkgs A character vector of package keys, e.g., \code{'renozao/repotools'} or 
#' \code{'renozao/NMF/devel'}.
#' @param repos CRAN-like repositories urls
#' @param ... other parameters passed to \code{\link[utils]{available.packages}}.
#' @param verbose logical that toggles log messages.
#' 
#' @export
#' @examples 
#' 
#' gran_dependencies(c('renozao/NMF/devel', 'renozao/repotools'))
#' 
gran_dependencies <- function(pkgs, repos = getOption('repos'), ..., verbose = FALSE){
    
    # detect gran packages: user/repo/ref
    specs <- as_gran_spec(pkgs)
    if( !length(specs$gran) ) return()
    
    o <- options(repos = c(repos, GRAN.repos()))
    on.exit( options(o) )
    
    specs$gran.only <- TRUE
    specs$verbose <- verbose
    
    with_gran({
        available.packages(...)
    }, pkgs = specs)
    
}

ns_hook <- function(name, value, env, with.object = is.function(value)){
    
    # hook wrapper in render environment
    if( is.character(env) ) env <- asNamespace(env)
    
    # unlock
    locked <- bindingIsLocked(name, env)
    if( locked ) do.call("unlockBinding", list(name, env))
    
    # restoring function
    .object <- get(name, env)
    restore <- function(){
        ns_hook(name, .object, env, with.object = FALSE)
    }
    
    # add old object to function environment if requested
    if( with.object ){
        fe <- environment(value)
        assign(name, .object, fe)
    }
    
    # override function
    assign(name, value, env)
    # lock it again
    if( locked ) lockBinding(name, env)
    
    invisible(restore)
}
