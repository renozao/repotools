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

#' Parse Package Dependencies
#' 
#' @param string Specification of a set of dependencies either as a character vector
#' or as a single string as returned by [devtools::as.package] in fields `Depends`, 
#' `Imports` or `Suggests`.
#' @param full logical that indicates if version requirements should also be returned.
#' @param all loigical that indicates if R requirement and base packages should be 
#' indluded in the result.
#' 
#' @return a character vector if `full=FALSE` or a character matrix otherwise.
#' 
#' @examples
#' 
#' parse_deps_versions("a, b, c")
#' # with some version requirement
#' parse_deps_versions("a (>= 1.2.3), b, c (<= 2.5)")
#' 
#' @export
parse_deps_versions <- function(string, full = FALSE, all = FALSE){
  
  deps <- string
  # split list of dependencies if necessary
  if( length(deps) == 1L && grepl(",", deps) )
    deps <- strsplit(deps, ",")[[1L]]
  desp <- str_trim(deps)
  deps <- str_match(deps, "([^ ]+) *(\\(([><=]=) *([^(]+)\\))?")[, c(2, 4, 5), drop = FALSE]
  colnames(deps) <- c('package', 'op', 'version')
  rownames(deps) <- deps[, 'package']
  
  # remove R and base package if requested
  if( !all ){
    base_pkg <- character()
    deps[!rownames(deps) %in% c('R', base_pkg), , drop = FALSE]
  }
  # only return package names if requested
  if( !full ) deps <- deps[, 'package']
  
  deps
  
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
