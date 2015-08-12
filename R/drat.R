# Project: repotools
# 
# Author: Renaud Gaujoux
# Created: Jul 30, 2015
###############################################################################

cachefile <- function(name){
    
    
    path <- attr(name, 'cachefile')
    if( is.null(path) ){
        if( file.exists(name) ) return(name)
        
        cache_dir <- userData('cache', create = TRUE)
        dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
        path <- file.path(cache_dir, paste0(gsub("\\.rds$", "", name), '.rds'))
    }
    path
}

cache <- function(name, value, default = NULL){
    
    cache_file <- cachefile(name)
    if( is.na(name) ) return(default)
    else if( !missing(value) ){
        saveRDS(value, cache_file)
        res <- value 
    }else if( file.exists(cache_file) ){
        res <- readRDS(cache_file)
    }else res <- default
    
    
    if( !is.null(res) ) attr(res, 'cachefile') <- cache_file
    res
}

gh_repo_forks <- function(user, repo, ...){
    res <- gh_GET(gh_api.path('repos', user, repo, 'forks'), ...)
    names(res) <- file.path(sapply(res, function(x) x$owner$login), sapply(res, '[[', 'name'))
    res
}

fetch_drat_forks <- function(...) gh_repo_forks('eddelbuettel', 'drat', ...)

load_repos_drat <- function(cache = cachefile('drat'), update = 'all', force = FALSE){
    
    DATA <- cache(cache, default = list(repos = NULL, PACKAGES = NULL))
    
    if( !is.na(cache) ) cache <- cachefile(DATA)
    message("* Initial drat repos data ... ", appendLF = FALSE)
    message(sprintf("OK [%s repos | %s packages]", length(DATA$repos) %||% 0, nrow(DATA$PACKAGES) %||% 0))
    
    update.choice <- c('repos', 'PACKAGES', 'index', 'userdata')
    if( isFALSE(update) ) update <- 'none'
    if( isTRUE(update) ) update <- 'all'
    update <- match.arg(update, c('all', 'none', update.choice), several.ok = TRUE)
    if( 'none' %in% update ) return(DATA)
    if( 'all' %in% update ) update <- update.choice 
    
    repos <- DATA$repos
    PACKAGES_str <- DATA$PACKAGES_str
    
    if( is.null(repos) || 'repos' %in% update ){
        
        # get all forks from main drat repo
        message("* Fetching drat repos list from github ... ", appendLF = FALSE)
        drat_forks <- fetch_drat_forks(verbose = FALSE)
        repos <- drat_forks
        message("OK [", length(repos), ' repositories]')
    }
    
    DATA$repos <- repos
    if( !is.na(cache) ){
        message("* Updating cached drat repos data in '", cache, "' ... ", appendLF =FALSE)
        cache(cache, DATA)
        message(" OK")
    }
        
    library(plyr)
    # process each repo
    if( 'PACKAGES' %in% update ){
        message("* Fetching repos indexes")
        PACKAGES_str <- sapply(names(repos), function(n){
                            
            message("  ** ", n, " ... ", appendLF = FALSE)
            rdata <- repos[[n]]
            pack <- if( force ) list() else PACKAGES_str[[n]]
            # check last push and skip if not changed and already processed
            pushed_at <- rdata$pushed_at
            gran_at <- pack$indexed_at %||% NA
            
            if( !is.na(gran_at) && pushed_at == gran_at ){
                res <- pack
                message(sprintf(" CACHE [%i hit(s)]", nrow(res$PACKAGES) %||% 0L))
            }else{
                P_df <- ldply(.repo_type, function(type){                
                    # retrieve repo index
                    P <- drat_PACKAGES(type = type, user = rdata$owner$login, repo = rdata$name, verbose = FALSE)
                    if( !is.null(P) ){
                        data.frame(P, stringsAsFactors = FALSE)
                    }
                })
                message(sprintf(" OK [%i hit(s)]", nrow(P_df)))
                res <- list(indexed_at = pushed_at, PACKAGES = P_df)
            }
            # log content
            #sres <- names(res)[nzchar(res)]
            #message(sprintf('[%s]', paste(sres, collapse = '|')))
            
            # update indexing date
#            res <- cbind(indexed_at = pushed_at, res)
            res
        }, simplify = FALSE)
        DATA$PACKAGES_str <- PACKAGES_str
        
        if( !is.na(cache) ){
            message("* Updating cached PACKAGES files'", cache, "' ... ", appendLF =FALSE)
            cache(cache, DATA)
            message(" OK")
        }
    }
    
    # process and check 
    # check validity
    if( 'index' %in% update ){
        message("* Checking validity ")
        USERS <- if( 'userdata' %in% update ) list() else DATA$USERS
        PACKAGES <- ldply(names(PACKAGES_str), function(n){
            rdata <- repos[[n]]
            PACK <- PACKAGES_str[[n]]
            username <- rdata$owner$login
            
            message("  ** ", n, " ... ", appendLF = !nrow(PACK$PACKAGES))
            if( !nrow(PACK$PACKAGES) ) return()
            
            # look for cached parsed data
            if( !is.null(res <- PACK$dcf) ){
                s <- paste0('-', res$R_release)
                s[is.na(res$R_release)] <- ''
                s <- summary(factor(paste0(res$pkgType, s)))
                message(sprintf('CACHE [%s]', paste0(sprintf("%s:%s", names(s), s), collapse = " ")))
                return(res)
            }
                
            res <- ldply(seq(nrow(PACK$PACKAGES)), function(i){
                pack <- PACK$PACKAGES[i, ]
                P <- pack[['PACKAGES']]
                type <- pack[['pkgType']]
                r_release <- pack[['R_release']]
                dcf <- try(read.dcf(textConnection(P)), silent = TRUE)
                if( is(dcf, 'try-error') ) return()
                
                reponame <- rdata$name
                # check all user's repository to flag forked repos
                user_repos <- USERS[[username]] %||% gh_user_repo(username)
                USERS[[username]] <<- user_repos  
                forked_repos <- names(which(sapply(user_repos, '[[', 'fork')))
                forked <- dcf[, 'Package'] %in% forked_repos 
                owned <- !forked & dcf[, 'Package'] %in% names(user_repos)
                
                message(sprintf('%s%s: %i/%i ', type
                                              , if( !is_NA(r_release) ) paste0("-", r_release) else ''
                                              , sum(owned), nrow(dcf))
                        , appendLF = FALSE)
                
                # extend PACKAGES fields
                provider <- paste0(username, '.github.io')
                provider_url <- file.path(provider, rdata$name)
                relpath <- file.path(provider_url, contrib.path(type = type, version = r_release))
                data.frame(dcf
                            , Path = relpath
                            , pkgType = type
                            , R_release = r_release
                            , GRANPath = file.path(n, dcf[, 'Package']) 
                            , GRANType = 'drat', GRANdate = PACK$indexed_at
                            , GithubRepo = reponame, GithubUsername = username, GithubRef = NA
                            , GithubFork = ifelse(forked, 'yes', 'no')
                            , GithubPushed = rdata$pushed_at
                            , GithubOwner = ifelse(owned, 'yes', 'no')
                            , stringsAsFactors = FALSE)
            }, .id = NULL)
            message()
            
            # cache result
            PACKAGES_str[[n]]$dcf <<- res
            
            res
        })
        
        DATA$PACKAGES_str <- PACKAGES_str
        DATA$PACKAGES <- PACKAGES
        DATA$USERS <- USERS
    }
    
    if( !is.na(cache) ){
        message("* Updating cache in '", cache, "' ... ", appendLF =FALSE)
        cache(cache, DATA)
        message(" OK")
    }
    
    DATA
}

#' Fetch the Last N R Versions
#' 
#' This function uses \code[rversions]{r_versions} to fetch all the 
#' R versions from R main SVN server.
#' 
#' @param n Number of versions to return
#' @param digits Number of version digits: 1 = major version number, 
#' 2 = <major>.<minor1>, 3 = <major>.<minor1>.<minor2>
#' @param cache logical that indicates if the result should be retrieved from 
#' cache or (re-)fetched from the SVN server.
#' 
#' @importFrom rversions r_versions
#' @export
r_versions_n <- local({
    .versions <- NULL
    function(n = Inf, digits = 2L, cache = TRUE){
        # fetch versions if necessary
        .versions <<- {if( cache ) .versions} %||% r_versions(dots = TRUE)
        
        # process and limit
        vd <- sapply(strsplit(.versions[[1L]], ".", fixed = TRUE)
                    , function(x) paste0(head(x, digits), collapse = "."))
        tail(unique(vd), n)
    }
})

#' @importFrom RCurl url.exists
drat_PACKAGES <- function(user, type, repo = 'drat', verbose = TRUE){
    
    if( !verbose ) message <- function(...) NULL 
    .local <- function(url){
        message("Fetch ", url, " ... ", appendLF = FALSE)
        url_gz <- paste0(url, '.gz')
        if( url.exists(url) ){
            url <- url_gz
        }
        if( url.exists(url) ){
            res <- readURL(url, quiet = verbose < 2)
            message('OK')
            res
        }else{
            message('NO')
            NULL
        }
    }
    
    # build contrib sub-dir
    # NB: for binary packages we look back 5 R releases
    r_release <- NA
    if( type != 'source' ){
        r_release <- r_versions_n(5L)
    }
    contrib <- contrib.path(type = type, version = r_release)
    
    url <- gh_io.path(user, repo, contrib, 'PACKAGES')
    res <- lapply(url, .local)
    i <- which(lengths(res) > 0)
    if( length(i) ) cbind(pkgType = type, R_release = r_release[i], PACKAGES = as.character(res[i]))
    
}

.repo_type <- c('source', 'mac.binary', 'win.binary')
contrib.path <- function(..., type = getOption('pkgType'), version = NULL){
    p <- gsub('^/', '', contrib.url('', type = type))
    if( !is.null(version) && !is_NA(version) ) p <- file.path(dirname(p), version)
    file.path(..., p)
    
}

# Adds a field to a DESCRIPTION/PACKAGE data
add_dcf_field <- function(x, name, value, force = FALSE){
    
    # early exit if no input data
    if( !nrow(x) %||% FALSE ) return(x)
    
    if( !name %in% colnames(x) ){
        x <- cbind(x, NA)
        colnames(x)[ncol(x)] <- name
    }
    
    if( missing(value) ) return(x)
    
    value <- rep(value, length.out = nrow(x))
    
    # set values
    if( force ){
        x[, name] <- value
        
    }else{
        i <- is.na(x[, name])
        x[i, name] <- value[i]
    }
    
    x
    
}

# adapted from utils:::available_packages_filters_db$R_version
R_version_depends <- function(depends, use.sign = FALSE){
    
    res <- rep(NA, length(depends))
    depends[is.na(depends)] <- ""
    x <- lapply(strsplit(sub("^[[:space:]]*", "", depends), "[[:space:]]*,[[:space:]]*"), 
                function(s) s[grepl("^R[[:space:]]*\\(", s)])
    lens <- lengths(x)
    pos <- which(lens > 0L)
    if (!length(pos)) 
            return(res)
    lens <- lens[pos]
    x <- unlist(x)
    pat <- "^R[[:space:]]*\\(([[<>=!]+)[[:space:]]+(.*)\\)[[:space:]]*"
    ops <- sub(pat, "\\1", x)
    v_t <- sub(pat, "\\2", x)
    res[pos] <- if( use.sign ) paste0(v_t, ifelse(grepl("<", ops), '-', '+'))
                else paste0(ops, v_t)
    return(res)
}

# built repos at all levels
write_GRAN_repo <- function(var, FUN, ..., PACKAGES, no.dups = TRUE, append = FALSE){
    
    PACKAGES$R_version <- R_version_depends(PACKAGES$Depends)
    
    paths <- dlply(PACKAGES, c('pkgType', 'R_release'), function(P){
        type <- unique(P[['pkgType']])
        r_release <- unique(P[['R_release']])
        stopifnot( length(type) == length(r_release) && length(r_release) == 1L ) 
        message("* Setting up ", type, "-", r_release, " packages ... ")
        dlply(P, var, function(P, ...){
            # extract top priority packages
            p <- FUN(P, ...)
            message(p$msg, ' ', appendLF = FALSE)
            
            .sort <- function(P){
                P[order(P[, 'Package'], package_version(P[, 'Version'])), ]
            }
            # add packages from other source, to honour top-priority
            # order by: package name, version
            # only owned packages are added
            owned <- (PACKAGES[, 'GithubOwner'] %in% c('yes', NA)) & (PACKAGES[, 'GithubFork'] %in% c('no', NA))
            PACKAGES <- PACKAGES[ owned & PACKAGES[, 'pkgType'] == type, ] 
            PACK <- rbind.fill(.sort(p$PACKAGES), .sort(PACKAGES))
            # remove duplicated entries
            id <- apply(PACK[, c('GRANPath', 'MD5sum', 'Path')], 1L, paste0, collapse = "_")
            PACK <- PACK[!duplicated(id), ]
            
            message(sprintf("[%i total]", nrow(PACK)))
                            
            # write files
            write_PACKAGES_files(PACK, file.path(p$path, contrib.path(type = type, version = r_release)), append = append)
            p$path
        }, ...)
    }, ...)

    # fix types with missing PACKAGES files
    paths <- unique(unlist(paths))
    l_ply(.repo_type, function(type){
        lapply(paths, function(p){
            tpath <- file.path(p, contrib.path(type = type))
            if( !file.exists(file.path(tpath, 'PACKAGES')) )
                write_PACKAGES_files(NULL, tpath)
        })            
    })
    
}

#' Updates Repository Indexes of Built Packages
#' 
#' @details
#' Create a package repository that aggregates all GitHub source and 
#' \code{drat} repositories into a single one, compatible with 
#' \code{\link{install.packages}}.
#' 
#' GRAN repositories are served by \url{http://gran.r-forge.r-project.org}, 
#' which provides access to:
#' \itemize{
#' \item user-specific repositories, which favors packages distributed 
#' by a given GitHub user, e.g. \url{http://gran.r-forge.r-project.org/renozao};
#' \item a top-level cross-user repository, which only includes packages distributed by
#' their owner (forked repositories), \url{http://gran.r-forge.r-project.org};
#' \item development/source repository, \url{http://gran.r-forge.r-project.org/devel}
#' }
#' 
#' 
#' The top-level repository 
#' 
#' @param outdir path to output directory
GRAN.update_drat <- function(outdir = '.', type = c('all', 'source', 'mac.binary', 'win.binary'), update = FALSE, repos = NULL){
     
    
    library(plyr)
    # load drat repos data
    DATA <- repos
    if( is.null(DATA) ) DATA <- load_repos_drat(update = update)
    
    build_type <- match.arg(type)
    if( build_type == 'all' ) build_type <- .repo_type
    
    repos <- DATA$repos
    message(sprintf("* Processing online repositories [%i repos]", length(repos)))
    
    # skip repos without PACKAGES data
    PACKAGES <- DATA$PACKAGES
    message(sprintf("* Repositories with PACKAGES data: %i repos", nrow(PACKAGES)))
    
    basedir <- normalizePath(outdir)
    d_ply(PACKAGES, c('pkgType', 'R_release'), function(P){
        basedir <- file.path(contrib.path(basedir, type = P[1L, 'pkgType'], version = P[1L, 'R_release']))
        write_PACKAGES_files(P, basedir)
    })

    invisible(DATA)
    
}
