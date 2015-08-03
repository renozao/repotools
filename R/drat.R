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

load_repos_drat <- function(cache = cachefile('drat'), update = 'all'){
    
    DATA <- cache(cache, default = list(repos = NULL, PACKAGES = NULL))
    
    if( !is.na(cache) ) cache <- cachefile(DATA)
    message("Initial drat repos data ... ", appendLF = FALSE)
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
        message("Fetching drat repos list from github ... ", appendLF = FALSE)
        drat_forks <- fetch_drat_forks(verbose = FALSE)
        repos <- drat_forks
        message("OK [", length(repos), ' repositories]')
    }
    
    DATA$repos <- repos
    if( !is.na(cache) ){
        message("Updating drat repos data in '", cache, "' ... ", appendLF =FALSE)
        cache(cache, DATA)
        message(" OK")
    }
        
    # re-shape PACKAGES data
    new_repos <- setdiff(names(repos), rownames(PACKAGES_str))
    if( n_new <- length(new_repos) ){
        addon <- cbind(indexed_at = NA, matrix('', n_new, length(.repo_type), dimnames = list(NULL, .repo_type)))
        rownames(addon) <- new_repos
        PACKAGES_str <- rbind(PACKAGES_str, addon)
        stopifnot( setequal(names(repos), rownames(PACKAGES_str)) )
        PACKAGES_str <- PACKAGES_str[names(repos), ]
    }
    
    # process each repo
    if( 'PACKAGES' %in% update ){
        PACKAGES_str <- t(sapply(names(repos), function(n){
                            
            message("Updating index for ", n, " ... ", appendLF = FALSE)
            r <- repos[[n]]
            pack <- PACKAGES_str[n, .repo_type]
            # check last push and skip if not changed and already processed
            pushed_at <- r[['pushed_at']]
            gran_at <- PACKAGES_str[n, 'indexed_at']
            
            if( !is.na(gran_at) && pushed_at == gran_at ){
                res <- pack
                message(" SKIP ", appendLF = FALSE)
                
            }else{
                res <- sapply(.repo_type, function(type){                
                    # retrieve repo index
                    P <- drat_PACKAGES(type = type, user = r$owner$login, repo = r$name, verbose = FALSE)
                    if( is.null(P) ) P <- ''
                    P
                })
                message(" OK ", appendLF = FALSE)
            }
            # log content
            #sres <- names(res)[nzchar(res)]
            #message(sprintf('[%s]', paste(sres, collapse = '|')))
            message()
            
            # update indexing date
            res <- c(indexed_at = pushed_at, res)
            
            res
        }))    
        DATA$PACKAGES_str <- PACKAGES_str
        
        if( !is.na(cache) ){
            message("Updating PACKAGES files in '", cache, "' ... ", appendLF =FALSE)
            cache(cache, DATA)
            message(" OK")
        }
    }
        
    library(plyr)
    # process and check 
    # check validity
    if( 'index' %in% update ){
        message("* Checking validity ")
        USERS <- if( 'userdata' %in% update ) list() else DATA$USERS
        PACKAGES <- ldply(rownames(PACKAGES_str), function(n){
            rdata <- repos[[n]]
            pack <- PACKAGES_str[n, ]
            username <- rdata$owner$login
            
            message("  ** ", n, " ... ", appendLF = FALSE)
            res <- ldply(names(pack)[-1L], function(type){
                P <- pack[type]
                if( !nzchar(P) ) return()
                dcf <- try(read.dcf(textConnection(P)), silent = TRUE)
                if( !is(dcf, 'try-error') ){
                    
                    # check all user's repository to flag forked repos
                    user_repos <- USERS[[username]] %||% gh_user_repo(username)
                    USERS[[username]] <<- user_repos  
                    githubrepo <- dcf[, 'Package']
                    githubrepo[!dcf[, 'Package'] %in% names(user_repos)] <- NA
                    forked_repos <- names(which(sapply(user_repos, '[[', 'fork')))
                    forked <- dcf[, 'Package'] %in% forked_repos 
                    owned <- !forked & dcf[, 'Package'] %in% names(user_repos)
                    message(sprintf('%s: %i/%i ', type, sum(owned), nrow(dcf)), appendLF = FALSE)
                     
                    # extend PACKAGES fields
                    provider <- paste0(username, '.github.io')
                    provider_url <- file.path(provider, rdata$name)
                    relpath <- file.path(provider_url, contrib.path(type))
                                 
                    data.frame(GRANProvider = n, GRANType = 'drat', GRANdate = pack[['indexed_at']], Main = owned
                                , GithubRepo = githubrepo, GithubUsername = username, GithubFork = forked, GithubPushed = rdata$pushed_at
                                , Type = type
                                , Path = relpath
                                , dcf
                                , stringsAsFactors = FALSE)
                }
            })
            message()
            res
        })
        
        DATA$PACKAGES <- PACKAGES
        DATA$USERS <- USERS
    }
    
    if( !is.na(cache) ){
        message("Saving complete drat repos data in '", cache, "' ... ", appendLF =FALSE)
        cache(cache, DATA)
        message(" OK")
    }
    
    DATA
}

#' @importFrom RCurl url.exists
drat_PACKAGES <- function(user, type, repo = 'drat', verbose = TRUE){
    
    if( !verbose ) message <- function(...) NULL
     
    url <- gh_io.path(user, repo, contrib.path(type = type), 'PACKAGES')
    message("Fetch ", url, " ... ", appendLF = FALSE)
    if( url.exists(url) ){
        res <- getURL(url)
        message('OK')
        res
    }else{
        message('NO')
        NULL
    }
    
}

.repo_type <- c('source', 'mac.binary', 'win.binary')
contrib.path <- function(type){
    gsub('^/', '', contrib.url('', type = type))
}

# Adds a field to a DESCRIPTION/PACKAGE data
add_dcf_field <- function(x, name, value, force = FALSE){
    
    if( !name %in% colnames(x) ){
        x <- cbind(x, NA)
        colnames(x)[ncol(x)] <- name
    }
    
    # set values
    if( force ){
        x[, name] <- value
        
    }else{
        i <- is.na(x[, name])
        x[i, name] <- value[i]
    }
    
    x
    
}

write_PACKAGES_files <- function(x, path){
    
    write.fun <- if( length(x) ) write.dcf else function(x, file) cat('', file = file)
    
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    pfile <- file.path(path, 'PACKAGES')
    write.fun(x, pfile)
    
    # create .gz version
    gzfile <- gzfile(paste0(pfile, '.gz'))
    write.fun(x, gzfile)
    close(gzfile)
    
}

# adapted from utils:::available_packages_filters_db$R_version
R_dep_version <- function(depends, use.sign = FALSE){
    
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
#' @param dir path
GRAN.update_drat <- function(dir = '.', type = c('all', 'source', 'mac.binary', 'win.binary'), update = FALSE, repos = NULL){
     
    
    # load drat repos data
    DATA <- repos
    if( is.null(DATA) ) DATA <- load_repos_drat(update = update)
    
    build_type <- match.arg(type)
    if( build_type == 'all' ) build_type <- .repo_type
    
    repos <- DATA$repos
    message(sprintf("* Processing online repositories [%i repos]", length(repos)))
    
    # skip repos without PACKAGES data
    PACKAGES <- DATA$PACKAGES
    message(sprintf("* Repositories with PACAKGES data: %i repos", nrow(PACKAGES)))
    
    PACKAGES$R_version <- R_dep_version(PACKAGES$Depends)
    # built repos at all levels
    write_GRAN_repo <- function(var, FUN, ...){
        
        paths <- dlply(PACKAGES, 'Type', function(P){
            type <- unique(P[, 'Type'])
            message("* Setting up ", type, " packages ... ")
            dlply(P, var, function(P, ...){
                # extract top priority packages
                p <- FUN(P, ...)
                
                .sort <- function(P){
                    P[order(P[, 'Package'], package_version(P[, 'Version'])), ]
                }
                # add packages from other source, to honour top-priority
                # order by: package name, version
                PACKAGES <- PACKAGES[PACKAGES[, 'Main'], ] # only owned packages are addes
                PACK <- rbind(.sort(p$PACKAGES), .sort(PACKAGES))
                key <- apply(PACK[, c('Package', 'R_version')], 1L, paste0, collapse = "_")
                PACK <- PACK[!duplicated(key), ] 
                                
                # write files
                write_PACKAGES_files(PACK, file.path(p$path, contrib.path(type)))
                p$path
            }, ...)
        }, ...)

        # fix types with missing PACKAGES files
        paths <- unique(unlist(paths))
        l_ply(.repo_type, function(type){
            lapply(paths, function(p){
                tpath <- file.path(p, contrib.path(type))
                if( !file.exists(file.path(tpath, 'PACKAGES')) )
                    write_PACKAGES_files(NULL, tpath)
            })            
        })
        
    }
    
    basedir <- normalizePath(dir)
    # user-specific individual drat repos
    write_GRAN_repo('GRANProvider', function(P){
                reponame <- unique(P[, 'GRANProvider'])
                rdata <- repos[[reponame]]
                message(sprintf('  * Repo %s:%i ', reponame, nrow(P)))
                path <- file.path(basedir, rdata$owner$login, rdata$name)
                list(PACKAGES = P, path = path)
            })
    
    # user-specific repos
    write_GRAN_repo('GithubUsername', function(P){
                username <- unique(P[, 'GithubUsername'])
                message(sprintf('  * Repo %s:%i ', username, nrow(P)))
                path <- file.path(basedir, username)
                list(PACKAGES = P, path = path)
            })
    
    # global drat repos
    write_GRAN_repo('GRANType', function(P){
                message(sprintf('  * Repo all:%i ', nrow(P)))
                list(PACKAGES = P, path = basedir)
            })
    
    invisible()
    
}
