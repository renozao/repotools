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

load_repos_drat <- function(cache = cachefile('drat'), update = FALSE){
    
    DATA <- cache(cache, default = list(repos = NULL, PACKAGES = NULL))
    
    if( !is.na(cache) ) cache <- cachefile(DATA)
    message("Initial drat repos data ... ", appendLF = FALSE)
    message(sprintf("OK [%s repos | %s packages]", length(DATA$repos) %||% 0, nrow(DATA$PACKAGES) %||% 0))
    
    if( !update ) return(repos)
    
    repos <- DATA$repos
    PACK <- DATA$PACKAGES_str
    
    if( is.null(repos) || update ){
        
        # get all forks from main drat repo
        message("Fetching drat repos list from github ... ", appendLF = FALSE)
        drat_forks <- fetch_drat_forks(verbose = FALSE)
        repos <- drat_forks
        message("OK [", length(repos), ' repositories]')
    }
    
    DATA$repos <- repos
    if( !is.na(cache) ){
        message("Saving drat repos data in '", cache, "' ... ", appendLF =FALSE)
        cache(cache, DATA)
        message(" OK")
    }
        
    # re-shape PACKAGES data
    new_repos <- setdiff(names(repos), rownames(PACK))
    if( n_new <- length(new_repos) ){
        addon <- cbind(indexed_at = NA, matrix('', n_new, length(.repo_type), dimnames = list(NULL, .repo_type)))
        rownames(addon) <- new_repos
        PACK <- rbind(PACK, addon)
        stopifnot( setequal(names(repos), rownames(PACK)) )
        PACK <- PACK[names(repos), ]
    }
    PACKAGES <- PACK
    
    # process each repo
    PACK_NEW <- t(sapply(names(repos), function(n){
                        
        message("Updating index for ", n, " ... ", appendLF = FALSE)
        r <- repos[[n]]
        pack <- PACKAGES[n, .repo_type]
        # check last push and skip if not changed and already processed
        pushed_at <- r[['pushed_at']]
        gran_at <- PACKAGES[n, 'indexed_at']
        
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


    DATA$PACKAGES_str <- PACK_NEW
        
    library(plyr)
    # process and check 
    # check validity
    message("* Checking validity ")
    USERS <- list()
    PACKAGES <- ldply(rownames(PACK_NEW), function(n){
        rdata <- repos[[n]]
        pack <- PACK_NEW[n, ]
        username <- rdata$owner$login
        
        message("  ** ", n, " ... ", appendLF = FALSE)
        res <- ldply(names(pack)[-1L], function(type){
            P <- pack[type]
            if( !nzchar(P) ) return()
            dcf <- try(read.dcf(textConnection(P)), silent = TRUE)
            if( !is(dcf, 'try-error') ){
                
                # check all user's repository to flag forked repos
                user_repos <- gh_user_repo(username)
                USERS[[username]] <<- user_repos 
                forked_repos <- names(which(sapply(user_repos, '[[', 'fork')))
                forked <- dcf[, 'Package'] %in% forked_repos 
                owned <- !forked & dcf[, 'Package'] %in% names(user_repos)
                message(sprintf('%s: %i/%i ', type, sum(owned), nrow(dcf)), appendLF = FALSE)
                 
                # extend PACKAGES fields
                provider <- paste0(username, '.github.io')
                provider_url <- file.path(provider, rdata$name)
                relpath <- file.path(provider_url, contrib.path(type))
                             
                data.frame(GRANKey = n, GRANProvider = 'drat', GRANdate = pack[['indexed_at']], Main = owned
                            , GithubRepo = rdata$name, GithubUsername = username, GithubFork = forked, GithubPushed = rdata$pushed_at
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
    
    if( !is.na(cache) ){
        message("Saving drat repos data in '", cache, "' ... ", appendLF =FALSE)
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

#' Updates Repository Indexes of Built Packages
#' 
#' @details
#' Create a package repository that aggregates all GitHub source and 
#' \code{drat} repositories into a single one, compatible with 
#' \code{\link{install.packages}}.
#' 
#' @param dir path
GRAN.update_built <- function(dir = '.', type = c('all', 'source', 'mac.binary', 'win.binary'), update = FALSE, repos = NULL){
     
    
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
    
    # built local repo structure for each one
    sapply(unique(PACKAGES[, 'GRANKey']), function(reponame){
                
        message("  * Setting up ", reponame, " ... ", appendLF = FALSE)
        rdata <- repos[[reponame]]
        sapply(build_type, function(type){
            
            P <- PACKAGES[PACKAGES[, 'GRANKey'] %in% reponame & PACKAGES[, 'Type'] == type, ]
            if( !nrow(P) ) return()
            
            # lookup in type sub-directory
            baseurl <- contrib.url(dir, type = type)
            basedir <- normalizePath(dir)
            
#            provider <- paste0(rdata$owner$login, '.github.io')
#            providerurl <- file.path(provider, rdata$name)
#            relpath <- file.path(provider_url, contrib.path(type))
            path <- file.path(basedir, rdata$owner$login, rdata$name, contrib.path(type))
            dir.create(path, recursive = TRUE, showWarnings = FALSE)
            pfile <- file.path(path, 'PACKAGES')
            
            
            # add Path and extra GitHub field and re-write
#            P <- read.dcf(pfile, all = TRUE)
#            P <- add_dcf_field(P, 'Path', relpath, force = TRUE)
#            P <- add_dcf_field(P, 'GRANProvider', 'drat')
#            P <- add_dcf_field(P, 'GRANSource', provider_url)
#            P <- add_dcf_field(P, 'GithubRepo', rdata$name)
#            P <- add_dcf_field(P, 'GithubUsername', rdata$owner$login)
#            P <- add_dcf_field(P, 'GithubPushed', rdata$pushed_at)
            
#            lapply(GRAN.fields(), function(f){
#                        
#                # add column if necessary
#                P <<- add_dcf_field(P, f, val)
#                
#            })
            write.dcf(P, pfile)
            
            # create .gz version
            gzfile <- gzfile(paste0(pfile, '.gz'))
            write.dcf(P, gzfile)
            close(gzfile)
            
            message(sprintf('%s:%i ', type, nrow(P)), appendLF = FALSE)
            
            P
        }, simplify = FALSE)
        message('')
    }, simplify = FALSE)
    invisible()
    
}

