# Project: repotools
# 
# Author: Renaud Gaujoux
# Created: Jul 30, 2015
###############################################################################


setOldClass('githubcontext')

# prototype object to interface with the github package
setClass("GithubInterface" 
        , representation(ctx = 'githubcontext')
        )

.GH <- new('GithubInterface')

setGeneric('.DollarNames', package='utils')

#' @S3method .DollarNames GithubInterface
.DollarNames.GithubInterface <- function(x, pattern = ""){
    
    if( !requireNamespace('github') ){
        stop("Could not query Github API: package 'github' is missing")
    }
    
    # remove leading "^"
	opattern <- sub("^[\\^]?(.*)", "\\1", pattern)
	grep(pattern, ls(asNamespace('github')), value=TRUE)
}

setMethod('.DollarNames', 'GithubInterface', .DollarNames.GithubInterface)

GH_fun <- function(name, exact = TRUE) gh_call(name)

setMethod('$', 'GithubInterface', function(x, name) GH_fun(name, exact = FALSE))
setMethod('[[', 'GithubInterface', function(x, i, exact=TRUE) GH_fun(i, exact = exact))

gh_context <- local({
    .ctx <- NULL
    function(reset = FALSE){
        if( is.null(.ctx) || reset ){
            # read credentials from .netrc file
            machines <- read_netrc()
            auth <- machines[machines[, 'machine'] == 'api.github.com' & machines[, 'login'] == 'app:repotools', , drop = FALSE][1L, ]
            token <- if( !is.na(auth['password']) ) auth['password']
            
            # connect  
            .ctx <<- create.github.context(access_token = token)
            
            # load cached etags from disk
            gh_cache <- cache('github', default = list(etags = NULL, cache = new.env(parent = emptyenv())))
            .ctx$etags <-  gh_cache$etags %||% .ctx$etags
            .ctx$cache <-  gh_cache$cache %||% .ctx$cache
        }
        .ctx
    }
})

gh_context_save <- function(){
    ctx <- gh_context()
    res <- cache('github', list(etags = ctx$etags, cache = ctx$cache))
}

gh_cache <- function(key, value){
    ctx <- gh_context()
    res <- ctx$cache
    if( !nargs() ) return(res)
    if( missing(value) ) return( res[[key]] )
    else{
        old <- res[[key]]
        res[[key]] <- value
        invisible(old)
    }
}

gh_call <- local({
    .rate <- NULL
    function(name){
        
        if( !requireNamespace('github') )
            stop(sprintf("Could not query Github API function '%s': package 'github' is missing", name))
            
        # ensure one uses the correct context
        .GH@ctx <- gh_context()
        # get function
        FUN <- get(name, asNamespace('github'))
        
        function(..., content.only = TRUE, nice = TRUE, ifnull = NA, per_page = Inf, ctx = .GH@ctx){
            
            library(github)
            .ctx <- gh_context()
            
            # check rate limit
            if( nice ){
                .rate <<- gh_rate_limit()
                if( .rate$core$remaining <= 0 ){
                    to_reset <- as.numeric(as.POSIXct(.rate$core$reset, origin="1970-01-01") - as.POSIXct(Sys.time()))
                    if( to_reset > 0 ){
                        warning(sprintf("Waiting for Github rate limit to reset in %ss", ceiling(to_reset))
                            , immediate. = TRUE)
                        Sys.sleep(ceiling(to_reset))
                    }
                }
            }
    
    #            api_req <- gsub(gh_api.path(''), '', url, fixed = TRUE)                
    #            res <- github:::.api.get.request(.ctx, api_req, params = c(list(...), list(per_page = per_page)))
            
            # call github function
            args <- list(...)
            args$ctx <- ctx
            if( !is.na(per_page) ){
                per_page <- min(100, per_page)
                args <- c(args, per_page = per_page)
            }
            res <- do.call(FUN, args)
            
            if( content.only ){
                res <- res$content
                
                # fix NULL elements recursively if requested
                if( !is.null(ifnull) && length(res) ){
                    .subst <- function(x){
                        i <- sapply(x, is.list)
                        x[i] <- sapply(x[i], .subst, simplify = FALSE)
                        x[-i] <- sapply(x[-i], '%||%', ifnull, simplify = FALSE)
                        x
                    }
                    
                    res <- .subst(res)
                }
            }
            res
        }
    }
})


# Custom wrapper calls to Github API 

gh_rate_limit <- function(){
    res <- .GH$.api.get.request('rate_limit', nice = FALSE, per_page = NA)
    res$resources
}

gh_user_repo <- function(user, repo = NULL, ...){
    
    res <- .GH$get.user.repositories(user)
    names(res) <- sapply(res, '[[', 'name')
    # subset
    if( !is.null(repo) ) res <- res[[repo]]
    res
}

gh_repo_forks <- function(user, repo, ...){
    res <- .GH$get.repository.forks(user, repo, ...)
    names(res) <- sapply(res, '[[', 'full_name')
    res
}

gh_repo_head <- function(user, repo, ref = NULL, ...){
    
    res <- .GH$get.repository.branches(user, repo)
    
    # early exit if no result
    if( !is.null(res$message) ) return(list())
    # use branch names as names
    names(res) <- sapply(res, '[[', 'name')
    # subset
    if( !is.null(ref) ) res <- res[[ref]]
    res
}

gh_get_content <- function(user, repo, ..., ref = NULL){
    path <- file.path(...)
    if( !length(path) ) path <- '.'
    res <- .GH$get.repository.path(user, repo, path = path, ref = ref)
    if( !is.null(res$message) ) return()
    names(res) <- sapply(res, '[[', 'name')
    res
}

gh_search_Rrepos <- function(query = NULL, ...){
    
    q <- 'language:R'
    if( !is.null(query) )
        q <- paste0(c(q, query), collapse = ' ')
    res <- .GH$search.repositories(q, per_page = Inf)
    if( !is.null(res$message) ) return(list())
    res
}
