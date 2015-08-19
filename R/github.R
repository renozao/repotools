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
            if( !qrequire('github') ) stop("Could not connect to Github API: missing package 'github'")
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

.substNULL <- function(x, val = NA){
    if( is.list(x) ){
        x <- sapply(x, .substNULL, val = val, simplify = FALSE)
    }else x <- x %||% val
    x
}

gh_call <- local({
    .rate <- NULL
    function(name){
            
        # ensure one uses the correct context
        .GH@ctx <- gh_context()
        # get function
        if( is.function(name) ) FUN <- name
        else{
            if( !requireNamespace('github') )
                stop(sprintf("Could not query Github API function '%s': package 'github' is missing", name))    
            FUN <- get(name, asNamespace('github'))
        }
        
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
#            str(args)
#            print(per_page)
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
                    res <- .substNULL(res, ifnull)
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

gh_user_repo <- function(user, repo = NULL, ..., all = TRUE, use.names = TRUE){
    
    F <- gh_call(function(user, ...){
                .GH$get.user.repositories(user, ..., content.only = FALSE) 
            })
    res <- gh_call_all(.GH$get.user.repositories, user, all = all, per_page = 100)
#    res <- .GH$get.user.repositories(user)
    if( !is.null(res$message) ) return(list())
    res <- res$content
    
    if( use.names ) names(res) <- sapply(res, '[[', 'name')
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
    
    res <- .GH$get.repository.branches(user, repo, per_page = NA)
    
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

gh_call_all <- function(FUN, ..., per_page = Inf, all = TRUE, join = TRUE){
    
    res <- FUN(..., per_page = per_page, content.only = FALSE)
    if( !all || !is.null(res$content$message) ){
        return(res)
    }
    
    # loop over pages if any
    link <- res$headers$link
    pcontent <- list(res$content)
    i <- 2L
    while( length(link) ){
        next_url <- sub(".*<([^>]+)>; rel=\"next\".*", "\\1", link)
        if( next_url == link ) break
        pres <- httr::GET(next_url) 
        h <- pres$headers
        link <- grep("^Link: ", h, value = TRUE)
        link <- gsub("^Link: ", "", link)
        co <- httr::content(pres)
        pcontent[[i]] <- co
        i <- i+1L
    }
        
    if( !join ) res$content <- pcontent
    else{
        
        jcontent <- pcontent[[1L]]
        . <- lapply(pcontent[-1L], function(x){
            if( !is.null(x$items) ) jcontent$items <<- c(jcontent$items, x$items)
            else jcontent <<- c(jcontent, x)
        })
        res$content <- .substNULL(jcontent)
    }
    
    # return
    res
}

gh_search_Rrepos <- function(query = NULL, ..., all = TRUE){
    
    q <- 'language:R'
    if( !is.null(query) )
        q <- paste0(c(q, query), collapse = ' ')
    
    res <- gh_call_all(.GH$search.repositories, q, all = all)
    # use full_name as names
    if( length(res$content$items) )
        names(res$content$items) <- sapply(res$content$items, '[[', 'full_name')
    
    res
}
