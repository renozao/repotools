# Project: repotools
# 
# Author: Renaud Gaujoux
# Created: Jul 30, 2015
###############################################################################

gh_call0 <- local({
    .rate <- NULL
    function(url, content.only = TRUE, nice = TRUE, ...){
    
        library(curl)
        agent <- 'Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.8.1.13) Gecko/20080311 Firefox/2.0.0.13'
        agent <- 'GRAN from repotools'
        gh_handle <- new_handle()
        handle_setheaders(gh_handle, 'User-Agent' = agent, ...)
        
        # check rate limit
        if( nice ){
            .rate <<- gh_rate_limit()
            if( .rate$resources$core$remaining <= 0 ){
                to_reset <- as.numeric(as.POSIXct(.rate$resources$core$reset, origin="1970-01-01") - as.POSIXct(Sys.time()))
                if( to_reset > 0 ) Sys.sleep(ceiling(to_reset))
            }
        }
        
        if( content.only ){
            con <- curl(url, handle = gh_handle)
            on.exit( close(con) )
            readLines(con)
        } else{
            curl_fetch_memory(url, handle = gh_handle)   
        }
    }
})

gh_GET0 <- local({
    .cache <- list()
    function(url, parse.json = TRUE, cache = TRUE, verbose = TRUE){
        
        url <- gh_api.path(paste0(url, collapse = '/'))
        res <- .cache[[url]]
        if( is.null(res) || !isTRUE(cache) ){
            
            if( verbose ) message("Fetching: '", url, "'")
            if( is.logical(cache) ) res <- gh_call0(url, content.only = FALSE)
            else{
                github_cache <- cache(cachefile('github', default = list()))
                
                res <- gh_call0(url, 'If-Modified-Since' = cache, content.only = FALSE)
#                on.exit({
#                    # update cache
#                    github_cache[[res]]
#                    saveRDS(github_cache, cachefile(github_cache))
#                })
            }
            .cache[[url]] <<- res
        }
        
        # return content
        ans <- rawToChar(res$content)
        if( parse.json ) ans <- jsonlite::fromJSON(ans)
        ans
    }
})


gh_context <- local({
    .ctx <- NULL
    function(){
        if( is.null(.ctx) ){
            .ctx <<- create.github.context(access_token = '617676d79833944dd3be391e19c120099faf8428')
            # load cached etags from disk
            cached <- cache('github', default = NULL)
            .ctx$etags <-  cached$etags %||% .ctx$etags
        }
        .ctx
    }
})

gh_context_save <- function(){
    ctx <- gh_context()
    cache('github', ctx$etags)
}

gh_call <- local({
    .rate <- NULL
    
    function(url, content.only = TRUE, nice = TRUE, ..., per_page = Inf){
        
        library(github)
        .ctx <- gh_context()
        
        # check rate limit
        if( nice ){
            .rate <<- gh_rate_limit()
            if( .rate$resources$core$remaining <= 0 ){
                to_reset <- as.numeric(as.POSIXct(.rate$resources$core$reset, origin="1970-01-01") - as.POSIXct(Sys.time()))
                if( to_reset > 0 ){
                    warning(sprintf("Waiting for Github rate limit to reset in %ss", ceiling(to_reset))
                        , immediate. = TRUE)
                    Sys.sleep(ceiling(to_reset))
                }
            }
        }
        
        api_req <- gsub(gh_api.path(''), '', url, fixed = TRUE)
        per_page <- min(100, per_page)
        res <- github:::.api.get.request(.ctx, api_req, params = c(list(...), list(per_page = per_page)))
        if( content.only ) res <- res$content
        res
    }
})

gh_api_call <- function(...) gh_call(..., content.only = TRUE)

gh_api_fetch <- function(...) gh_call(..., content.only = FALSE)

gh_GET <- function(url, verbose = FALSE, ...){
        
    if( verbose ) message("Fetching: '", paste0(url, collapse = '/'), "'")
    gh_api_call(url, ...)
    
}


gh_rate_limit <- function(){
    gh_api_call(gh_api.path('rate_limit'), nice = FALSE)
}

gh_user_repo <- function(user, ...){
    res <- gh_GET(c('users', user, 'repos'), ...)
    names(res) <- sapply(res, '[[', 'name')
    res
}

gh_repo_head <- function(user, repo, ref = 'master', ...){
    gh_GET(c('repos', user, repo, 'git/refs/heads', ref), ...)
}

gh_get_content <- function(user, repo, ..., ref = 'master'){
    res <- gh_api_call(c('repos', user, repo, 'contents', ...), ref = ref)
    if( !is.null(res$message) ) return()
    names(res) <- sapply(res, '[[', 'name')
    res
}

gh_search_Rrepos <- function(...){
    res <- gh_api_fetch(c('search', 'repositories'), ..., q='language:R')
}
