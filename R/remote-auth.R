# Functions to handle authentication for remotes
# 
# Author: Renaud Gaujoux
###############################################################################

#' @include url.R
NULL

.must_message_auth <- local({
  .call <- NULL
  .logged <- c()
  function(key, frame){
    sf <- format(frame)
    .call <<- .call %||% sf
    # reset if new call
    if( .call != sf ){
      .logged <<- c()
      .call <<- sf
    }
    if( key %in% .logged ) FALSE
    else{
      .logged <<- c(.logged, key)
      TRUE
    }

  }
})

remote_url_auth <- function(urls, quiet = NULL, ...){
  
  for(u in urls){
    res <- url_auth(u, default = NULL, quiet = quiet %||% TRUE, full = TRUE, ...)[[1L]]
    if( !is.null(res) ){
      
      # show message for hits
      if( is.null(quiet) ){
        sf <- sys.frame(1)
        if( .must_message_auth(digest(res), sf) )
          url_auth(u, default = NULL, quiet = FALSE, full = TRUE, ...)[[1L]]
      }
      #
      return(res)
    }
  }
  NULL
  
}

#' Resolve Authentication Credentials from Remotes
#' 
#' Looks for suitable authentication tokens in environment or .netrc file for 
#' some type of remotes.
#' 
#' @param x a `remote` S3 object as built by `devtools:::remote`.
#' @param ... extra arguments passed to [url_auth]
#' 
#' @return the remote object with updated credentials.
#' 
#' @export 
remote_auth <- function(x, ...){
  UseMethod('remote_auth')
}

#' @export 
remote_auth.default <- function(x, ..., host = NULL, auth_user = 'auth_user', auth_token = 'auth_token'){
  
  if( is.null(host) ) x
  else{
    base_url <- paste(host, x$username, sep = '/')
    auth <- remote_url_auth(c(paste(base_url, x$repo, sep = '/'), base_url), ...)
    if( !is.null(auth) ){
      x[['auth_user']] <- auth[['login']]
      x[[auth_user]] <- x[['auth_user']]
      # add field name from github to make other devtools auth calls work (e.g., remote_package_name)
      x[['auth_token']] <- auth[['password']]
      x[[auth_token]] <- x[['auth_token']]
    }
    x
  }
  
}

#' @export
remote_auth.github_remote <- function(x, ...){
  remote_auth.default(x, host = 'github.com', ...)
  
}

#' @export
remote_auth.bitbucket_remote <- function(x, ...){
  remote_auth.default(x, host = 'bitbucket.org', auth_token = 'password', ...)
  
}
