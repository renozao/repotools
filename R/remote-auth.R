# Functions to handle authentication for remotes
# 
# Author: Renaud Gaujoux
###############################################################################

#' @include url.R
NULL

remote_url_auth <- function(urls, quiet = FALSE, ...){
  
  for(u in urls){
    res <- url_auth(u, default = NULL, quiet = quiet, full = TRUE, ...)[[1L]]
    if( !is.null(res) ) return(res)
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
