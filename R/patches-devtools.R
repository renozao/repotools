# Patches for devtools
# 
# Author: Renaud Gaujoux
###############################################################################

#' @include patches.R remote-auth.R
NULL

# make remotes use authentication from netrc file
shim_devtools_remote <- function(type, ...) {
  res <- structure(list(...), class = c(paste0(type, "_remote"), "remote"))
  # resolve remote authentication
  res <- repotools::remote_auth(res)
  # return remote object
  res
  
}

# To fix issue hadley/devtools#1370: change default value for devtools:::install_packages
shim_devtools_install_packages <- local({
  f <- devtools:::install_packages
  formals(f)[['dependencies']] <- NA
  f
})

# #' Install Packages from Github
# #' 
# #' This function is used to mask the original function [devtools::install_github],
# #' to provide the same functionnality but using authentication tokens stored in 
# #' the user's `.netrc` file.
# #' 
# #' @inheritParams devtools::install_github
# #' 
# #' @export
#install_github <- local({
#  f <- devtools::install_github
#  body(f) <- substitute({ ca <- match.call(); ca[[1L]] <- devtools::install_github; pe <- parent.frame(); eval(ca, envir = pe) })
#  f
#})

#' Fetch DESCRIPTION File from Remote
#' 
#' @param remote remote S3 object as returned by `devtools:::remote`
#' @param url base repo host url
#' @param path path to DESCRIPTION file
#' @param user username to use for authentication
#' @param password authentication password or token
#' @param ... arguments passed to [httr::GET]
#' 
#' @export
get_remote_package_description <- function(remote, url, path, user = remote$auth_token, password = NULL, ..., file = NULL) {
  
  loadNamespace('httr')
  
  desc_file <- file
  if( is.null(desc_file) ){
    desc_file <- tempfile()
    on.exit( unlink(desc_file) )
  }
  
  if (!is.null(user)) {
    auth <- httr::authenticate(
        user = user,
        password = password %||% '',
        type = "basic"
    )
  } else {
    auth <- NULL
  }  
  
  req <- httr::GET(url, path = path, httr::write_disk(path = desc_file), auth, ...)
  if (httr::status_code(req) >= 400) {
    warning(sprintf("Could not access remote DESCRIPTION file at '%s/%s' [Code: %s]", url, dirname(path), httr::status_code(req)))
    return(NA_character_)
  }
  
  read_dcf <- ns_get('read_dcf', 'devtools')
  read_dcf(desc_file)
}

#' @noRd
#' @export
remote_package_description <- function(remote, ...){
  UseMethod('remote_package_description')
}

#' @noRd
#' @export
remote_package_description.github_remote <- function(remote, url = "https://raw.githubusercontent.com", ...){
  
  path <- paste(c(
          remote$username,
          remote$repo,
          remote$ref,
          remote$subdir,
          "DESCRIPTION"), collapse = "/")
  
  repotools::get_remote_package_description(remote, url, path, user = remote$auth_token, password = "x-oauth-basic", ...)
}

# Mask devtools method with bug-fixed version for remote package name query 
# to ensure that queries to private repository use authentication token
#' @noRd
#' @export
remote_package_name.github_remote <- function(remote, url = "https://raw.githubusercontent.com", ...) {
  
  desc <- repotools::remote_package_description(remote, url = url, ...)
  desc$Package
}
environment(remote_package_name.github_remote) <- asNamespace('devtools')
shim_devtools_remote_package_name.github_remote <- remote_package_name.github_remote

#' @noRd
#' @export
remote_package_description.bitbucket_remote <- function(remote, url = "https://api.bitbucket.org", ...) {
  
  path <- paste(c(
          "1.0/repositories",
          remote$username,
          tolower(remote$repo),
          'raw',
          remote$ref,
          remote$subdir,
          "DESCRIPTION"), collapse = "/")
  repotools::get_remote_package_description(remote, url = url, path = path, user = remote$auth_user, password = remote$password, ...)
}

# https://api.bitbucket.org/1.0/repositories/{accountname}/{repo_slug}/raw/{revision}/{path}
#' @noRd
#' @export
remote_package_name.bitbucket_remote <- function(remote, url = "https://api.bitbucket.org", ...) {
  
  desc <- repotools::remote_package_description(remote, url = url, ...)
  desc$Package
}
environment(remote_package_name.bitbucket_remote) <- asNamespace('devtools')
shim_devtools_remote_package_name.bitbucket_remote <- remote_package_name.bitbucket_remote

#' @noRd
#' @export
remote_sha.github_remote <- function(remote, url = "https://github.com", ...) {
  
  f <- repotools::get_shim_parent('devtools::remote_sha.github_remote')
  # try with credentials if any
  cred <- if( !is.null(remote$auth_token) ){
    usr <- remote$auth_user
    if( is.null(usr) ) usr <- ""
    git2r::cred_user_pass(usr, remote$auth_token)
    
  }
  sha <- f(remote, url = url, credentials = cred, ...)
  # if this did not work: try without credentials
  if( all(is.na(sha)) && !is.null(cred) ){
    sha <- f(remote, url = url, credentials = NULL, ...)
  }
  sha
  
}
environment(remote_sha.github_remote) <- asNamespace('devtools')
shim_devtools_remote_sha.github_remote <- remote_sha.github_remote
