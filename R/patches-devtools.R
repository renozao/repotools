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
get_remote_package_description <- function(remote, url, path, user = remote$auth_token, password = NULL, ...) {
  
  loadNamespace('httr')
  
  tmp <- tempfile()
  if (!is.null(user)) {
    auth <- httr::authenticate(
        user = user,
        password = password %||% '',
        type = "basic"
    )
  } else {
    auth <- NULL
  }  
  
  req <- httr::GET(url, path = path, httr::write_disk(path = tmp), auth, ...)
  if (httr::status_code(req) >= 400) {
    warning(sprintf("Could not access remote DESCRIPTION file at '%s/%s' [Code: %s]", url, dirname(path), httr::status_code(req)))
    return(NA_character_)
  }
  
  read_dcf(tmp)$Package
}
environment(get_remote_package_description) <- asNamespace('devtools')

# Mask devtools method with bug-fixed version for remote package name query 
# to ensure that queries to private repository use authentication token
#' @noRd
#' @export
remote_package_name.github_remote <- function(remote, url = "https://raw.githubusercontent.com", ...) {
  
  path <- paste(c(
          remote$username,
          remote$repo,
          remote$ref,
          remote$subdir,
          "DESCRIPTION"), collapse = "/")
  
  repotools::get_remote_package_description(remote, url, path, user = remote$auth_token, password = "x-oauth-basic", ...)
}
environment(remote_package_name.github_remote) <- asNamespace('devtools')
shim_devtools_remote_package_name.github_remote <- remote_package_name.github_remote

# https://api.bitbucket.org/1.0/repositories/{accountname}/{repo_slug}/raw/{revision}/{path}
#' @noRd
#' @export
remote_package_name.bitbucket_remote <- function(remote, ...) {
  
  path <- paste(c(
          "1.0/repositories",
          remote$username,
          tolower(remote$repo),
          'raw',
          remote$ref,
          remote$subdir,
          "DESCRIPTION"), collapse = "/")
  
  repotools::get_remote_package_description(remote, "https://api.bitbucket.org", path
      , user = remote$auth_user, password = remote$password, ...)
}
environment(remote_package_name.bitbucket_remote) <- asNamespace('devtools')
shim_devtools_remote_package_name.bitbucket_remote <- remote_package_name.bitbucket_remote

