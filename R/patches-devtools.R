# Patches for devtools
# 
# Author: Renaud Gaujoux
###############################################################################

#' @include patches.R remote-auth.R
NULL

# patch for internal function remotes:::remote so that remote objects are built
# including a relevant authentication token loaded from the .netrc file.
shim_remotes_remote <- function(type, ...) {
  # get original function
  f <- repotools::get_shim_parent('remotes::remote')
  res <- f(type, ...)
  # resolve remote authentication
  res <- repotools::remote_auth(res)
  # return remote object
  res
  
}

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
