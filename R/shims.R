# Functions injected into devtools namespace
# 
# Author: Renaud Gaujoux
###############################################################################

.shim_registry <- new.env()

set_shim <- function(envir, FUN, name = NULL){
  
  # process envir
  if( isString(envir) ){
    envir <- strsplit(envir, '::')[[1L]]
    name <- name %||% envir[2L]
    envir <- asNamespace(envir[1L])
  }
  if( is.null(name) || !nzchar(name) ) stop("Invalid target function name (empty)")
  
  ename <- packageName(envir)
  # force function's environment
  fpkg <- packageName(topenv(environment(FUN)))
  key <- paste0(ename, '::', name)
  
  # backup original definition if necessary
  if( is.null(.shim_registry[[key]]) ){
    .shim_registry[[key]] <- envir[[name]]
  }
  
  eFUN <- FUN
  environment(eFUN) <- environment(envir[[name]])
  
  # inject if necessary
  if( digest(eFUN) != digest(envir[[name]]) ){
    # check if environment is locked
    was_locked <- bindingIsLocked(name, envir)
    if( was_locked ) do.call("unlockBinding", list(name, envir))
    
    # override function if necessary
    msg <- sprintf("Patching %s::%s with definition in %s [%s <- %s]"
        , ename, name, fpkg, sha1(.shim_registry[[key]] %||% envir[[name]]), sha1(eFUN))
    message(msg)
    
    assign(name, eFUN, envir = envir)
    
#    if( isS3method(name, envir = envir) ) namespaceExport(envir, name)
    
    # lock it again if necessary
    if( was_locked ) lockBinding(name, envir)
    return(invisible(TRUE))
    
  }
  
  # return FALSE if nothing was injected 
  invisible(FALSE)
  
}

set_shims <- function(ns = topenv(parent.frame())){
  # inject shims into their respective namespace
  shims <- ls(ns, pattern = "^shim_")
  shims <- sapply(shims, get, envir = ns, simplify = FALSE)
  shims <- shims[sapply(shims, is.function)]
  names(shims) <- gsub("^shim_", '', names(shims))
  names(shims) <- sub("_", '::', names(shims))
  sapply(names(shims), function(x){
        f <- shims[[x]]
        set_shim(x, f)
      })
  
}

reset_shim <- function(envir, name = NULL){
  
  # process envir
  if( isString(envir) ){
    envir <- strsplit(envir, '::')[[1L]]
    name <- name %||% envir[2L]
    envir <- asNamespace(envir[1L])
  }
  if( is.null(name) || !nzchar(name) ) stop("Invalid target function name (empty)")
  
  ename <- packageName(envir)
  was_locked <- bindingIsLocked(name, envir)
  if( was_locked ) do.call("unlockBinding", list(name, envir))
  key <- paste0(ename, '::', name)
  message(sprintf("Restoring definition for function %s::%s [%s <- %s]"
          , ename, name, digest(envir[[name]]), digest(.shim_registry[[key]])))
  envir[[name]] <- .shim_registry[[key]]
  if( was_locked ) lockBinding(name, envir)
  
}

with_shim <- function(envir, FUN, expr, name = NULL){
  
  # restore shim on exit
  on.exit(reset_shim(envir, name))
  if( !set_shim(envir, name) ) on.exit()
  
}

# make remotes use authentication from netrc file
shim_devtools_remote <- function(type, ...) {
  res <- structure(list(...), class = c(paste0(type, "_remote"), "remote"))
  # resolve remote authentication
  res <- repotools::remote_auth(res)
  # return remote object
  res
  
}

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

