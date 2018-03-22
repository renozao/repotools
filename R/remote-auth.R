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

#' Parse Remote Specification
#' 
#' These functions are used to parse the specification of a remote repository given as a single string
#' such as `"username/repo_name@@branch"` or `"bitbucket::username/repo_name@@commit_hash"`, 
#' and build a `remote` object that is used to query the repository's API.
#' 
#' @param x remote specification.
#' `as_remote` also supports an already built devtools `remote` object.
#' @return a list containing remote split characteristics such as main url, username, repo, 
#' raw url, etc...
#' 
#' @source devtools:::parse_one_remote
#' @rdname remotes
#' 
NULL

#' @describeIn remotes Builds an augmented `remote` object, which includes extra slots, 
#' such as url base and function to query and fetch raw contents.
#' 
#' @export
#' @examples
#' # build augmented remote object
#' as_remote('a/b@c')
as_remote <- function(x){
  
  if( !is(x, 'remote') ){
    # parse the remote characteristic
    x <- parse_remote_specification(x)
  }
  # augment with extra slots
  remote_augment(x)
  
}

#' @describeIn remotes build a devtools `remote` object, augmented with extra slots.
#' @export
#' @examples
#' # build devtools remote object
#' parse_remote_specification('a/b@c')
#' 
parse_remote_specification <- function(x) 
{
  pieces <- strsplit(x, "::", fixed = TRUE)[[1]]
  if (length(pieces) == 1) {
    type <- "github"
    repo <- pieces
  }
  else if (length(pieces) == 2) {
    type <- pieces[1]
    repo <- pieces[2]
  }
  else {
    stop("Malformed remote specification '", x, "'", call. = FALSE)
  }
  fun <- tryCatch(get(paste0(tolower(type), "_remote"), envir = asNamespace("devtools"), 
          mode = "function", inherits = FALSE), error = function(e) stop("Unknown remote type: ", 
            type, call. = FALSE))
  fun(repo)
}

# augments particular remotes with extra slots
remote_augment <- function(x){
  UseMethod('remote_augment')
}

remote_augment.github_remote <- function(x){
  
  # url to access raw content
  x[['host_raw']] <- "https://raw.githubusercontent.com"
  x[['build_raw_path']] <- function(remote, ...){
    paste(c(remote$username,
            remote$repo,
            remote$ref,
            remote$subdir,
            ...), collapse = "/")
  }
  
  x
  
}

remote_augment.bitbucket_remote <- function(x){
  
  x[['host_raw']] <- "https://api.bitbucket.org"
  x[['build_raw_path']] <- function(remote, ...){
    paste(c("1.0/repositories",
            remote$username,
            tolower(remote$repo),
            'raw',
            remote$ref,
            remote$subdir,
            ...), collapse = "/")
  }
  
  x
  
}

#' API Query to Remote Source Control Repositories
#' 
#' @param remote a repository specification or `remote` object.
#' @param path path to the file to downlad, relatively to the root of the repository.
#' @param url base API url to use.
#' @param user,password repository authetication credentials. The default is to use the user define in the remote
#' specification `'auth_user'` and `'auth_token'` respectively, which can be determined transparently 
#' with [remote_auth], via the credentials stored in a `.netrc` file.
#' @param ... other arguments passed to [httr::GET]
#' @param error logical that indicates if an error should be thrown if the request is unsuccessful
#' 
#' @return the request result object returned by [httr::GET] 
#' 
#' @export
#' 
remote_get_api <- function(remote, url, path, user = remote$auth_user, password = remote$auth_token, ..., config = NULL, error = FALSE) {
  
  remote <- as_remote(remote)
  
  # define authentication object
  if (!is.null(user)) {
    auth <- httr::authenticate(
        user = user,
        password = password %||% '',
        type = "basic"
    )
  } else {
    auth <- NULL
  }  
    
    req <- httr::GET(url, config = c(auth, config), path = path)
  # throw error if GET is not not successful
  if (httr::status_code(req) >= 400) {
    msg <- switch(as.character(httr::status_code(req)), 
        '404' = 'Could not access remote file',
        '400' = 'Could not find remote file',
        'Could not fetch remote file'
    )
    
    msg <- sprintf("%s '%s' at '%s/%s' [Code: %s]", msg, basename(path), url, dirname(path), httr::status_code(req))
    if( error ) stop(msg)
    else warning(msg)
    return(NA_character_)  
    
  }
  
  # return request result
  req
  
}

#' Fetching Raw Contents form Remote Source Control Repositories
#' 
#' @param remote a repository specification or `remote` object.
#' @param path path to the file to downlad, relatively to the root of the repository.
#' @param url base url to use. When `NULL` (default), the base url is taken from the remote
#' specification `'host_raw'`.
#' @param user,password repository authetication credentials. The default is to use the user define in the remote
#' specification `'auth_user'` and `'auth_token'` respectively, which can be determined transparently 
#' with [remote_auth], via the credentials stored in a `.netrc` file.
#' @param ... other arguments passed to [httr::GET]
#' @param destfile file where to save the fetched content.
#' If `NULL` (default), then the file is saved in the temporary directory, with a unique prefix of the form
#' `<prefix>__<basename(path)>`.
#' 
#' @return the full normalized path to the file where the raw content was saved.
#' 
#' @importFrom httr authenticate GET write_disk status_code
#' @export
remote_get_raw <- function(remote, path, url = NULL, ..., destfile = NULL) {
  
  remote <- as_remote(remote)
  
  # setup destination file in the temporary directory
  if( is.null(destfile) ){
    destfile <- tempfile('', fileext = paste0('__', basename(path)))
    # delete temporary file on.exit (in case there is an error before returning)
    on.exit( unlink(destfile) )
  }
  
  # build base URL and path
  url <- url %||% remote[['host_raw']]
  if( is.function(remote$build_raw_path) ) path <- remote$build_raw_path(remote, path)
  
  req <- remote_get_api(remote, path = path, url = url, ..., config = httr::write_disk(path = destfile))
  if( is_NA(req) ) return(req)
  
  # load or return path
  on.exit() # cancel deletion of destination file
  normalizePath(destfile)
  
}

# May only work for Github Apps
#remote_get_tree <- function(remote, path, ...){
#  
#  remote <- as_remote(remote)
#  path <- paste(c('repos', remote$username,
#                  remote$repo,
#                  'git/trees',
#                  paste0(paste(c(remote$ref, remote$subdir), collapse = "/"), ':', URLencode(path, reserved = TRUE))
#                  )
#          , collapse = "/")
#  print(path)
#  remote_get_api(remote, url = remote$host, path = path, ..., config = httr::add_headers(Accept = 'application/vnd.github.machine-man-preview+json'))
#  #"/repos/<owner>/<repo>/git/trees/url_encode(<branch_name>:<parent_path>)"
#  
#  
#  
#}
