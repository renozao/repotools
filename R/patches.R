# Functions injected into devtools namespace
# 
# Author: Renaud Gaujoux
###############################################################################

.shim_registry <- new.env()

digest_function <- function(fun, n = 10L){
  bd <- if( !is.primitive(fun) ) body(fun) else capture.output(fun)
  attributes(bd) <- NULL
  bd <- as.character(bd)
  if( !is.primitive(fun) ) bd <- list(bd, formals(fun))
  substr(digest::digest(bd), 1L, n)
  
}

set_shim <- function(name, FUN, envir = NULL, quiet = FALSE){
  
  # process envir
  spec <- parse_function_name(name, envir = envir)
  ename <- spec$ename
  name <- spec$name
  key <- spec$key
  envir <- spec$envir
  
  # force function's environment
  tenv <- topenv(environment(FUN))
  fpkg <- packageName(tenv) %||% environmentName(tenv) %||% format(tenv)
  eFUN <- FUN
  environment(eFUN) <- environment(envir[[name]])
    
  # inject only if necessary
  old <- get_shim_parent(key)
  .shim_registry[[key]] <- eFUN
  sha_new <- digest_function(eFUN)
  sha_env <- digest_function(envir[[name]])
  if( is.null(old) && !identical(eFUN, envir[[name]]) && sha_new != sha_env ){
    
    # override function if necessary
    if( !quiet ){
      msg <- sprintf("Patching %s with definition in %s [%s <- %s]", key, fpkg, sha_env, sha_new)
      message(msg)
    }
    
    # backup original definition as an attribute of the injected function
    old <- envir[[name]]
    attr(eFUN, 'original') <- envir[[name]]
    
    # check if environment is locked
    was_locked <- bindingIsLocked(name, envir)
    if( was_locked ) do.call("unlockBinding", list(name, envir))
        
    assign(name, eFUN, envir = envir)
    
#    if( isS3method(name, envir = envir) ) namespaceExport(envir, name)
    
    # lock it again if necessary
    if( was_locked ) lockBinding(name, envir)
    return(invisible(old))
    
  }
  
  # return NULL if nothing was injected 
  invisible(NULL)
  
}


parse_function_name <- function(name, envir = NULL){
  
  # check for format ns::name
  parts <- strsplit(name, '::')[[1L]]
  if( length(parts) > 1L ){
    envir <- envir %||% parts[[1L]]
    name <- parts[2L]
  }
  envir <- asNamespace(envir)
  ename <- packageName(envir)
  if( is.null(name) || !nzchar(name) ) stop("Invalid target function name (empty)")
  
  # return environment, name and key (explicit name)
  list(envir = envir
      , ename = ename
      , name = name
      , key = paste0(ename, '::', name)
  )
  
}

#' Retrieving Original Patched Function
#' 
#' Returns the original function that has been patched.
#' 
#' @param name name of the patched function.
#' Can be specified as `package_name::funname`, in which case 
#' argument `envir` default value becomes "package_name".
#' @param envir environment where the function should be looked for.
#' 
#' @export
get_shim_parent <- function(name, envir = NULL){
  # extract key from name
  spec <- parse_function_name(name, envir = envir)
  # return stored original function
#  .shim_registry[[spec$key]]
  attr(spec$envir[[spec$name]], 'original')
  
}

set_shims <- function(ns = topenv(parent.frame()), quiet = getOption('repotools.shim_quiet')){
  
  # inject shims into their respective namespace
  shims <- ls(ns, pattern = "^shim_")
  shims <- sapply(shims, get, envir = ns, simplify = FALSE)
  shims <- shims[sapply(shims, is.function)]
  names(shims) <- gsub("^shim_", '', names(shims))
  names(shims) <- sub("_", '::', names(shims))
  
  # log patches
  if( is.null(quiet) ){
    pkg <- packageName(ns)
    quiet <- !isDevNamespace(pkg)
    if( quiet ){
      t <- lengths(split(shims, sub("([^:]+).*", "\\1", names(shims))))
      msg <- sprintf("Applying %s patches to %s", pkg, paste0(sprintf("%s [%i]", names(t), t), collapse = ", "))
      packageStartupMessage(msg)
    }
  }
  
  sapply(names(shims), function(x){
        f <- shims[[x]]
        set_shim(x, f, quiet = quiet)
      })
  
}

reset_shim <- function(name, envir = NULL){
  
  # process envir
  spec <- parse_function_name(name)
  name <- spec$name
  envir <- spec$envir  
  ename <- spec$ename
  key <- spec$key
  
  old <- get_shim_parent(key)
  if( is.null(old) ){
    message("No shim defined for ", key)
    return()
  }
  was_locked <- bindingIsLocked(name, envir)
  if( was_locked ) do.call("unlockBinding", list(name, envir))
  message(sprintf("Restoring definition for function %s [%s <- %s]", key, digest_function(envir[[name]]), digest_function(old)))
  envir[[name]] <- old
  if( was_locked ) lockBinding(name, envir)
  
}

with_shim <- function(name, FUN, expr, envir = NULL){
  
  # restore shim on exit
  on.exit(reset_shim(name, envir))
  if( is.null(set_shim(name, FUN, envir)) ) on.exit()
  e <- parent.frame()
  eval(expr, envir = e)
  
}
