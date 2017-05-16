# Functions injected into devtools namespace
# 
# Author: Renaud Gaujoux
###############################################################################

.shim_registry <- new.env()

set_shim <- function(name, FUN, envir = NULL, quiet = FALSE){
  
  # process envir
  spec <- parse_function_name(name, envir = envir)
  ename <- spec$ename
  name <- spec$name
  key <- spec$key
  envir <- spec$envir
  
  # backup original definition if necessary
  if( is.null(.shim_registry[[key]]) ){
    .shim_registry[[key]] <- envir[[name]]
  }
  
  # force function's environment
  fpkg <- packageName(topenv(environment(FUN)))
  eFUN <- FUN
  environment(eFUN) <- environment(envir[[name]])
  
  # inject if necessary
  if( digest(eFUN) != digest(envir[[name]]) ){
    # check if environment is locked
    was_locked <- bindingIsLocked(name, envir)
    if( was_locked ) do.call("unlockBinding", list(name, envir))
    
    # override function if necessary
    if( !quiet ){
      msg <- sprintf("Patching %s::%s with definition in %s [%s <- %s]"
          , ename, name, fpkg, sha1(.shim_registry[[key]] %||% envir[[name]]), sha1(eFUN))
      message(msg)
    }
    
    assign(name, eFUN, envir = envir)
    
#    if( isS3method(name, envir = envir) ) namespaceExport(envir, name)
    
    # lock it again if necessary
    if( was_locked ) lockBinding(name, envir)
    return(invisible(TRUE))
    
  }
  
  # return FALSE if nothing was injected 
  invisible(FALSE)
  
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

# returns the original function that has been patched
get_shim_parent <- function(name, ...){
  # extract key from name
  spec <- parse_function_name(name, ...)
  # return stored original function
  .shim_registry[[spec$key]]
  
}

set_shims <- function(ns = topenv(parent.frame()), quiet = NULL){
  
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
      msg <- sprintf("Applying %s patches to %s", pkg, paste0(sprintf("%s[%i]", names(t), t), collapse = ", "))
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

with_shim <- function(name, FUN, expr, envir = NULL){
  
  # restore shim on exit
  on.exit(reset_shim(name, envir))
  if( !set_shim(name, FUN, envir) ) on.exit()
  e <- parent.frame()
  eval(expr, envir = e)
  
}
