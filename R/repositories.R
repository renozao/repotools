# Project: pkgmaker
#
# Author: Renaud Gaujoux
# Created: Feb 13, 2014
###############################################################################

.repotools.setup.url = 'http://renozao.github.io/repotools/install.R'

.PACKAGES_fields <- c('Package', 'Version', 'Date')
.PACKAGES_filters_keep_all_versions <- c("R_version", "OS_type", "subarch")

#' @importFrom tools write_PACKAGES
create_repo <- function(dir = '.', type = NULL, pkgs = NULL, all = TRUE, ..., clean = FALSE, verbose = FALSE){

    # dump messages if non-verbose
    if( !verbose ) message <- function(...) NULL

    # clean root directory if requested
    if( clean && is.dir(dir) ) unlink(dir, recursive = TRUE)

    # create root directory if needed
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    repo_dir <- normalizePath(dir)
    repo_url <- paste0('file://', if( OS_type() == 'windows' ) "/" , repo_dir)

    message('* Building repository in ', repo_dir)

    contrib_url_types <- type %||% names(.contrib_url_types)

    # create directory structure
    contribs <- sapply(contrib_url_types, contrib.url, repos = repo_dir)
    sapply(contribs, dir.create, recursive = TRUE, showWarnings = FALSE)

    # early exit if required
    if( is_NA(pkgs) ) return( invisible(repo_url) )

    # fill repo with files
    if( !is.null(pkgs) && is.character(pkgs) ){
        if( verbose ) message('Copying package files into repository')
        mapply(url.copy, pkgs, contribs[package_type(pkgs)])
    }

    # change to repo base directory
    od <- setwd(repo_dir)
    on.exit( setwd(od) )

    # write PACKAGES files
    .makePACKAGES <- function(dir = '.', ...){
        od <- setwd(dir)
        on.exit( setwd(od) )

        if( verbose ) message('  - Processing ', dir, ' ... ', appendLF = FALSE)
        n <- write_PACKAGES('.', ...)
        if( verbose ) message('OK [', n, ']')
        n
    }

    message('* Generating PACKAGES files for type(s): ', str_out(contrib_url_types, Inf))
    n <- sapply(contrib_url_types, function(t, ...){
                            bdir <- contrib.url('.', type = t)
                            if( all ) bdir <- list.dirs(dirname(bdir), full.names = TRUE, recursive = FALSE)
                            sapply(bdir, .makePACKAGES, type = .contrib_url_types[t], ...)
                    }, ...)

    # return repo URL
    invisible(repo_url)
}

GRAN.fields <- function(named = FALSE, all = FALSE){
  f <- c(Repo = 'GithubRepo', User = "GithubUsername"
      , Branch = "GithubRef", Forked = 'GithubFork'
      , SHA1 = 'GithubSHA1', RepoType = 'GRANType')
  if( all ) f <- c(f, XDepends = 'XDepends', Key = 'XPath', Type = 'GRANType', Path = 'GRANPath')
  if( !named ) f <- unname(f)
  f
}

gh_repo_path <- function(user, repo, branch = 'master'){
  sprintf("https://github.com/%s", file.path(user, repo, branch))
}

#' Fetching Github Package Description
#'
#' Fetches DESCRIPTION file form a remote Github repository.
#'
#' @param ... arguments passed to `devtools:::github_remote` to build a `remote` S3 object
#' if argument `remote=NULL`.
#' @param remote remote S3 object as returned by `devtools:::github_remote`.
#' @param url base url
#' @param warn logical that indicates if a warning should be thrown if the file cannot be found.
#'
#' @return an S3 object as returned by [devtools::as.package].
#'
#' @importFrom devtools as.package
#' @export
github_remote_description <- function(..., remote = NULL, url = "https://raw.githubusercontent.com", warn = TRUE) {

  # build remote object
  remote <- remote %||% do.call(ns_get('github_remote', 'remotes'), list(...))

  # setup download destination directory
  tmpd <- tempfile()
  dir.create(tmpd)
  on.exit(unlink(tmpd, recursive = TRUE))
  tmp <- file.path(tmpd, 'DESCRIPTION')

  # fetch DESCRIPTION file
  path <- paste(c(
          remote$username,
          remote$repo,
          remote$ref,
          remote$subdir,
          "DESCRIPTION"), collapse = "/")

  if (!is.null(remote$auth_token)) {
    auth <- httr::authenticate(
        user = remote$auth_token,
        password = "x-oauth-basic",
        type = "basic"
    )
  } else {
    auth <- NULL
  }

  req <- httr::GET(url, path = path, httr::write_disk(path = tmp), auth)

  if (httr::status_code(req) >= 400) {
    if( warn ) warning(sprintf("Could not access Github repo DESCRIPTION file at '%s' [Code: %s]", dirname(path), httr::status_code(req)))
    return(NA_character_)
  }

  # return as a devtools package S3 object
  as.package(tmpd)

}


#' Generate CRAN-like Repository Index
#'
#' @param path path to the repository's root directory
#' @param output output filename -- relative to the repository root \code{path}.
#' @param pattern regular expression used to filter the names of the packages that will appear in
#' the index.
#' @param title title of the index page
#' @param robots.file logical that indicates if a file \code{robots.txt} that hides the repository from
#' search engine robots should be created.
#' @export
#' @importFrom tools write_PACKAGES
write_PACKAGES_index <- function(path = '.', output = 'index.html', pattern = NULL, title = 'Packages', robots.file = TRUE){

    if( !requireNamespace('rmarkdown') )
      stop("Could not generate package HTML index: missing required package 'rmarkdown'")

    # parameters
    dir <- path
    sel <- c(.PACKAGES_fields, GRAN.fields(TRUE))

    # load package list from contrib
    repo_dir <- normalizePath(dir)
    contrib_dir <- contrib.url(repo_dir)
    repo <- paste0('file://', repo_dir)
    contrib <- contrib.url(repo, type = 'source')
    contrib_path <- contrib.url('.')

    # change to repo base directory
    od <- setwd(repo_dir)
    on.exit( setwd(od) )

    smessage('Generating HTML page in ', repo_dir, appendLF = TRUE)
    if( robots.file ){
        write("User-agent: *\nDisallow: /\n\n", file = file.path(repo_dir, 'robots.txt'))
    }
    smessage('Reading PACKAGES file in ', contrib_path, ' ... ')
    p <- lapply(names(.contrib_url_types), function(t){
                url <- contrib.url('.', t)
                if( !file.exists(url) ) return()
                available.packages(file.path('file:/', normalizePath(url)), fields = sel, filters = .PACKAGES_filters_keep_all_versions)
            })
    p <- do.call(rbind, p)
    message(sprintf('OK [%s (%s dups)]', nrow(p), sum(duplicated(p[, 'Package']))))

    # remove bad packages
    bad <- bad_version(p[, 'Version'])
    if( any(bad) ){
        smessage(sprintf("Removing packages with invalid versions [%s]"
                            , str_out(setNames(p[bad, 'Version'], p[bad, 'Package']), use.names = TRUE, total = TRUE)
                        )
                , appendLF = TRUE)
    }
    p <- p[!bad, , drop = FALSE]

    if( !is.null(pattern) ){
        smessage('Selecting packages matching pattern "', pattern, '" only ... ')
        i <- grep(pattern, p[, 'Package'])
        message('OK [', length(i), '/', nrow(p), ']')
        p <- p[i, , drop = FALSE]
    }
    rownames(p) <- NULL
    sel <- c(sel, Downloads = 'Repository')
    df <- as.data.frame(p[, sel, drop = FALSE], stringsAsFactors = FALSE)

    # built version
    .pkg_files <- function(df){
        res <- alply(df, 1L, function(x){
            llply(.contrib_url_types, function(t){
                sprintf("%s_%s.%s", x$Package, x$Version, .contrib_ext[t])
            })
        })
        as.character(unlist(res))
    }

    # build list of built packages
    pkg_builts <- unique(list.files(dirname(gsub("^file:/", "", df[, 'Repository'])), recursive = TRUE, full.names = TRUE
                    , pattern = sprintf("(%s)$", paste0("(", .package_type.reg, ")", collapse = "|"))))
    # aggregate into single packages
    qlibrary('plyr')
    ov <- order(package_version(df[['Version']]), decreasing = TRUE)
    df <- df[ov, , drop = FALSE]
    df <- ldply(split(seq(nrow(df)), paste0(df$Package, df$GithubRef)), function(i){
                p <- df[i, , drop = FALSE]
                if( !is.na(p$GithubRef)[1] )
                    p[, 'Repository'] <- gh_repo_path(p$GithubUsername, p$GithubRepo, sprintf("archive/%s.zip", p$GithubRef))
                else{

                    p <- ddply(p, 'Version', function(v){
                        # build list of download links

                        pf <- pkg_builts[basename(pkg_builts) %in% .pkg_files(v)]
                        v[1L, 'Repository'] <- paste0(pf, collapse = " | ")
                        v[1L, ]
                    })
                    p <- p[1L, ]
                }
                p
            })
    df$.id <- NULL

    # use field human names
    colnames(df) <- ifelse(nzchar(names(sel)), names(sel), sel)

    # write index page
    smessage('Loading required packages ... ')
    message('OK')
    smessage('Generating ', output, ' ... ')

    # link to source package
    linkPackage <- function(df, ...){
	    pkg_src <- file.path(sub(file.path(repo, ''), '', contrib, fixed = TRUE), as.character(df$Package))

        dlinks <- replicate(nrow(df), character())
        gh_repo <- gh_repo_path(df$User, df$Repo, df$Branch)
        if( length(i <- which(!is.na(df$Downloads))) ){
            dlinks[i] <- lapply(df$Downloads[i], function(x){
                        strsplit(x, " | ", fixed = TRUE)[[1]]
                        })
        }

        hwrite <- function(x, link = x, ...) sprintf('<a href="%s" target="%s">%s</a>', link, x)
        hwrite_ghlink <- function(x, link = x) sprintf('<a href="%s" target="%s">%s</a>', link, "_github", x)
#	      df$Package <- hwrite(as.character(df$Package), link = NA, table=FALSE)
        i_gh <- !is.na(df$Repo)
        # SHA1
        sha1 <- hwrite_ghlink(substr(df$SHA1, 1, 7), link = gh_repo_path(df$User, df$Repo, file.path('tree', df$SHA1)), table = FALSE)
        sha1[is.na(df$SHA1)] <- NA
        df$SHA1 <- sha1
        # branch
        df$Branch <- hwrite_ghlink(df$Repo, link = gh_repo_path(df$User, df$Repo, file.path('tree', df$Branch)), table = FALSE)
        df$Branch[!i_gh] <- NA
        # repo
        df$Repo <- hwrite_ghlink(df$Repo, link = gh_repo_path(df$User, df$Repo, ''), table = FALSE)
        df$Repo[!i_gh] <- NA

        # downloads
        .make_link <- function(x){
            if( !length(x) ) return(NA)
            i_gh <- grepl("^http", x)
            l <- character()
            if( sum(i_gh) )
                l <- paste0('github: ', hwrite(sprintf('[%s]', tools::file_path_sans_ext(basename(x[i_gh]))), link = x[i_gh], table = FALSE))
            if( length(x <- x[!i_gh]) ){

                p_ext <- split(seq_along(x), .contrib_ext[package_type(x)])
                l <- lapply(names(p_ext), function(ext){
                        t <- gsub("\\..*$", '', .contrib_ext_types[ext])
                        x <- x[p_ext[[ext]]]
                        if( t == 'source' ) paste0(t, ": ", hwrite(sprintf('[%s]', ext), link = x, table = FALSE))
                        else paste0(t, ": ", paste(hwrite(sprintf('[%s]', basename(dirname(x))), link = x, table = FALSE), collapse = " "))
                })
            }
            paste0(l, collapse = " | ")

        }
        df$Downloads <- sapply(dlinks, .make_link)
	    df
    }
    # maintainer email
    emailMaintainer <- function(df, ...){
	    if( !is.null(df$Maintainer) ){
		    df$Maintainer <- gsub(".*<([^>]+)> *$", "\\1", df$Maintainer)
	    }
	    df
    }

    df <- emailMaintainer(df)
    df <- linkPackage(df)
    cat(sprintf('Install packages from this repository as follows (in an R console):
```{r, eval = FALSE}
# install repotools (only once)
source("%s")

# install package
library(repotools)
install.pkgs("<pkgname>", devel = TRUE)
```

```{r, package_table, echo = FALSE}
library(DT)
datatable(df, escape = FALSE)
```
', .repotools.setup.url), file = tmp <- tempfile(tmpdir = ".", fileext = ".Rmd"))
    on.exit( unlink(tmp), add = TRUE)
    rmarkdown::render(tmp, output_file = output)
    message('OK')
    invisible(list(path = normalizePath(output), packages = df))
}

#' Repository Indexes
#'
#' \code{contrib_cache} returns the path to the RDS file were a repository index is
#' stored when first accessed by \code{\link[utils]{available.packages}}.
#'
#' @param repos Repository URL
#' @param type Package type (see \code{\link[utils]{contrib.url}})
#' @export
contrib_cache <- function(repos, type = getOption('pkgType')){
    url <- contrib.url(repos, type)
    file.path(tempdir(), paste0("repos_", URLencode(url, TRUE), ".rds"))

}

#' \code{contrib_cache_clear} deletes the index cache file,
#' so that the repository's index is refreshed by the next call to
#' \code{\link[utils]{available.packages}}.
#'
#' @param ... parameters passed to \code{contrib_cache}.
#' @rdname contrib_cache
#' @export
contrib_cache_clear <- function(...){

    # clear all cache
    if( !nargs() ){
        part <- paste0(sprintf("(%s)", sapply(names(.contrib_path2type), URLencode, TRUE)), collapse = "|")
        f <- list.files(tempdir(), pattern = sprintf("^repos_.*(%s).*\\.rds$", part))
        unlink(file.path(tempdir(), f))
    }else unlink(contrib_cache(...))
}
