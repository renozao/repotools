# Project: pkgmaker
# 
# Author: Renaud Gaujoux
# Created: Feb 13, 2014
###############################################################################

.repotools.setup.url = 'http://renozao.github.io/repotools/install.R'

.PACKAGES_fields <- c('Package', 'Version')

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
    
    # parameters
    dir <- path
    sel <- c(.PACKAGES_fields, GRAN.fields(TRUE))
    
    # load package list from contrib
    repo_dir <- normalizePath(dir)
    contrib_dir <- contrib.url(repo_dir)
    repo <- paste0('file://', repo_dir)
    contrib <- contrib.url(repo)
    contrib_path <- contrib.url('.')
    
    # change to repo base directory
    od <- setwd(repo_dir)
    on.exit( setwd(od) )
    
    smessage('Generating HTML page in ', repo_dir, appendLF = TRUE)
    if( robots.file ){
        write("User-agent: *\nDisallow: /\n\n", file = file.path(repo_dir, 'robots.txt'))
    }
    smessage('Reading PACKAGES file in ', contrib_path, ' ... ')
    p <- available.packages(contrib, fields = sel)
    message('OK [', nrow(p), ']')
    if( !is.null(pattern) ){
        smessage('Selecting packages matching pattern "', pattern, '" only ... ')
        i <- grep(pattern, p[, 'Package'])
        message('OK [', length(i), '/', nrow(p), ']')
        p <- p[i, , drop = FALSE]
    }
    df <- as.data.frame(p[, sel, drop = FALSE], stringsAsFactors = FALSE)
    colnames(df) <- ifelse(nzchar(names(sel)), names(sel), sel)
    
    # write index page
    smessage('Loading required packages ... ')
    qlibrary('ReportingTools')
    qlibrary('hwriter')
    message('OK')
    smessage('Generating ', output, ' ... ')
    index <- HTMLReport(shortName = tools::file_path_sans_ext(output), title = title)
    
    # link to source package
    linkPackage <- function(df, ...){
	    pkg_src <- file.path(sub(file.path(repo, ''), '', contrib, fixed = TRUE), as.character(df$Package))
        pkg_file <- sprintf("%s_%s.tar.gz", pkg_src, df$Version)
        if( length(i <- which(!file.exists(pkg_file))) ){
            gh <- df[i, ]
            pkg_file[i] <- gh_repo_path(gh$User, gh$Repo, gh$Branch)
        }
	    df$Package <- hwrite(as.character(df$Package), link = pkg_file, table=FALSE)
	    df
    }
    # maintainer email
    emailMaintainer <- function(df, ...){
	    if( !is.null(df$Maintainer) ){
		    df$Maintainer <- gsub(".*<([^>]+)> *$", "\\1", df$Maintainer)
	    }
	    df
    }
    
    # publish
    publish(knit2html(quiet = TRUE, text = sprintf("Install packages from this repository as follows (in an R console):
                            
```{r, eval = FALSE}
# install repotools (only once)
source('%s')
                            
# install package
library(repotools)
install.pkgs('<pkgname>', devel = TRUE)
```", .repotools.setup.url), fragment.only = TRUE), index)
    publish(df, index, name=title, .modifyDF = list(emailMaintainer, linkPackage))
    finish(index)
    message('OK')
    invisible(normalizePath(output))
}

