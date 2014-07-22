# Project: pkgmaker
# 
# Author: Renaud Gaujoux
# Created: Feb 13, 2014
###############################################################################

.repotools.setup.url = 'http://renozao.github.io/repotools/install.R'

.PACKAGES_fields <- c('Package', 'Version')
.PACKAGES_filters_all_versions <- c("R_version", "OS_type", "subarch")

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
    sel <- c(.PACKAGES_fields, GRAN.fields(TRUE), 'Downloads')
    
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
                available.packages(file.path('file:/', normalizePath(contrib.url('.', t))), fields = sel, filters = .PACKAGES_filters_all_versions)
            })
    p <- do.call(rbind, p)
    message(sprintf('OK [%s (%s dups)]', nrow(p), sum(duplicated(p[, 'Package']))))
    if( !is.null(pattern) ){
        smessage('Selecting packages matching pattern "', pattern, '" only ... ')
        i <- grep(pattern, p[, 'Package'])
        message('OK [', length(i), '/', nrow(p), ']')
        p <- p[i, , drop = FALSE]
    }
    rownames(p) <- NULL
    df <- as.data.frame(p[, sel, drop = FALSE], stringsAsFactors = FALSE)
    
    # built version
    .pkg_files <- function(df){
        res <- alply(df, 1L, function(x){
            llply(.contrib_url_types, function(t){
                sprintf("%s/%s_%s.%s", contrib.url('.', t), x$Package, x$Version, .contrib_ext[t])
            })
        })
        as.character(unlist(res))
    }
    
    # aggregate into single packages
    qlibrary('plyr')
    ov <- orderVersion(df[['Version']], decreasing = TRUE)
    df <- df[ov, , drop = FALSE]
    df <- ldply(split(seq(nrow(df)), paste0(df$Package, df$GithubRef)), function(i){
                p <- df[i, , drop = FALSE]
                if( !is.na(p$GithubRef)[1] )
                    p[, 'Downloads'] <- gh_repo_path(p$GithubUsername, p$GithubRepo, sprintf("archive/%s.zip", p$GithubRef))
                else{
                    
                    p <- ddply(p, 'Version', function(v){
                        # build list of download links
                        pf <- unique(Filter(file.exists, .pkg_files(v)))
                        v[1L, 'Downloads'] <- paste0(pf, collapse = " | ")
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
    qlibrary('ReportingTools')
    qlibrary('hwriter')
    message('OK')
    smessage('Generating ', output, ' ... ')
    index <- HTMLReport(shortName = tools::file_path_sans_ext(output), title = title)
    
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
        
	    df$Package <- hwrite(as.character(df$Package), link = NA, table=FALSE)
        i_gh <- !is.na(df$Repo)
        df$Repo <- hwrite(df$Repo, link = gh_repo_path(df$User, df$Repo, file.path('tree', df$Branch)), table=FALSE)
        df$Repo[!i_gh] <- NA
        
        .make_link <- function(x){
            if( !length(x) ) return(NA)
            i_gh <- grepl("^http", x)
            l <- character()
            if( sum(i_gh) )
                l <- hwrite(sprintf('[%s]', tools::file_path_sans_ext(basename(x[i_gh]))), link = x[i_gh], table = FALSE)
            if( length(x <- x[!i_gh]) )
                l <- c(l, hwrite(sprintf('[%s]', .contrib_ext[package_type(x)]), link = x, table = FALSE))
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

