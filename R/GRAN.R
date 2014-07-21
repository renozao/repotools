# Project: repotools
# 
# Author: Renaud Gaujoux
# Created: May 30, 2014
###############################################################################

api.path <- function(..., type = c('raw', 'api')){
    type = match.arg(type)
    url <- file.path(sprintf('https://%s.github.com', type), ...)
    #message(url)
    url
}

gh_repo_path <- function(user, repo, branch = 'master'){
    sprintf("http://github.com/%s", file.path(user, repo, branch))
}

GRAN.repos <- function(...){
    file.path('http://tx.technion.ac.il/~renaud/GRAN', ...)
}
GRAN.fields <- function(named = FALSE){
    f <- c(Repo = 'GithubRepo', User = "GithubUsername", Branch = "GithubRef", Forked = 'GithubFork')
    if( !named ) f <- unname(f)
    f
}

GRAN.available <- function(type = getOption('pkgType'), fields = GRAN.fields(), ..., version = NULL){
    if( is.null(version) ){
        p <- available.pkgs(contrib.url2(GRAN.repos(), type = type), fields = fields, ...)
    }else{
        p <- available.pkgs(contrib.url(GRAN.repos(), type = type), fields = fields, ..., filters = c("R_version", "OS_type", "subarch"))
        
        invert <- match.fun(ifelse(grepl("^!", version), "!", 'identity'))
        version <- gsub("^!", "", version)   
        sel <- invert(p[, 'GithubRef'] %in% version)
        p <- p[sel, , drop = FALSE]
    }
    # fix for migration of field names
    if( length(no_repo <- is.na(p[, 'GithubRepo'])) ){
        p[no_repo, 'GithubRepo'] <- p[no_repo, 'Package']  
    }
    
    p
}

# deprecated setup (prior-webhook)
#' @importFrom RCurl getURL
.GRAN.setup <- function(path = 'GRAN', repos = NULL, cleanup = FALSE, fields = GRAN.fields()){
    
    # create repo src/contrib directory
    repo_dir <- contrib.url(path, type = 'source')
    # cleanup on package directories exit
    if( cleanup ){
        on.exit( .cleanup(repo_dir) )
        .cleanup <- function(dir){
            unlink(list.dirs(dir, full.names = TRUE, recursive = FALSE), recursive = TRUE)
        }
    }
    if( !is.dir(repo_dir) ) dir.create(repo_dir, recursive = TRUE)
    
    
    
    if( !is.null(repos) ){
        repos <- cbind('renozao', c('RcppOctave', 'dbutils', 'repotools', 'CLIR', 'pkgmaker'))
        colnames(repos) <- c('user', 'project')
        
        .fields <- .repo_fields(fields)
        # fetch DESCRIPTION for each package
        raw_desc <- gh_read.dcf(repos, fields = .fields, raw = TRUE)
        
        
        sapply(raw_desc, function(desc){
                    # skip missin packages
                    if( is.na(desc) ) return()
                    
                    # read DESCRIPTION
                    con <- textConnection(desc)
                    on.exit( close(con) )
                    res <- setNames(read.dcf(con)[1L, ][.fields], .fields)
                    
                    # write DESCRIPTION
                    if( !is.dir(pdir <- file.path(repo_dir, res['Package'])) ) dir.create(pdir)
                    cat(desc, file = file.path(pdir, 'DESCRIPTION'))
                })
    }
    
    # write PACKAGES files
    create_repo(path, verbose = TRUE)
    write_PACKAGES(repo_dir, unpacked = TRUE, fields = fields, latestOnly = FALSE)
    # return path to file 
    invisible(file.path(repo_dir, 'PACKAGES'))
}

gh_getRaw <- function(user, repo, ..., ref = 'master'){
    url <- api.path(user, repo, ref, ..., type = 'raw')
    getURL(url, .opts = list(followlocation = TRUE))
}

#gh_getLastCommit <- function(){
#    
#}

.repo_fields <- function(fields){
    unique(c(ns_get('.get_standard_repository_db_fields', 'tools')('source'), fields))
}

#' @import stringr
gh_read.dcf <- function(repos, user = NULL, fields = NULL, raw = FALSE){
    
    if( is.vector(repos) ){
        repos <- cbind(dirname(repos), basename(repos))
    }
    if( !is.null(user) ) repos[, 1L] <- user
    
    fields <- .repo_fields(fields)
    .fetch_desc <- function(x){
        
        message("* Fetching package ", x[1], ":", x[2], " ... ", appendLF = FALSE)
        desc <- gh_getRaw(x[1], x[2], 'DESCRIPTION')
        # not found? => return dummy vector
        if( grepl("^not found", desc, ignore.case = TRUE) ){
            message("[ERROR: not found]")
            if( raw ) return(as.character(NA))
            return( setNames(rep(as.character(NA), length(fields)), fields) )
        }
        
        # read DESCRIPTION
        con <- textConnection(desc)
        on.exit( close(con) )
        res <- setNames(read.dcf(con)[1L, ][fields], fields)
        
        message("[OK - ", res['Version'], "]")
        if( raw ) desc else res
    }
    
    res <- apply(repos, 1L, .fetch_desc)
    
    if( raw ) res <- setNames(res, repos[, 2L])
    else{
        res <- t(res)
        rownames(res) <- repos[, 2L]
    }
    res
}

GRAN.update <- function(src, outdir = dirname(normalizePath(src)), clean = FALSE, fields = GRAN.fields(), actions = c('PACKAGES', 'index'), verbose = TRUE){
    
    # dump messages if non-verbose
    if( !verbose ) message <- function(...) NULL
    
    # match type of action to perform
    actions <- match.arg(actions, several.ok = TRUE)
    
    # initialise complete repository structure if necessary
    if( !is.dir(outdir) || clean ) create_repo(outdir, pkgs = NA)

    # update PACKAGES files
    if( 'PACKAGES' %in% actions ){
        # update from built packages
        create_repo(outdir, verbose = TRUE)
        
        ## add Github packages to source PACKAGES
        # write PACKAGES file from Github directories 
        write_PACKAGES(src, type = 'source', unpacked = TRUE, fields = fields, latestOnly = FALSE)
        P <- available.packages(file.path('file:/', normalizePath(src)), fields = fields, filters = c("R_version", "OS_type", "subarch"))
        # merge with PACKAGES in src/contrib
        src_contrib <- contrib.url(outdir, type = 'source');
        pfile <- file.path(src_contrib, 'PACKAGES')
        write.dcf(P, file = pfile, append = TRUE)
        # create .gz version
        gzfile <- gzfile(paste0(pfile, '.gz'))
        write.dcf(P, file = gzfile)
        close(gzfile)
    }
    
    # generate index file
    if( 'index' %in% actions ){
        message("* Generating index file ... ")
        write_PACKAGES_index(outdir, title = 'GRAN: Github R Archive Network')
        message("OK")
    }
    
    # return output directory to calling script in non-interactive mode
    if( !interactive() ) cat(outdir)
    invisible(outdir)
}
