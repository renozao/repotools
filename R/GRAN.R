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
    sprintf("https://github.com/%s", file.path(user, repo, branch))
}

GRAN.repos <- function(...){
    file.path('http://tx.technion.ac.il/~renaud/GRAN', ...)
}
GRAN.fields <- function(named = FALSE){
    f <- c(Repo = 'GithubRepo', User = "GithubUsername"
            , Branch = "GithubRef", Forked = 'GithubFork'
            , SHA1 = 'GithubSHA1')
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

md5hash <- function(x, strip = x, skip = NULL){
    x <- normalizePath(x)
    # compute hash list of source directory
    hash <- md5sum(dir(x, recursive = TRUE, full.names = TRUE))
    
    # process
    hash <- hash[basename(names(hash)) != 'MD5']
    if( !is_NA(strip) ){
        if( identical(strip, 1L) ) strip <- dirname(x)
        else strip <- normalizePath(strip) 
        names(hash) <- gsub(strip, '', names(hash), fixed = TRUE)
    }
    names(hash) <- gsub('^/', '', names(hash))
    
    # skip requested patterns
    if( !is.null(skip) ){
        for(s in skip){
            hash <- hash[grep(s, names(hash), invert = TRUE)]
        }
    }
    # return
    hash
}

#' @importFrom tools md5sum
GRAN.update <- function(src, outdir = dirname(normalizePath(src)), clean = FALSE, force = FALSE, fields = GRAN.fields(), actions = c('changes', 'PACKAGES', 'index'), verbose = TRUE){
    
    # dump messages if non-verbose
    if( !verbose ) message <- function(...) NULL
    
    message("* Updating GRAN in ", outdir, ' [source: ', src, ']')
    
    # initialise complete repository structure if necessary
    if( !is.dir(outdir) || clean ) create_repo(outdir, pkgs = NA)
    
    # match type of action to perform
    actions <- match.arg(actions, several.ok = TRUE)
    
    # push changes to Github
    has_git <- function(x) is.dir(file.path(x, '.git'))
    if( 'changes' %in% actions ){
        
        tmp <- src
        within_git <- FALSE
        while( nzchar(tmp) && !within_git){
            within_git <- has_git(tmp)
            tmpd <- dirname(tmp)
            if( tmpd == tmp ) break
            else tmp <- tmpd
        }
        message("* Pushing changes ... ", appendLF = FALSE)
        if( within_git ){
            pkg_srcdir <- list.dirs(src, recursive = FALSE, full.names = TRUE)
            nb <- sapply(pkg_srcdir, function(srcd){
                    cf <- list.files(srcd, pattern = "commit-[^/]+$", full.names = TRUE)
                    if( !length(cf) ) return(0L)
                    
                    # load description file to extract some GitHub data 
                    desc <- read.dcf(file.path(srcd, 'DESCRIPTION'))
                    ghUser <- desc[1L, 'GithubUsername']
                    ghRepo <- desc[1L, 'GithubRepo']
                    
                    # build commit message
                    msg <- sapply(cf, function(f){
                        l <- readLines(f)
                        sha <- gsub(".*commit-", "", f)
                        paste0(c(sprintf('%s: %s/%s@%s', basename(srcd), ghUser, ghRepo, sha), l), collapse = "\n")
                    })
                    sha_sid <- substr(gsub("commit-", "", basename(cf), fixed = TRUE), 1, 7)
                    message("\n  - ", basename(srcd), sprintf(" (%s): ", ghUser), str_out(sha_sid, total = TRUE, quote = FALSE), appendLF = FALSE)
                    tmp <- tempfile(paste0("commit-", basename(srcd)), fileext=".log")
                    on.exit( unlink(tmp), add = TRUE)
                    cat(msg, file = tmp, sep = "\n\n")
                    git_cmd <- sprintf("git add . && git commit -F %s;", srcd, tmp)
    #                print(git_cmd)
    #                cat(readLines(tmp), sep = "\n")
                    message()
                    owd <- setwd(srcd)
                    on.exit( setwd(owd), add = TRUE)
                    system(git_cmd)
                    unlink(cf)
                    length(msg)
            })
            if( all(nb == 0) ) message('OK [none]')
            else message()
            
        }else message('SKIP [not a git repo]')
        
    }
    
    # check if things have changed based on MD5 file -- unless required to force update
    MD5_file <- file.path(src, 'MD5')
    
    # generate current MD5 content
    # compute hash list of output repo directory
    .md5hash <- function(...) md5hash(..., skip = "PACKAGES(\\.gz)?$")
    gh_hash <- .md5hash(src)
    hash <- c(setNames(gh_hash, file.path('github', names(gh_hash))), .md5hash(file.path(outdir, c('src', 'bin')), strip = outdir))
    if( file.exists(MD5_file) && !force ){
        message("* Checking changes based on MD5 file ... ", appendLF = FALSE)
        hash0 <- readLines(MD5_file)
        m <- str_match(hash0, "^([^ ]+) (.*)")
        hash0 <- setNames(m[, 3L], m[, 2L])
        # exit if nothing needs to be done
        stopifnot( !anyDuplicated(names(hash0)) && !anyDuplicated(names(hash)) )
        if( identical(hash, hash0) || identical(hash[names(hash0)], hash0) ){
            message('OK [', digest(hash0), ']')
            return(character())
        }
        
        f_shared <- intersect(names(hash0), names(hash))
        f <- list(New = setdiff(names(hash), names(hash0))
                   , Deleted = setdiff(names(hash0), names(hash))
                    , Changed = f_shared[which(hash[f_shared] != hash0[f_shared])])
        message(sprintf('NOTE [ %s changed | %s new | %s deleted ]', length(f$Changed), length(f$New), length(f$Deleted)))
        lapply(names(f), function(t){
            if( length(f[[t]]) )
                message(' * ', t, ':\n   - ', str_out(f[[t]], Inf, sep = "\n   - "))  
        })
    }
    
    # update PACKAGES files
    if( 'PACKAGES' %in% actions ){
        
        # make PACKAGES in output repos for built packages
        create_repo(outdir, verbose = TRUE)
        
        ## add Github packages to source PACKAGES
        # write PACKAGES file from Github source directories 
        write_PACKAGES(src, type = 'source', unpacked = TRUE, fields = fields, latestOnly = FALSE)
        P <- available.packages(file.path('file:/', normalizePath(src)), fields = fields, filters = .PACKAGES_filters_all_versions)
        # fix for migration of field names
        if( length(no_repo <- is.na(P[, 'GithubRepo'])) ){
            P[no_repo, 'GithubRepo'] <- P[no_repo, 'Package']  
        }
        # merge with PACKAGES in src/contrib
        out_contrib <- contrib.url(outdir, type = 'source');
        pfile <- file.path(out_contrib, 'PACKAGES')
        write.dcf(P, file = pfile, append = TRUE)
        # create .gz version
        gzfile <- gzfile(paste0(pfile, '.gz'))
        write.dcf(P, file = gzfile)
        close(gzfile)
    }
    
    # generate index file
    if( 'index' %in% actions ){
        message("* Generating index file:")
        write_PACKAGES_index(outdir, title = 'GRAN: Github R Archive Network')
    }
    
    message("* Writing MD5 file ... ", appendLF = FALSE)
    cat(paste(names(hash), hash), file = MD5_file, sep = "\n")
    message('OK [', digest(hash), ']')
    
    # return output directory to calling script in non-interactive mode
    invisible(outdir)
}
