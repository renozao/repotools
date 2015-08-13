# Project: repotools
# 
# Author: Renaud Gaujoux
# Created: May 30, 2014
###############################################################################

gh_api.path <- function(..., type = c('api', 'raw')){
    type = match.arg(type)
    url <- file.path(sprintf('https://%s.github.com', type), ...)
    #message(url)
    url
}

gh_io.path <- function(user, repo, ...) file.path(sprintf('http://%s.github.io/%s', user, repo), ...)

gh_rawapi.path <- function(...){
    gh_api.path(..., type = 'raw')
}

gh_repo_path <- function(user, repo, branch = 'master'){
    sprintf("https://github.com/%s", file.path(user, repo, branch))
}

GRAN.repos <- function(...){
    file.path('http://renozao.github.io/GRAN', ...)
    file.path('http://localhost/~renaud/projects/repotools/testRepo', ...)
}
GRAN.fields <- function(named = FALSE, all = FALSE){
    f <- c(Repo = 'GithubRepo', User = "GithubUsername"
            , Branch = "GithubRef", Forked = 'GithubFork'
            , SHA1 = 'GithubSHA1', RepoType = 'GRANType')
    if( all ) f <- c(f, XDepends = 'XDepends', Key = 'XPath', Type = 'GRANType', Path = 'GRANPath')
    if( !named ) f <- unname(f)
    f
}

GRAN.available <- function(type = getOption('pkgType'), fields = GRAN.fields(all = TRUE), ..., version = NULL){
    if( is.null(version) ){
        p <- available.pkgs(contrib.url2(GRAN.repos(), type = type), fields = fields, ...)
    }else{
        p <- available.pkgs(contrib.url(GRAN.repos(), type = type), fields = fields, ..., filters = .PACKAGES_filters_keep_all_versions)
        
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
    url <- gh_rawapi.path(user, repo, ref, ...)
#    getURL(url, .opts = list(followlocation = TRUE))
    con <- curl(url)
    res <- readLines(con)
    close(con)
    res
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
    x <- x[file.exists(x)]
    
    if( !length(x) ) return( character() )
    
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

GRAN_key <- function(...){
    
    
    .local <- function(...){
        res <- paste0(..., collapse = "/")
        gsub("/NA$", "", res)
    }
    
    args <- list(...)
    if( length(args) == 1L ){
        x <- args[[1L]]
        if( !is.null(nrow(x)) ) return( apply(x, 1L, .local) )
        stopifnot( is.character(x) )
    }
    do.call(.local, args)
    
}

GRAN.update <- function(src, outdir = dirname(normalizePath(src)), clean = FALSE, force = FALSE, fields = GRAN.fields(), update = FALSE, actions = c('changes', 'PACKAGES', 'index'), verbose = TRUE){
     
    # dump messages if non-verbose
    if( !verbose ) message <- function(...) NULL
    
    message(sprintf("* Updating GRAN packages in %s [source: %s]", outdir, src))
    
    # initialise complete repository structure if necessary
    if( !is.dir(outdir) || clean ) create_repo(outdir, pkgs = NA)
    
    # update GitHub source package
    src_github <- file.path(src, 'github')
    contrib_github <- contrib.url(src_github, type = 'source')
    GRAN.update_github(src_github, update = update)
    
    # update Drat packages
    DATA <- GRAN.update_drat(file.path(src, 'drat'), update = update)
    
    # match type of action to perform
    actions <- match.arg(actions, several.ok = TRUE)
    
    # update PACKAGES files
    if( 'PACKAGES' %in% actions ){
        
        # make PACKAGES in output repos for built packages
        create_repo(outdir, verbose = TRUE)
        
        ## add Github packages to those in src/contrib
        ## Only add non-forked packages
        gh_P <- read.dcf(file.path(contrib_github, 'PACKAGES'))
        # fix for migration of field names
        gh_P <- add_dcf_field(gh_P, 'GithubRepo', gh_P[, 'Package'])
        gh_P <- add_dcf_field(gh_P, 'GRANType', 'github')
        gh_P <- cbind(gh_P, pkgType = 'source', R_release = NA)
        
        # preprend to drat packages
        library(plyr)
        PACKAGES <- rbind.fill(data.frame(gh_P, stringsAsFactors = FALSE), DATA$PACKAGES)
        PACKAGES <- add_dcf_field(PACKAGES, 'GRANPath', GRAN_key(PACKAGES[, c('GithubUsername', 'GithubRepo', 'GithubRef')]))
        
        # Filter out bad packages
        # remove bad packages
        bad <- bad_version(PACKAGES[, 'Version'])
        if( any(bad) ){
            message(sprintf("* Removing packages with invalid versions [%s])"
                            , str_out(setNames(PACKAGES[bad, 'Version'], PACKAGES[bad, 'Package']), use.names = TRUE, total = TRUE)
                            )
            )
        }
        PACKAGES <- PACKAGES[!bad, , drop = FALSE]
        
        # combined GitHub + Drat repos
        write_GRAN_repo('pkgType', function(P){
            msg <- sprintf('  * Repo all :%i ', nrow(P))
            list(PACKAGES = P, path = outdir, msg = msg)
        }, PACKAGES = PACKAGES, append = TRUE, no.dups = FALSE)
                
    }
    
    # generate index file
    if( 'index' %in% actions ){
        message("* Generating index file:")
        write_PACKAGES_index(outdir, title = 'GRAN: Github R Archive Network')
    }
    
}

# Fetch package data from GitHub and update associated unpacked sub-directory
update_github_user <- function(user, dir, repos = NULL){
    
    # clean up priority repos
    repos <- repos[is.na(repos)]
    
    # fetch all the users repositories
    user_repos <- gh_user_repo(user)
    
    if( !length(user_repos) ){
        message(" ... SKIP [no repo]")
        return()
    }
    
    # fix NULL elements
    user_repos <- sapply(user_repos, function(rdata){
                rdata <- sapply(rdata, '%||%', NA, simplify = FALSE) 
                rdata
            }, simplify = FALSE)
    user_repos0 <- user_repos
    pushed_at <- sapply(user_repos0, '[[', 'pushed_at')
    
    # check if anything changed at all
    user_url <- dirname(gsub("^https?://", '', user_repos[[1L]]$html_url))
    user <- user_repos[[1L]]$owner$login # to ensure the case is correct
    udir <- file.path(dir, user_url)
    loc_user_file <- file.path(udir, 'USER')
    if( file.exists(loc_user_file) ){
        luser <- read.table(loc_user_file, sep = "\t", stringsAsFactors = FALSE, header = TRUE)
        rname_field <- intersect(c('repo', 'name'), colnames(luser))
        pushed_at0 <- setNames(luser[, 'pushed_at'], luser[, rname_field])
        rname <- union(names(pushed_at), names(pushed_at0))
        changed <- rname[which( pushed_at[rname] !=  pushed_at0[rname] )]
        user_repos <- user_repos[names(user_repos) %in% changed]
    }
    
    save_user_data <- function(file = loc_user_file){
        fields <- c('name', 'full_name', 'html_url', 'fork', grep('_at$', names(user_repos0[[1L]]), value = TRUE), 'language')
        ldata <- cbind(user = user
                      , ldply(user_repos0, .id = NULL, function(x) as.data.frame(x[fields], stringsAsFactors = FALSE))
                      , stringsAsFactors = FALSE)
        dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
        write.table(ldata, file = file, sep = "\t", row.names = FALSE)
    }
    
    if( !length(user_repos) ){
        message(" ... SKIP [not changed]")
        return()
    }
    message(":")
    
    # put priority to requested repos and those flagged as R language
    R_repos <- which(sapply(user_repos, function(x) identical(x$language, 'R')))
    priority <- unique(c(repos, names(R_repos)))
    m <- match(names(user_repos), priority)
    user_repos <- user_repos[order(m)]
    
    res <- sapply(user_repos, function(rdata){
                
        # create base directory
        path <- gsub("^https?://", '', rdata$html_url)
        pdir <- file.path(dir, path)
        repo <- rdata$name
        
        message("  ** ", repo, appendLF = FALSE)
                
        # process all suitable branches: default branch, devel*
        refs <- gh_repo_head(user, rdata$name)
        res <- integer()
        if( !length(refs) ){
            message(" ... OK [empty]")
            
        }else{
            refs_todo <- unique(c(rdata$default_branch, grep("^devel[^/]*", names(refs), value = TRUE)))
            if( length(refs_todo) > 1L ) message(":")
            
            res <- sapply(refs_todo, function(ref){
                
                # log branch name
                if( length(refs_todo) > 1L ){
                    message("     - ", ref, "/ ... ", appendLF = FALSE)
                }else message("/", ref, "/ ... ", appendLF = FALSE)
                
                refobject <- refs[[ref]] 
                SHA <- refobject$object$sha
                
                # ref sub-directory
                refdir <- file.path(pdir, 'tarball', ref, repo)
                
                # skip packages processed by the webhook
                flag_file <- file.path(refdir, 'SHA1')
                if( dir.exists(refdir) && !file.exists(flag_file) ){
                    message('SKIP [in webhook]')
                    return(0L)
                }
                
                # load/check local DESCRIPTION
                desc_file <- file.path(refdir, 'DESCRIPTION')
                if( file.exists(desc_file) ){
                    dcf <- read.dcf(desc_file)
                    dcf <- add_dcf_field(dcf, 'GithubSHA1')
                    # skip whole repo if no changes
                    if( dcf[, 'GithubSHA1'] %in% SHA ){
                        message('OK [no changes]')
                        return(0L)
                    }
                }
                
                # fetch content of master branch
                cnt <- gh_get_content(user, repo, ref = ref)
                if( is.null(d <- cnt$DESCRIPTION) ){
                    message('SKIP [not a package]')
                    return(0L)
                }
                
                # create ref sub-directory
                dir.create(refdir, recursive = TRUE, showWarnings = FALSE)
                
                # update DESCRIPTION only if necessary
                res <- 1L
                cat(SHA, '\n', file = flag_file, sep = '')
                message(sprintf("DESCRIPTION", substr(SHA, 1, 7)), appendLF = FALSE)
                tmp_desc <- tempfile('DESCRIPTION')
                on.exit( unlink(tmp_desc), add = TRUE)
                download.file(d$download_url, dest = tmp_desc, quiet = TRUE)
                message(sprintf("[%s] ", substr(SHA, 1, 7)), appendLF = FALSE)
                # add fields
                dcf <- read.dcf(tmp_desc)
                dcf <- cbind(dcf, GithubRepo = repo
                                , GithubUsername = user
                                , GithubRef = ref
                                , GithubSHA1 = SHA
                                , GithubFork = ifelse(rdata$fork, 'yes', 'no')
                                )
                
                # create src/ sub-directory if present
                src_dir <- file.path(refdir, 'src')
                if( !is.null(cnt$src) && cnt$src$type == 'dir' ){
                    if( !dir.exists(src_dir) ){
                        res <- res + 1L
                        message("src(+)", appendLF = FALSE)
                        dir.create(src_dir, recursive = TRUE, showWarnings = FALSE)
                        cat('', file = file.path(src_dir, 'README'))
                    }
                }else if( dir.exists(src_dir) ){
                    res <- res + 1L
                    message("src(-)", appendLF = FALSE)
                    unlink(src_dir, recursive = TRUE, force = TRUE)
                }
                message()
                
                # update DESCRIPTION file
                write.dcf(dcf, desc_file)
                return(res)
            })
        }
        
        res[res > 0L]
    }, simplify = FALSE)
    
    # update user repo local data
    save_user_data()
    
    res[lengths(res) > 0L]
}

list.github.packages <- function(){
    
    # init result
    res <- matrix(NA, 0, 2, dimnames = list(NULL, c('user', 'repo')))
    
    # From CRAN
    message("* Checking Github package on CRAN ... ", appendLF = FALSE)
    tmp <- file.path(tempdir(), 'packages.rds')
    if( !file.exists(tmp) ) download.file("http://cran.r-project.org/web/packages/packages.rds", dest = tmp, quiet = TRUE)
    CRAN <- readRDS(tmp)
    # select packages with GitHub BugReports
    m <- str_match(CRAN[, 'BugReports'], "://(github\\.com/([^/]+)/([^/]+))/issues")
    i_gh <- which(!is.na(m[, 1L]))
    m <- m[i_gh, 3:4, drop = FALSE]
    message(sprintf("OK [%i/%i packages | %s users]", nrow(m), nrow(CRAN), length(unique(m[, 1L]))))
    res <- rbind(res, m)
    
    # From drat forks
    message("* Checking drat froks on Github ... ", appendLF = FALSE)
    drat <- fetch_drat_forks()
    drat_users <- unique(sapply(drat, function(x) x$owner$login))
    message(sprintf("OK [%s users]", length(drat_users)))
    drat_users <- setdiff(drat_users, res[, 'user'])
    if( length(drat_users) )
        res <- rbind(res, cbind(drat_users, NA))
    
    # total
    message(sprintf("* Number of unique Github users: %s", length(unique(res[, 'user']))))
    res
}

#' Updates GitHub Source Package Repository
#' 
#' @param src path to the directory that holds the package directory tree 
#' @importFrom tools md5sum
GRAN.update_github <- function(src, force = FALSE, fields = GRAN.fields(), update = FALSE, actions = c('changes', 'PACKAGES', 'index'), test = FALSE, verbose = TRUE){
    
    do_commit <- TRUE
    if( test ){
        do_commit <- FALSE
        verbose <- TRUE
    }
    
    # dump messages if non-verbose
    if( !verbose ) message <- function(...) NULL
    
    outdir <- src
    src <- contrib.url(src, type = 'source')
    src_fullpath <- normalizePath(src)
    message("* Updating GitHub source packages in ", src_fullpath)
    
    # match type of action to perform
    actions <- match.arg(actions, several.ok = TRUE)
    
    # fetch updates for "all" GitHub R repos
    if( update ){
        
        gh_packages <- list.github.packages()
        
        # update each unpacked sub-directory
        if( nrow(gh_packages) ){
            on.exit( gh_context_save(), add = TRUE)
            library(plyr)
#            o <- options(download.file.method = 'curl')
#            on.exit( options(o), add = TRUE)
            # update Github unpacked packages for each user
            with_rcurl({
                iuser <- split(seq(nrow(gh_packages)), gh_packages[, 'user'])
#                iuser <- head(iuser, 3L)
                l_ply(iuser, function(i){
                    data <- gh_packages[i, , drop = FALSE]
                    user <- data[1L, 'user']
                    message("* Checking ", user, appendLF = FALSE)
                    update_github_user(user, dir = src_fullpath, repos = data[, 'repo'])
                })
            })
        }
    }
    
    # check if things have changed based on MD5 file -- unless required to force update
    MD5_file <- file.path(src, 'MD5')
    
    # generate current MD5 content
    # compute hash list of output repo directory
    .md5hash <- function(...) md5hash(..., skip = "PACKAGES(\\.gz)?$")
    gh_hash <- .md5hash(src)
    hash <- c(setNames(gh_hash, names(gh_hash))) #, .md5hash(file.path(outdir, c('src', 'bin')), strip = outdir))
    CHANGES <- list()
    if( file.exists(MD5_file) && !force ){
        message("* Checking changes based on MD5 file ... ", appendLF = FALSE)
        hash0 <- readLines(MD5_file)
        m <- str_match(hash0, "^([^ ]+) (.*)")
        hash0 <- setNames(m[, 3L], m[, 2L])
        # exit if nothing needs to be done
        stopifnot( !anyDuplicated(names(hash0)) && !anyDuplicated(names(hash)) )
        if( identical(hash, hash0) ){
            message('OK [', digest(hash0), ']')
            return( invisible(character()) )
        }
        
        f_shared <- intersect(names(hash0), names(hash))
        CHANGES <- f <- list(New = setdiff(names(hash), names(hash0))
                   , Deleted = setdiff(names(hash0), names(hash))
                    , Changed = f_shared[which(hash[f_shared] != hash0[f_shared])])
        message(sprintf('NOTE [ %s changed | %s new | %s deleted ]', length(f$Changed), length(f$New), length(f$Deleted)))
        lapply(names(f), function(t){
            if( length(f[[t]]) )
                message(' * ', t, ':\n   - ', str_out(f[[t]], Inf, sep = "\n   - "))  
        })
    }
    
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
        if( within_git || !do_commit ){
            pkg_srcd <- list.files(src, recursive = TRUE, full.names = TRUE, pattern = '^DESCRIPTION$')
            # filter those that changed
            if( length(CHANGES) )
                pkg_srcd <- pkg_srcd[pkg_srcd %in% file.path(src, c(CHANGES$Changed, CHANGES$New))]
            if( !length(pkg_srcd) ) message('OK [none]')
            else{
                sapply(pkg_srcd, function(desc_file){
                    
                    srcd <- dirname(desc_file)
                    message("\n  - ", basename(srcd), " ... " , appendLF = FALSE)
                    # load description file to extract some GitHub data 
                    desc <- read.dcf(desc_file)
                    ghRepo <- if( 'GithubRepo' %in% colnames(desc) ) desc[1L, 'GithubRepo'] else desc[, 'Package']
                    ghUser <- desc[1L, 'GithubUsername']
                    ghRef <- desc[1L, 'GithubRef']
                    ghSHA1 <- if( 'GithubSHA1' %in% colnames(desc) ) desc[1L, 'GithubSHA1']
                    
                    # build commit message
                    ref <- paste0(sprintf('%s/%s', ghUser, ghRepo), if( length(ghSHA1) ) paste0('@', ghSHA1))
                    msg <- sprintf('%s: %s', basename(srcd), ref) 
                    message("[", ref, ']')
                    
                    # commit
                    tmp <- tempfile(paste0("commit-", basename(srcd), '-', ghRef), fileext=".log")
                    on.exit( unlink(tmp), add = TRUE)
                    cat(msg, file = tmp, sep = "\n\n")
                    git_cmd <- sprintf("git add . && git commit -F %s;", tmp)
    #                print(git_cmd)
    #                cat(readLines(tmp), sep = "\n")
                    owd <- setwd(srcd)
                    on.exit( setwd(owd), add = TRUE)
                    if( do_commit ) system(git_cmd)
                    else message(sprintf("skipped [%s]: %s", srcd, git_cmd))
                    length(msg)
                })
            }
            
        }else message('SKIP [not a git repo]')
        
    }
    
    # update PACKAGES files
    if( 'PACKAGES' %in% actions ){
                
        ## add Github packages to source PACKAGES
        # write PACKAGES file from Github source directories 
        fields <- c('Date', fields)
        message("* Generating PACKAGES files in '", src, "'")
        write_PACKAGES(src, type = 'source', unpacked = TRUE, fields = fields, latestOnly = FALSE, subdirs = TRUE)
        
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
    invisible(src)
}
