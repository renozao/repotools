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
        gsub("(/NA)*$", "", res)
    }
    
    args <- list(...)
    if( length(args) == 1L ){
        x <- args[[1L]]
        if( !is.null(nrow(x)) ){
            if( length(grep("^Github", colnames(x))) ){
                .f <- c('GithubUsername', 'GithubRepo', 'GithubRef', 'GithubPath')
                x <- rbind.fill.matrix(x, matrix(NA, 0, length(.f), dimnames = list(NULL, .f)))
                x <- x[, .f, drop = FALSE]
            }
            return( apply(x, 1L, .local) )
        }
        stopifnot( is.character(x) )
    }
    do.call(.local, args)
    
}

GRAN.update <- function(src, outdir = dirname(normalizePath(src)), clean = FALSE, force = FALSE, fields = GRAN.fields(), update = NA, actions = c('changes', 'PACKAGES', 'index'), verbose = TRUE){
     
    # dump messages if non-verbose
    if( !verbose ) message <- function(...) NULL
    
    message(sprintf("* Updating GRAN packages in %s [source: %s]", outdir, src))
    
    # initialise complete repository structure if necessary
    if( !is.dir(outdir) || clean ) create_repo(outdir, pkgs = NA)
    
    # update GitHub source package
    update_gh <- update
    if( isTRUE(update_gh) ) update_gh <- c('Github', 'drat')
    contrib_github <- GRAN.update_github(src, update = update_gh)
     
    # update Drat packages
    DATA <- GRAN.update_drat(file.path(src, 'drat'), update = update)
    
    # match type of action to perform
    actions <- match.arg(actions, several.ok = TRUE)
    
    # update PACKAGES files
    if( 'PACKAGES' %in% actions ){
        
        # make PACKAGES in output repos for built packages
        create_repo(outdir, verbose = TRUE)
        
        ## add Github packages to those in src/contrib
        gh_P <- read.dcf(file.path(contrib_github, 'PACKAGES'))
        # fix for migration of field names
        gh_P <- add_dcf_field(gh_P, 'GithubRepo', gh_P[, 'Package'])
        gh_P <- add_dcf_field(gh_P, 'GRANType', 'github')
        gh_P <- cbind(gh_P, pkgType = 'source', R_release = NA)
        
        # preprend to drat packages
        library(plyr)
        PACKAGES <- rbind.fill(data.frame(gh_P, stringsAsFactors = FALSE), DATA$PACKAGES)
        PACKAGES <- add_dcf_field(PACKAGES, 'GRANPath', GRAN_key(PACKAGES))
        
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
update_github_user <- function(user, dir, repos = NULL, repos_data = NULL, packages = NULL, all.R = FALSE){
    
    
    if( !is.null(repos) ) messagef(": %s", str_out(repos, 4, total = length(repos) > 4), appendLF = FALSE)
    # clean up priority repos
    repos <- repos[!is.na(repos)]
    
    # fetch all the users repositories
    user_repos <- gh_user_repo(user)
    
    if( !length(user_repos) ){
        message(" ... SKIP [user has no repo]")
        return()
    }
    
    user_repos0 <- user_repos
    
    # build user repos index
    user_url <- dirname(gsub("^https?://", '', user_repos[[1L]]$html_url))
    user <- user_repos[[1L]]$owner$login # to ensure the case is correct
    udir <- file.path(dir, user_url)
    loc_user_file <- file.path(udir, 'USER')
    is_updating <- file.exists(loc_user_file)
    is_updating <- nrow(repos_data) %||% 0
    fields <- c('name', 'full_name', 'html_url', 'fork', grep('_at$', names(user_repos0[[1L]]), value = TRUE), 'language')
    .USER_INDEX <- cbind(user = user
            , ldply(user_repos0, .id = NULL, function(x) as.data.frame(x[fields], stringsAsFactors = FALSE))
                , indexed_at = NA
                , refs_sha = NA
                , desc_sha = NA
            , stringsAsFactors = FALSE)
    if( is_updating ){
#        luser <- read.repos(file = loc_user_file)
        luser <- as.matrix(repos_data)
        # fix for backward compatiblility
        colnames(luser)[colnames(luser) == 'repo'] <- 'name'
        luser <- set_column(luser, 'indexed_at', luser[, 'pushed_at'])
        luser <- rbind.fill.matrix(luser, .USER_INDEX[0, , drop = FALSE])
        luser <- luser[!is.na(luser[ , 'name']), , drop = FALSE]
        # force in previous indexing data
        inm <- match(luser[, 'name'], .USER_INDEX$name, nomatch = 0L)
        .f <- c('indexed_at', 'refs_sha', 'desc_sha')
        .USER_INDEX[inm, .f] <- luser[inm > 0, .f]
    }
    # set rownames
    rownames(.USER_INDEX) <- .USER_INDEX[['name']]
    
    # save repos data on.exit
    .update_repo_index <- function(data, content){
        .USER_INDEX[data$name, 'indexed_at'] <<- data[['pushed_at']]
        if( is.matrix(content) ){
            SHA <- paste0(na.omit(content[2, ]), collapse = ' ')
            if( !nzchar(SHA) ) SHA <- NA
            .USER_INDEX[data$name, 'refs_sha'] <<- unname(SHA)
            desc <- paste0(na.omit(content[1, ]), collapse = ' ')
            if( !nzchar(desc) ) desc <- NA
            .USER_INDEX[data$name, 'desc_sha'] <<- unname(desc)
        }
        
    }

    .save_user_data <- function(file = loc_user_file){
        dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
        write.repos(.USER_INDEX, file = file)
    }
    on.exit( .save_user_data(), add = TRUE)
    
    force <- FALSE
#    if( user != 'renozao' ) force <- TRUE
    
    # only check repos that were pushed after last indexing
    diffs <- .USER_INDEX$pushed_at !=  .USER_INDEX$indexed_at
    changed <- .USER_INDEX$name[which( is.na(diffs) | diffs )]
    if( !force ) user_repos <- user_repos[names(user_repos) %in% changed]
    
    if( !length(user_repos) ){
        message(sprintf(" ... SKIP [not changed - %s repos]", length(user_repos0)))
        return()
    }
    
    # only process given repos vector and all the user's R repos (if requested or first time indexing)
    user_R_repos <- which(sapply(user_repos, function(x) identical(x$language, 'R')))
    if( force || all.R || !is_updating ){
        repos <- c(repos, names(user_R_repos))
    }
    m <- match(names(user_repos), unique(repos))
    user_repos <- user_repos[!is.na(m)]
#    user_repos <- user_repos[order(m)]

    if( !length(user_repos) ){
        message(sprintf(" ... SKIP [not changed - %s R / %s changed / %s repos]", length(user_R_repos), length(user_repos), length(user_repos0)))
        return()
    }
    
    if( is_updating ) message(sprintf(" (updating %s/%s):", length(user_repos), length(user_repos0)))
    else message(sprintf(" (indexing %s repos):", length(user_repos)))
    
    res <- sapply(user_repos, function(rdata){
        
        # create base directory
        path <- gsub("^https?://", '', rdata$html_url)
        pdir <- file.path(dir, path)
        repo <- rdata$name
        RDATA <- .USER_INDEX[repo, , drop = TRUE]
        
        # last commit
        if( !is_NA(COMMIT_SHA0 <- RDATA[['refs_sha']]) ){
            m <- str_match(COMMIT_SHA0, "([^@]+)@([^ ]+)")
            COMMIT_SHA0 <- setNames(m[, 3], m[, 2])
        }
        # DESCRIPITON file SHA
        if( !is_NA(DESC_SHA0 <- RDATA[['desc_sha']]) ){
            m <- str_match(DESC_SHA0, "([^@]+)@([^ ]+)")
            DESC_SHA0 <- setNames(m[, 3], m[, 2])
        }
        
        message("  ** ", repo, appendLF = FALSE)
                
        # process all suitable branches: default branch, devel*
        refs <- gh_repo_head(user, rdata$name)
        res <- NULL
        if( !length(refs) ){
            message(" ... OK [empty]")
            
        }else{
            refs_todo <- unique(c(rdata$default_branch, grep("^devel[^/]*$", names(refs), value = TRUE)))
            if( length(refs_todo) > 1L ) message(":")
            
            res <- sapply(refs_todo, function(ref){
                
                # log branch name
                if( length(refs_todo) > 1L ){
                    message("     - ", ref, "/ ... ", appendLF = FALSE)
                }else message("/", ref, "/ ... ", appendLF = FALSE)
                
                refobject <- refs[[ref]] 
                SHA <- refobject$commit$sha
                
                # not sure how this can happen, but it does happens (e.g., wch/r-source/trunk)
                if( is.null(SHA) ){
                    message('SKIP [invalid ref]')
                    return(c(NA, NA))   
                }
                
                # ref sub-directory
                refdir <- file.path(pdir, 'tarball', ref, repo)
                added_flag <- if( is.na(RDATA[['indexed_at']]) ) '+' else ''
                # skip packages processed by the webhook
#                flag_file <- file.path(refdir, 'SHA1')
#                if( dir.exists(refdir) && !file.exists(flag_file) ){
#                    message('SKIP [in webhook]')
#                    return(0L)
#                }
                
                # load/check last local commit SHA
                desc_file <- list.files(refdir, recursive = TRUE, full.names = TRUE, pattern = '^DESCRIPTION$')
                stopifnot( length(desc_file) %in% c(0, 1) )
                # SHAs
                SHA0 <- COMMIT_SHA0[ref]
                SHA0_D <- DESC_SHA0[ref]
                DESC_SHA <- COMMIT_SHA <- sprintf("%s@%s", ref, SHA)
                if( !is.na(SHA0_D) ) DESC_SHA <- sprintf("%s@%s", ref, SHA0_D)
                if( is_NA(SHA0) && length(desc_file) ){
                    dcf <- read.dcf(desc_file)
                    dcf <- add_dcf_field(dcf, 'GithubSHA1')
                    SHA0 <- dcf[, 'GithubSHA1']
                }
                
                # skip whole repo if no changes
                if( SHA %in% SHA0 && !force ){
                    message('OK [no changes]')
                    return(c(DESC_SHA, COMMIT_SHA))
                }
                
                # fetch content of master branch
                cnt <- gh_get_content(user, repo, ref = ref)
                if( is.null(d <- cnt$DESCRIPTION) ){
                    
                    # test pkg/ sub-directory (e.g., rforge-like tree structure)
                    if( !is.null(cnt$pkg) && cnt$pkg$type == 'dir' ){
                        cnt <- gh_get_content(user, repo, ref = ref, path = 'pkg')
                    }
                    
                    if( is.null(d <- cnt$DESCRIPTION) ){
                        message(added_flag, 'SKIP [no package]')
                        return(c(NA, COMMIT_SHA))
                    }
                }
                                
                # append package subdirectory
                pkg_subdir <- dirname(cnt$DESCRIPTION$path)
                if( pkg_subdir != '.' ){
                    refdir <- file.path(refdir, paste0('!', pkg_subdir))
                    message(added_flag, file.path(pkg_subdir, ''), appendLF = FALSE)
                    added_flag <- ''
                }
                
                
                # Build Github data part
                GH_DATA <- cbind(GithubRepo = repo
                        , GithubUsername = user
                        , GithubRef = ref
                        , GithubSHA1 = SHA
                        , GithubFork = ifelse(rdata$fork, 'yes', 'no')
                        , GithubPath = if( pkg_subdir != '.' ) pkg_subdir else NA)
                
                # download DESCRIPTION file only if necessary
                SHA_D <- cnt$DESCRIPTION$sha
                # load data from packages db if present
                dcf <- if( i_pkg <- match(GRAN_key(GH_DATA), rownames(packages), nomatch = 0L) ){
                    packages[i_pkg, , drop = FALSE] 
                }
                # fetch update if necessary
                if( !SHA_D %in% SHA0_D || is.null(dcf) ){
                    
                    message(added_flag, "DESCRIPTION", appendLF = FALSE)
                    tmp_desc <- file.path(tempdir(), paste0('DESCRIPTION_', DESC_SHA))
                    on.exit( unlink(tmp_desc), add = TRUE)
                    download.file(d$download_url, dest = tmp_desc, quiet = TRUE)
                    # add fields
                    dcf <- try(read.dcf(tmp_desc), silent = TRUE)
                    if( is(dcf, 'try-error') ){
                        message(sprintf('[error: %s]', dcf))
                        return(c(DESC_SHA, COMMIT_SHA))
                    }
                }else{
                    message("CACHE", appendLF = FALSE)
                }
                message(sprintf("[%s] ", substr(SHA_D, 1, 7)), appendLF = FALSE)
                DESC_SHA <- sprintf("%s@%s", ref, SHA_D)
                
                # add/replace up-to-date Github data
                dcf <- cbind(dcf[, setdiff(colnames(dcf), colnames(GH_DATA)), drop = FALSE], GH_DATA)
                #
                
                # create ref sub-directory
                dir.create(refdir, recursive = TRUE, showWarnings = FALSE)
                
                # process src/ sub-directory
                has_src <- !is.null(cnt$src) && cnt$src$type == 'dir'
                refdir_src <- file.path(refdir, 'src')
                if( has_src ){
                    message('src ', appendLF = FALSE)
                    dir.create(refdir_src, recursive = TRUE, showWarnings = FALSE)
                }else unlink(refdir_src)
                dcf <- add_dcf_field(dcf, 'NeedsCompilation', .yesno(has_src), force = TRUE)
                #
                
                # update DESCRIPTION file
                write.dcf(dcf, file.path(refdir, 'DESCRIPTION'))
                
                message()
                return(c(DESC_SHA, COMMIT_SHA))
            })
        }
        
        # update indexed data
        .update_repo_index(rdata, res)
        
        res[res > 0L]
    }, simplify = FALSE)
    
#    res <- res[lengths(res) > 0L]
#    res
    .USER_INDEX
}

repo_matrix <- function(source, obj){
    
    fields <- c('source', 'user', 'name', 'full_name', 'html_url', 'fork', 'created_at', 'updated_at', 'pushed_at', 'language')
    res <- matrix(NA, 0, length(fields), dimnames = list(NULL, fields))
    
    if( !nargs() ) return(res)
    
    if( !length(obj) ) return()
    
    res <- t(sapply(obj, function(x){
       rdata <- c(user = x$owner$login)
       c(rdata, unlist(x[setdiff(fields, c('source', names(rdata)))])) 
    }))
    res <- cbind(source = source, res)
    rownames(res) <- res[, 'full_name']
    res[!duplicated(rownames(res)), , drop = FALSE]
}

read.repos <- function(dir, repos = NULL, file = NULL){
    
    if( is.null(file) ){
        src <- file.path(dir, 'repos/github')
        file <- file.path(src, 'REPOS') 
        if( !is.null(repos) ){
            src <- file.path(src, 'src/contrib/github.com', repos)
            file <- file.path(src, 'USER')
        }
    }
    read.table(file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
}

write.repos <- function(x, file, ...){
    write.table(x, file = file, sep = "\t", row.names = FALSE)
}

list.github.packages <- function(Rsince = Sys.Date(), skip = c('cran/.*', 'Bioconductor-mirror/.*', 'rforge/.*')){
    
    # init result
    res <- repo_matrix()
    
    # From R repositories on Github if a since date is passed
    if( !is.null(Rsince) ){
        message("* Checking Github R repositories pushed since ", Rsince, " ... ", appendLF = FALSE)
        r_repos <- gh_search_Rrepos(paste0("pushed:>=", Rsince))
        r_lang <- repo_matrix('Github', r_repos$content$items)
        message(sprintf("OK [%s repos]", nrow(r_lang)))
        res <- rbind(res, r_lang)
    }
    
    # From CRAN
    message("* Checking Github package on CRAN ... ", appendLF = FALSE)
    tmp <- file.path(tempdir(), 'packages.rds')
    if( !file.exists(tmp) ) download.file("http://cran.r-project.org/web/packages/packages.rds", dest = tmp, quiet = TRUE)
    CRAN <- readRDS(tmp)
    # select packages with GitHub BugReports
    m <- str_match(CRAN[, 'BugReports'], "://(github\\.com/([^/]+)/([^/]+))/issues")
    i_gh <- which(!is.na(m[, 1L]))
    m <- cbind(source = 'CRAN', m[i_gh, 3:4, drop = FALSE])
    colnames(m) <- c('source', 'user', 'name')
    message(sprintf("OK [%i/%i packages | %s users]", nrow(m), nrow(CRAN), length(unique(m[, 'user']))))
    res <- plyr::rbind.fill.matrix(res, m)
    
    # From drat forks
    message("* Checking drat forks on Github ... ", appendLF = FALSE)
    drat <- fetch_drat_forks()
    drat <- repo_matrix('drat', drat)
    # force language R for drat repos
    drat[, 'language'] <- 'R'
    drat_users <- unique(drat[, 'user'])
    message(sprintf("OK [%s users]", length(drat_users)))
    res <- rbind(res, drat)
    
    # remove duplicated entries
    rn <- res[, 'full_name']
    i <- is.na(rn) 
    res[i, 'full_name'] <- rn[i] <- file.path(res[i, 'user'], res[i, 'name'])
    rownames(res) <- rn
    res <- res[!duplicated(rownames(res)), , drop = FALSE]
    
    # skip some repos if requested
    if( !is.null(skip) ){
        i_skip <- unlist(lapply(skip, grep, rownames(res)))
        if( length(i_skip) ) res <- res[-i_skip, ]
        message(sprintf("* Skipping %i repos: %s", length(i_skip), str_out(skip, Inf)))
    }
    
    # total
    message(sprintf("* Number of unique Github users: %s", length(unique(res[, 'user']))))
    res
}

list.changed.packages <- function(dir){
    list.changed.files(dir, pattern.new = '^DESCRIPTION$', pattern.changed = "/((DESCRIPTION)|(src))$")
}

list.changed.user <- function(dir){
    list.changed.files(dir, pattern.new = '/USER$', pattern.changed = "/USER$", basename = TRUE)
}

list.changed.files <- function(dir, pattern.new, pattern.changed, basename = FALSE){
    library(git2r)
    .git <- discover_repository(dir)
    basedir <- dirname(.git)
    repo <- repository(.git)
    st <- status(repo)
    
    .list <- function(obj){
        lapply(obj, function(x){
                p <- file.path(basedir, x)
                list.files(p, recursive = TRUE, full.names = TRUE, pattern = pattern.new)
            })
    }
    res <- list(
        Changed = grep(pattern.changed, st$unstaged[names(st$unstaged) == 'modified'], value = TRUE)
        , New = if( !basename ).list(st$untracked) else grep(pattern.new, st$untracked, value = TRUE)
        , Deleted =  grep(pattern.changed, st$unstaged[names(st$unstaged) == 'deleted'], value = TRUE)
    )
    
    # clean up
    .clean <- function(x){
        x <- unlist(x, use.names = FALSE)
        if( !basename ) x <- dirname(x)
        x <- sub(sprintf("^%s/", basedir), '', x)
        unique(gsub("//", "/", x, fixed = TRUE))
    }
    res <- sapply(res, .clean, simplify = FALSE)
    
    attr(res, 'root') <- basedir
    res
}


pull_github_packages <- function(dir, sources = NULL){
    
    
    message("* Pullling Github package into ", dir)
    
    # look for a config file
    CONFIG <- list()
    if( file.exists(config_file <- file.path(dir, 'config')) ){
        CONFIG <- yaml::yaml.load_file(config_file)
    }
    PULL_TIME <- NULL
    on.exit({
        if( !is.null(PULL_TIME) ) CONFIG$pulled_at <- as.character(PULL_TIME) 
        cat(yaml::as.yaml(CONFIG), file = config_file)
    }, add = TRUE)
    ##
    
    result.ok <- function(res){
        PULL_TIME <<- Sys.time()
        res
    } 
    
    # fetch R repos pushed since last pulled date
    since <- CONFIG$pulled_at %||% Sys.Date()
    gh_packages <- list.github.packages(as.character(as.Date(since)))
    
    if( length(sources) )
        gh_packages <- gh_packages[gh_packages[, 'source'] %in% sources, , drop = FALSE]
    
    if( !nrow(gh_packages) ) return(result.ok(0L))
    
    # update each unpacked sub-directory
    on.exit(gh_context_save(), add = TRUE)
    library(plyr)
    
    # load previous index and save new on exit
    USER <- NULL
    if( file.exists(index_file <- file.path(dir, "REPOS")) ){
        message("* Loading previous repo index ... ", appendLF = FALSE)
        USER <- read.repos(file = index_file)                
        index <- as.matrix(USER[!duplicated(USER[, 'full_name']), , drop = FALSE])
        message(sprintf(" OK [%i refs | %i repos]", nrow(USER), nrow(index)))
        
        # add data from previous runs 
        i <- match(gh_packages[, 'full_name'], index[, 'full_name'])
        # pick Github data from index file for CRAN packages
        i_CRAN <- gh_packages[, 'source'] == 'CRAN'
        .f <- intersect(colnames(gh_packages), colnames(index))
        gh_packages[i_CRAN, .f] <- index[i[i_CRAN], .f]
        # add indexed date
        gh_packages <- cbind(gh_packages, indexed_at = index[i, 'indexed_at'])
        
        # limit check to repos that are either not R or with index date not up to date
        message("* Filtering up-to-date repos ... ", appendLF = FALSE)
        g <- as.data.frame(gh_packages, stringsAsFactors = FALSE)
        g[is.na(g)] <- ''
        to_update <- subset(g, (language == 'R' & pushed_at != indexed_at) | language != 'R' )
        message(sprintf(" OK [%i/%i changed]", nrow(to_update), nrow(gh_packages)))
        gh_packages <- as.matrix(to_update)
    }
    
    PACKS <- NULL
    if( file.exists(packages_file <- file.path(dir, 'packages.rds')) ){
        PACKS <- readRDS(packages_file)
        rownames(PACKS) <- GRAN_key(PACKS)
    }
    
    if( !nrow(gh_packages) ) return(result.ok(0L))
    
    USER_hash <- digest(USER)
    on.exit({
        message("* Checking changes in repo indexes ")
        user_path <- list.files(dir, recursive = TRUE, full.names = TRUE, pattern = "^USER$")
        uname <- basename(dirname(user_path))
        # remove users for which there are updated data 
        USER <- USER[!USER[, 'user'] %in% uname, , drop = FALSE]
        USER_NEW <- ldply(user_path, function(x) read.repos(file = x))
        USER <- rbind.fill(USER, USER_NEW)
        # drop old columns
        USER$refs <- NULL
        USER$commit_sha <- NULL
        # order by full_name
        USER <- USER[order(USER[, 'full_name']), , drop = FALSE]
        
        if( digest(USER) != USER_hash ){
            message("* Updating repo index ")
            # write file
            write.repos(USER, index_file)
            
        }
            
    }, add = TRUE)
    
    # update Github unpacked packages for each user
    sum <- summary(factor(gh_packages[!duplicated(paste(gh_packages[, 'source'], gh_packages[, 'user'])), 'source']))
    message("* Processing repos: ", str_out(sum, Inf, use.names = TRUE))
    with_rcurl({
        iuser <- split(seq(nrow(gh_packages)), gh_packages[, 'user'])
#        iuser <- head(iuser, 3L)
        USER_NEW <- ldply(iuser, function(i){
            data <- gh_packages[i, , drop = FALSE]
            user <- data[1L, 'user']
            message("* Checking ", user, appendLF = FALSE)
            update_github_user(user, dir = dir, repos = data[, 'name']
                                , repos_data = USER[USER[, 'user'] == user, , drop = FALSE]
                                , packages = PACKS[PACKS[, 'GithubUsername'] == user, , drop = FALSE])
        }, .id = NULL)
        
        # Update USER table if necessary
#        if( nrow(USER_NEW) ){
#            # remove data from updated repos
#            if( !is.null(USER) ){
#                USER <- USER[!USER[, 'full_name'] %in% unique(USER_NEW[, 'full_name']), , drop = FALSE]
#            }
#            # append new data
#            USER <- rbind.fill(USER, USER_NEW)
#        }
    })

    result.ok(nrow(USER_NEW))
}


write_GH_PACKAGES <- function(dir, fields = c('Date', GRAN.fields())){
    write_PACKAGES(dir, type = 'source', unpacked = TRUE, fields = fields, latestOnly = FALSE, subdirs = TRUE)
}

repopath2xpath <- function(x){
    gsub(".*/github.com/([^/]+)/([^/]+)/tarball/([^/]+)/.*", "\\1/\\2/\\3", x)
}

#' Updates GitHub Source Package Repository
#' 
#' @param src path to the directory that holds the package directory tree 
#' @importFrom tools md5sum
GRAN.update_github <- function(src, force = FALSE, fields = GRAN.fields(), update = FALSE, actions = c('changes', 'PACKAGES', 'index')
                                , clean = TRUE, test = FALSE, verbose = TRUE){
    
    library(plyr)
    do_commit <- TRUE
    if( test ){
        do_commit <- FALSE
        verbose <- TRUE
        clean <- FALSE
    }
    
    # dump messages if non-verbose
    if( !verbose ) message <- function(...) NULL
    
    src0 <- src
    base_github <- 'repos/github'
    outdir <- file.path(src0, base_github)
    src_github <- contrib.url(base_github, type = 'source')
    src <- file.path(src0, src_github)
    src_fullpath <- normalizePath(src)
    if( is_NA(update) ){
        return(src_fullpath)
    }
    message("* Updating GitHub source packages in ", src_fullpath)
    
    # match type of action to perform
    actions <- match.arg(actions, several.ok = TRUE)
    
    # pull updates for "all" GitHub R repos
    update.src <- NULL
    if( is.character(update) ){
        update.src <- update
        update <- TRUE
    }
    
    nUpdated <- 0
    if( update ){
        nUpdated <- pull_github_packages(src_fullpath, update.src)
    }
        
    ## CHECK CHANGES
    library(git2r)
    # initialise repository if not already in a git repo
    if( !in_repository(src) ){
        init(src0)
    }
    GH_repo <- repository(src0)
    #        status <- status(GH_repo) 
#    message("* Checking changes ", appendLF = FALSE)
#    CHANGES <- list.changed.packages(src)
#    message(sprintf("[%s]", paste(lengths(CHANGES), names(CHANGES), collapse = " | ")))
    #
    
    # update PACKAGES files
    if( 'PACKAGES' %in% actions ){
                
        ## add Github packages to source PACKAGES
        # write PACKAGES file from Github source directories 
        fields <- c('Date', fields)
        message("* Generating PACKAGES files in '", src, "'")
        
        message("* Loading updated PACKAGES ... ", appendLF = FALSE)
        d <- list.files(src_fullpath, recursive = TRUE, pattern = '^DESCRIPTION$', full.names = TRUE)
        PACKS_NEW <- do.call(rbind.fill.matrix, lapply(d, read.dcf))
        n_up <- nrow(PACKS_NEW) %||% 0L 
        messagef("OK [%i packages]", n_up)
        if( !n_up ) return()
        
        # enforce format
        PACKS_NEW <- add_dcf_field(PACKS_NEW, 'Package')
        PACKS_NEW <- add_dcf_field(PACKS_NEW, 'GithubRepo', PACKS_NEW[, 'Package'])
        
        # set rownames and check/remove  for duplicates
        rownames(PACKS_NEW) <- GRAN_key(PACKS_NEW)
        if( anyDuplicated(rownames(PACKS_NEW)) ){
            dups <- unique(rownames(PACKS_NEW)[duplicated(rownames(PACKS_NEW))])
            id <- which(rownames(PACKS_NEW) %in% dups)
            messagef("* Removing %i duplicated packages: %s", length(id), str_out(dups, 5, total = TRUE))
            PACKS_NEW <- PACKS_NEW[-id, , drop = FALSE] 
        } 
        
        # clean up
        bad <- which( is.na(PACKS_NEW[, 'Package']) | !nzchar(PACKS_NEW[, 'Package']) ) 
        if( length(bad) ){
            bad_nm <- unique(PACKS_NEW[bad, 'Package'])
            messagef("* Removing %i packages with invalid names: %s", length(bad), str_out(bad_nm, 5, total = TRUE))
            PACKS_NEW <- PACKS_NEW[-bad, , drop = FALSE]
        }
        
        # load previous PACKAGES
        message("* Loading old PACKAGES ... ", appendLF = FALSE)
        if( file.exists(packages_file <- file.path(src, 'packages.rds')) ){
            PACKS <- readRDS(packages_file)
            rownames(PACKS) <- GRAN_key(PACKS)
            PACKS <- PACKS[!rownames(PACKS) %in% rownames(PACKS)[duplicated(rownames(PACKS))], , drop = FALSE]
            messagef("OK [%i packages]", nrow(PACKS))
            
        }else{
            PACKS <- rbind.fill.matrix(matrix(NA, 0, 0), PACKS_NEW[0, , drop = FALSE])
            message("none")
        }
        
        # align package matrices
        i <- match(rownames(PACKS_NEW), rownames(PACKS), nomatch = 0L)
        PACKS <- rbind.fill.matrix(PACKS, PACKS_NEW[0L, , drop = FALSE])
        rownames(PACKS) <- GRAN_key(PACKS)
        PACKS_NEW <- rbind.fill.matrix(PACKS_NEW, PACKS[0L, , drop = FALSE])
        PACKS_NEW <- PACKS_NEW[, colnames(PACKS), drop = FALSE]
        rownames(PACKS_NEW) <- GRAN_key(PACKS_NEW)
        
        # add new packages
        new <- which(!i)
        if( length(new) ){
            messagef("  ** Adding %i packages to index", length(new))
            PACKS <- rbind(PACKS, PACKS_NEW[new, colnames(PACKS), drop = FALSE])
        }
        rownames(PACKS) <- GRAN_key(PACKS)
        
        # update other packages
        messagef("  ** Updating %s packages ", sum(i>0))
        PACKS[i, colnames(PACKS)] <- PACKS_NEW[i>0, colnames(PACKS), drop = FALSE]
        
        # clean up
        PACKS <- PACKS[!is.na(PACKS[, 'Package']), , drop = FALSE]
        PACKS <- add_dcf_field(PACKS, 'Path', file.path('github.com', rownames(PACKS)), force = TRUE)
        
        # TODO: remove deleted packages
        to_delete <- NULL
#        if( length(del <- which(rownames(PACKS) %in% to_delete)) ){
#            message(sprintf("  ** Removing %i packages from index", length(del)))
#            PACKS <- PACKS[-del, , drop = FALSE]
#        }
        
        # write files
        message(sprintf("  ** Updating file PACKAGES files [%i]", nrow(PACKS)))
#        write_PACKAGES_files(PACKS, src)
        if( !test ) saveRDS(PACKS, file.path(src, 'packages.rds'))
            
        # clean pulled data
        if( clean ){
            unlink( file.path(src_fullpath, 'github.com'), recursive = TRUE)
        }
        
#        }else if( force ){
#            message("  ** Writing PACKAGES from unpacked directories")
##            write_GH_PACKAGES(src, fields = fields)
#        }
        
    }
    
    # generate index file
    if( 'index' %in% actions ){
        message("* Generating index file:")
        write_PACKAGES_index(outdir, title = 'GRAN: Github R Archive Network')
    }
    
    # return output directory to calling script in non-interactive mode
    invisible(PACKS)
}
