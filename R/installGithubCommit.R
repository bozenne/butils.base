
## * installGithubCommit (Documentation)
#' @title Install commit
#' @name installGithubCommit
#' 
#' @description Install a specific commit of a package, possibly under a temporary name.
#'
#' @param repo Repository address in the format username/repo. See the documentation of \code{devtools::install_github}.
#' @param ref Desired git reference. See the documentation of \code{devtools::install_github}.
#' @param temporary Should the package be installed under a newname, i.e. adding \code{Tempo} to the name of the package?
#' @param trace should the execution of the function be trace?
#' @param ... additional arguments to be passed to \code{devtools::install}
#'
#' @details This function is essentially a copy of the function \code{install_remote} from the package remotes,
#' with one additional argument, \code{temporary}, that enable to install the package under an other name.
#' This can be convinient to track a change between package versions.
#' 
#' @keywords function github package
#' @examples 
#' \dontrun{
#' commit <- readGithubCommit("bozenne/BuyseTest", seq.commit = 1)[1,"commits"]
#' installGithubCommit("bozenne/BuyseTest", ref = commit)
#' }

## * installGithubCommit (code)
#' @rdname installGithubCommit
#' @export
installGithubCommit <- function (repo, ref = "master", temporary = TRUE, trace = TRUE, ...){

    ## inspired from
    ## - remotes::install_github
    ## - remotes:::install_remotes
    ## - remotes:::install_remote

    ## ** extract information (remotes:::install_remote)
    host <- "https://api.github.com"
    out <- remotes_parse_git_repo(repo)
    username <- out$username
    package <- out$repo

    ## check that the commit exists or transform master into the last commit
    commit <- remotes_github_commit(username = username, repo = package, host = host, ref = ref)
    
    ## ** download package (remotes:::remote_download.github_remote)
    if (trace) {
        cat("* Downloading GitHub repo ", username, "/", package, "@", commit,"\n", sep = "")
    }
    
    file.tempo <- tempfile(fileext = paste0(".tar.gz"))
    url.package <- remotes_build_url(host, "repos", username, package)
    url.tarball.package <- paste0(url.package, "/tarball/", utils::URLencode(ref, reserved = TRUE))
    dir.download.tar.gz <- remotes_download(file.tempo, url.tarball.package, auth_token = NULL)
    on.exit(unlink(dir.download.tar.gz), add = TRUE)

    ## ** unzip file
    if (trace) {
        cat("* Unzip \n")
    }
    dir.download <- remotes_source_pkg(dir.download.tar.gz, subdir = NULL)
    on.exit(unlink(dir.download, recursive = TRUE), add = TRUE)
    
    ## ** do stuff
    ## remotes:::update_submodules(dir.download, trace)
    ## remotes:::add_metadata(dir.download,
                           ## remotes:::remote_metadata(remote, dir.download.tar.gz, dir.download, commit))
    ## remotes:::clear_description_md5(dir.download)

    ## ** change package name
    if(temporary){
        if (trace) {
            cat("* Transform package ",package," into package ",paste0(package,"Tempo")," \n", sep = "")
        }
        
        ## DESCRIPTION file
        file.description <- readLines(file.path(dir.download,"DESCRIPTION"))
        indexLine <- grep("Package:",file.description)
        file.description[1] <- paste0("Package: ", package,"Tempo")
    
        con <- file(file.path(dir.download,"DESCRIPTION")) 
        writeLines(text = file.description, con = con) 
        close(con) 
    
        ## NAMESPACE file
        file.namespace <- readLines(file.path(dir.download,"NAMESPACE"))
        indexLine <- grep("useDynLib",file.namespace)
        if(length(indexLine)!=0){ # cpp links to R
            file.namespace[indexLine] <-  gsub(pattern = paste0("useDynLib(",package,")"), 
                                               replacement = paste0("useDynLib(",package,"Tempo)"),
                                               x = file.namespace[indexLine], fixed = TRUE)
      
            con <- file(file.path(dir.download,"NAMESPACE")) 
            writeLines(text = file.namespace, con = con) 
            close(con) 
      
            Rcpp::compileAttributes(dir.download)
        }
    }
    
    ## ** install package (remotes:::install_remote)
    if (trace) {
        cat("* Install package \n", sep = "")
    }
    devtools::install(dir.download, quiet = trace, ...)
}



