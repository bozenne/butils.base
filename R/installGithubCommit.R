#' @title Install commit
#' 
#' @description Install a specific commit of a package, possibly under a temporary name
#'
#' @param repo Repository address in the format username/repo
#' @param ref Desired git reference. See the documentation of \code{devtools::install_github}.
#' @param subdir Subdirectory within repo that contains the R package.  See the documentation of \code{devtools::install_github}.
#' @param host GitHub API host to use. See the documentation of \code{devtools::install_github}.
#' @param temporary Should the package be installed under a newname, i.e. adding \code{Tempo} to the name of the package
#' @param force Not sure what it is !!!
#' @param quiet Not sure what it is !!!
#' @param ... additional arguments to be passed to \code{devtools::install_packages} or \code{devtools:::install}
#'  
#' @keywords function github package
#' @examples 
#' \dontrun{
#' commit <- readGithubCommit("bozenne/BuyseTest", seq.commit = 1)[1,"commits"]
#' installGithubCommit("bozenne/BuyseTest", ref = commit)
#' }
#' 
#' @export
installGithubCommit <- function (repo, ref = "master", subdir = NULL, host = "https://api.github.com",
                                  temporary = TRUE, force = FALSE, quiet = FALSE, ...){
  remote <- lapply(repo, github_remote, ref = ref, host = host,
                   username = NULL, subdir = subdir)[[1]]
  
  stopifnot(is.remote(remote))
  remote_sha <- remote_sha(remote)
  package_name <- remote_package_name(remote)
  local_sha <- local_sha(package_name)
  if (!isTRUE(force) && !different_sha(remote_sha = remote_sha, 
                                       local_sha = local_sha)) {
    if (!quiet) {
      message("Skipping install of '", package_name, "' from a ", 
              sub("_remote", "", class(remote)[1L]), " remote,", 
              " the SHA1 (", substr(remote_sha, 1L, 8L), ") has not changed since last install.\n", 
              "  Use `force = TRUE` to force installation")
    }
    return(invisible(FALSE))
  }
  if (is_windows && inherits(remote, "cran_remote")) {
    install_packages(package_name, repos = remote$repos, 
                     type = remote$pkg_type, ..., quiet = quiet)
    return(invisible(TRUE))
  }
  bundle <- remote_download(remote, quiet = quiet)
  on.exit(unlink(bundle), add = TRUE)
  source <- source_pkg(bundle, subdir = remote$subdir)
  on.exit(unlink(source, recursive = TRUE), add = TRUE)
  metadata <- remote_metadata(remote, bundle, source)
  list.files(source)
  
  if(temporary){
    cat("* Installation as a temporary package \n")
    
    # description
    file.description <- readLines(file.path(source,"DESCRIPTION"))
    indexLine <- grep("Package:",file.description)
    packageName <- gsub("Package: ", replacement = "", x = file.description[indexLine])
    file.description[1] <- paste0("Package: ", packageName,"Tempo")
    
    con <- file(file.path(source,"DESCRIPTION")) 
    writeLines(text = file.description, con = con) 
    close(con) 
    
    # namespace
    file.namespace <- readLines(file.path(source,"NAMESPACE"))
    indexLine <- grep("useDynLib",file.namespace)
    if(length(indexLine)!=0){ # cpp links to R
      file.namespace[indexLine] <-  gsub(pattern = paste0("useDynLib(",packageName,")"), 
                                           replacement = paste0("useDynLib(",packageName,"Tempo)"),
                                           x = file.namespace[indexLine], fixed = TRUE)
      
      con <- file(file.path(source,"NAMESPACE")) 
      writeLines(text = file.namespace, con = con) 
      close(con) 
      
      Rcpp::compileAttributes(source)
    }
    
  }

  install(source, ..., quiet = quiet, metadata = metadata)
}



