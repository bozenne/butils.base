### pathGitHub.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: nov 27 2018 (10:24) 
## Version: 
## Last-Updated: May 12 2024 (11:50) 
##           By: Brice Ozenne
##     Update #: 6
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## * pathGitHub (documentation)
#' @title Find GitHub directory
#' @description Localise the github directory on linux or windows OS.
#'
#' @param user the name corresponding to the cession.
#' 
#' @examples
#' \dontrun{
#' pathGitHub()
#' }
#'

## * pathGitHub (code)
#' @export
pathGitHub <- function(user = NULL){
  
  if(is.null(user)){
    if(Sys.info()["user"] != "unknown"){
      user <- Sys.info()["user"]
    }else if(Sys.info()["login"] != "unknown"){
      user <- Sys.info()["login"]
    }else stop("dir.gitHub: please specify the user \n")
  }
      
  if(Sys.info()["sysname"] == "Linux"){
      dir <- c(file.path("/home",user,"GitHub"),
               file.path("/home",user,"Documents","GitHub"))
  }else if(Sys.info()["sysname"] == "Windows"){
    dir <- file.path("C:/Users",user,"Documents","GitHub")
  }else{
    stop("only implemented for linux and windows \n")
  }
  
    if(any(dir.exists(dir))){
        return(dir[dir.exists(dir)][1]) 
    }else{
        stop("dir.gitHub: no GitHub directory found \n")
    }
  
}


######################################################################
### pathGitHub.R ends here
