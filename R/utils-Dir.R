#' @title Remove directory
#' @description Remove all files from a given directory
#'
#' @param dir.name the path leading to the directory
#' @param test should the user be asked whether he really wants to remove the directory
#' @param trace should the execution of the function be traced
#' 
#' @examples 
#' \dontrun{
#'    dir.create("MyNewDirButils314")
#'    cleanDir("MyNewDirButils314", test = FALSE)
#' }
#' 
cleanDir <- function(dir.name,test=TRUE, trace = TRUE){
  
  dir.name_split <- strsplit(dir.name,split="/")[[1]]
  if(length(dir.name_split)>1){
    path <- paste(dir.name_split[-length(dir.name_split)],collapse="/")
    dir.name_1 <- dir.name_split[length(dir.name_split)]
  }else{
    path <- "."
    dir.name_1 <- dir.name_split
  }
  
  list_files <- list.files(dir.name,recursive = TRUE)
  n.files <- length(list_files)
  if(dir.name_1 %in% list.files(path)){
    if(test==TRUE){
      test <- readline(paste("should the directory \'",dir.name,"\' be removed ? (1/0) : \n",
                             "(",n.files," files )",sep=""))
    }else{
      test <- 1
    }
    
    if(test==1){
      unlink(dir.name,recursive=TRUE)
      if(trace){cat("directory ",dir.name," removed \n",sep = "")}
    }
  }else if(trace){cat("directory not in the current working directory \n")}
  
  return(invisible(list_files))
}


#' @title Find GitHub directory
#' 
#' @description Localise the github directory on linux or windows OS.
#'
#' @param user the name corresponding to the cession.
#' 
#' @keywords function package
#' 
#' @examples 
#' pathGitHub()
#' 
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
    dir <- file.path("/home",user,"GitHub")
  }else if(Sys.info()["sysname"] == "Windows"){
    dir <- file.path("C:/Users",user,"Documents","GitHub")
  }else{
    stop("only implemented for linux and windows \n")
  }
  
  if(dir.exists(dir)){
    return(dir) 
  }else{
    stop("dir.gitHub: no GitHub directory found \n")
  }
  
}
