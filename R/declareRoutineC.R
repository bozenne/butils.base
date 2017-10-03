## * declareRoutineC
#' @title Help function to fill the file declareRoutines.c [should be removed ?]
#' @description Help function to fill the file declareRoutines.c
#' 
#' @param fct.name A character string containig the name of the function
#'
#' @examples 
#' \dontrun{
#' library(devtools)
#' library(tools)
#' path2package <- file.path(pathGitHub(),"lavaReduce")
#' setwd(path2package)
#' Rcpp::compileAttributes(pkgdir = path2package)
#' x <- capture.output(package_native_routine_registration_skeleton(dir = ".",,,FALSE))
#' cat(paste(x,collapse = "\n"))
#' 
#' 
#' }
#'
#' @export
declareRoutineC <- function(fct.name){
    fct <- eval(parse(text=fct.name))
  
    ls.args <- as.list(args(fct))
    package <- environmentName(environment(fct))
  
    allArgs <- setdiff(names(ls.args),"")
    n.args <- length(allArgs)
  
    line1 <- paste0("extern SEXP ",package,"_",fct.name,"(",paste(rep("SEXP",n.args),collapse=","),");")
    line2 <- paste0("{\"",package,"_",fct.name,"\", (FL_FUNC) &",package,"_",fct.name,n.args,"},")
    return(list(extern=line1,
                CallEntries=line2))
}


## * countCppArgs
#' @title Help function to fill the file declareRoutines.c
#' @description Help function to fill the file declareRoutines.c
#' 
#' @param fct.name A character string containig the name of the function
#'
#' @examples 
#' \dontrun{
#' library(devtools)
#' library(tools)
#' path2package <- file.path(pathGitHub(),"lavaReduce")
#' setwd(path2package)
#' Rcpp::compileAttributes(pkgdir = path2package)
#' x <- capture.output(package_native_routine_registration_skeleton(dir = ".",,,FALSE))
#' cat(paste(x,collapse = "\n"))
#' 
#' 
#' }
#'
#' @export
countCppArgs <- function(fct.name){
  fct <- eval(parse(text=fct.name))
  
  ls.args <- as.list(args(fct))
  package <- environmentName(environment(fct))
  
  allArgs <- setdiff(names(ls.args),"")
  n.args <- length(allArgs)
  
  line1 <- paste0("extern SEXP ",package,"_",fct.name,"(",paste(rep("SEXP",n.args),collapse=","),");")
  line2 <- paste0("{\"",package,"_",fct.name,"\", (FL_FUNC) &",package,"_",fct.name,n.args,"},")
  return(list(extern=line1,
              CallEntries=line2))
}
