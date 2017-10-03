#' @title Source a package directory
#' 
#' @description Source all the R and Cpp file contain in a package
#' 
#' @param name the name of the package
#' @param path the path to the directory containing the package
#' @param r.package should the related R package be loaded
#' @param r.code should all the .R be sourced
#' @param r.order.collate should the R files be sourced in the order indicate by collate
#' @param onAttach source the .onAttach function if it is present in the current environment
#' @param onLoad source the .onLoad function if it is present in the current environment
#' @param c.code should all the .cpp file be source (using Rcpp::sourceCpp)
#' @param rebuild Force a rebuild of the shared library (from Rcpp:::sourceCpp).
#' @param warning should a warning be displayed if some of the R files are not sourced
#' 
#' @seealso \code{\link{pathGitHub}}
#'
#' @examples 
#' package.source("butils")
#' package.source("riskRegression")
#' @export
sourcePackage <- function(name, path = pathGitHub(), 
                          r.package = TRUE,
                          r.code = TRUE, r.order.collate = FALSE, onAttach = TRUE, onLoad = TRUE,
                          c.code = FALSE, rebuild = FALSE,
                          warning = TRUE){
  
  validPath(path, type = "dir", method = "package.source")
  validPath(file.path(path, name), type = "dir", method = "package.source")
  if(r.package){
    
    if(identical(r.package,TRUE)){r.package <- c("Imports","Depends","Suggests")}
    
    validCharacter(r.package, valid.length = NULL, valid.values = c("Imports","Depends","Suggests"))
    
    packageToLoad <- NULL
    if("Imports" %in% r.package){
      packageToLoad <- c(packageToLoad,
                         readDescription(name, path = path, field = "Imports", rm.comma = TRUE, rm.blanck = TRUE)
      )
    }
    if("Depends" %in% r.package){
      packageToLoad <- c(packageToLoad,
                         readDescription(name, path = path, field = "Depends", rm.comma = TRUE, rm.blanck = TRUE)
      )
    }
    if("Suggests" %in% r.package){
      packageToLoad <- c(packageToLoad,
                         readDescription(name, path = path, field = "Suggests", rm.comma = TRUE, rm.blanck = TRUE)
      )
    }
    
    ## remove version in parenthesis
    packageToLoad <- gsub( " *\\(.*?\\) *", "", packageToLoad)
    
    ## remove space
    packageToLoad <- gsub( "^\\s+|\\s+$", "", packageToLoad)
    
    ##
    packageToLoad <- setdiff(packageToLoad, "R")
    
    lapply(packageToLoad, require, character.only = TRUE)
  }
  
  if(r.code){
    validPath(file.path(path, name, "R"), type = "dir", method = "package.source")
    
    ## find files
    fileNames <- setdiff(list.files(file.path(path, name, "R")),
                         "RcppExports.R")
    fileExts <- tools::file_path_sans_ext(fileNames)
    indexC <- grep("R", x = tools::file_ext(fileNames), 
                   fixed = FALSE)
    fileNames <- fileNames[indexC]
      
    if(r.order.collate){ ## reorder according DESCRIPTION
      
      if(file.exists(file.path(path,name,"DESCRIPTION")) == FALSE){
        warning("package.source: no DESCRIPTION file founded \n",
                "set \'r.order.collate\' to FALSE to source all the files that are present in the directory R \n")  
      }
      
      filesR.description <- readDescription(name, path = path, field = "Collate", rm.blanck = TRUE, rm.comma = TRUE)
      filesR.description <- gsub("'|\"","",filesR.description)
      
      test.missing <- is.na(match(fileNames, filesR.description))
      if(warning && any(test.missing)){
        warning("package.source: did not find files: ",paste(fileNames[which(test.missing)], collapse = " ")," in DESCRIPTION \n",
                "set \'r.order.collate\' to FALSE to source all the files that are present in the directory R \n")  
      }
      fileNames <- filesR.description[filesR.description %in% fileNames]
    }
        
    ## mimic .onload
    .onAttach <- try(get(".onAttach", envir = globalenv()), silent = TRUE)
    if(onAttach && class(.onAttach) == "function"){
        ..onAttach <- .onAttach
        ..onAttach_envir <- environment(fun = .onAttach)
        assign(".onAttach", value = NULL, envir = ..onAttach_envir)
        ..onAttach_test <- TRUE
    }else{
        ..onAttach_test <- FALSE
    }
    
    .onLoad <- try(get(".onLoad", envir = globalenv()), silent = TRUE)
    if(onLoad && class(.onLoad) == "function"){
        ..onLoad <- .onLoad
        ..onLoad_envir <- environment(fun = .onLoad)
        assign(".onLoad", value = NULL, envir = ..onLoad_envir)
        ..onLoad_test <- TRUE
    }else{
        ..onLoad_test <- FALSE
    }

    
    
    ## SOURCE
    res <- lapply(file.path(path,name,"R",fileNames), source)
    
    .onAttach <- try(get(".onAttach", envir = globalenv()), silent = TRUE)
    if(onAttach){
        if(class(.onAttach) == "function"){
            .onAttach()
        }
        if(..onAttach_test){        
            try(assign(".onAttach", value = ..onAttach, envir = ..onAttach_envir), silent = TRUE)
        }
    }
    
    
    .onLoad <- try(get(".onLoad", envir = globalenv()), silent = TRUE)
    if(onLoad){
        if(class(.onLoad) == "function"){
            .onLoad()
        }
        if(..onLoad_test){
            try(assign(".onLoad", value = ..onLoad, envir = ..onLoad_envir), silent = TRUE)
        }
    }
  }
    
  test.src <- try(validPath(file.path(path, name, "src"), type = "dir", method = "package.source"), silent = TRUE)
  if(c.code && identical(test.src,TRUE)){
    
    fileNames <- list.files(file.path(path, name, "src"))
    if(length(fileNames)>0){
    fileExts <- tools::file_path_sans_ext(fileNames)
    indexC <- grep("cpp", x = tools::file_ext(fileNames), 
                   fixed = FALSE)
    lapply(file.path(path,name,"src",setdiff(fileNames[indexC],"RcppExports.cpp")), 
           Rcpp::sourceCpp, 
           rebuild = rebuild)
    }
  }
  
  return(invisible(TRUE))
  
}



