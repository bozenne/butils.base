## * sourcePackage (documentation)
#' @title Source a package directory
#' @description Source all the R and Cpp file contain in a package
#' 
#' @param name [character] The name of the package.
#' @param path [character] The path to the directory containing the package.
#' @param trace [logical]  Should the execution of the R and cpp files be traced?
#' @param r.package [logical] Should the related R package be loaded.
#' @param field [character vector] which type of dependency should be loaded?
#' Only active if \code{r.package} it \code{TRUE}.
#' @param r.code [logical] Should all the .R be sourced.
#' @param r.order.collate [logical] Should the R files be sourced in the order indicate by collate.
#' @param onAttach [logical] Source the .onAttach function if it is present in the current environment.
#' @param onLoad [logical] Source the .onLoad function if it is present in the current environment.
#' @param c.code [logical] Should all the .cpp file be source (using Rcpp::sourceCpp).
#' @param rebuild [logical] Force a rebuild of the shared library (from Rcpp:::sourceCpp).
#' @param warning [logical] Should a warning be displayed if some of the R files are not sourced.
#' 
#' @seealso \code{\link{pathGitHub}}
#'
#' @examples
#' \dontrun{
#' sourcePackage("butils")
#' sourcePackage("riskRegression")
#' }
#'

## * sourcePackage (code)
#' @export
sourcePackage <- function(name, path = pathGitHub(), trace = FALSE,
                          r.package = TRUE, field = c("Imports","Depends"),
                          r.code = TRUE, r.order.collate = FALSE, onAttach = TRUE, onLoad = TRUE,
                          c.code = FALSE, rebuild = FALSE,
                          warning = TRUE){
  

    if(r.package){
        packageToLoad   <- dependencyPackage(name = name, path = path, field = field)   
        lapply(packageToLoad, require, character.only = TRUE)
    }else{
        validPath(path, type = "dir", method = "package.source")
        validPath(file.path(path, name), type = "dir", method = "package.source")
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

    
    
    ## ** source R code
    if(trace == 0){
        res <- lapply(file.path(path,name,"R",fileNames), source)
    }else if(trace==1){
        cat("R code: \n")
        res <- pbapply::pblapply(file.path(path,name,"R",fileNames), source)
    }else{
        cat("R code: \n")
        res <- lapply(file.path(path,name,"R",fileNames), function(iFile){
            cat("  > ",iFile," ",sep = "")
            source(iFile)
            cat("\n")
        })
        cat("\n")
    }

    ## ** run .onAttach function
    .onAttach <- try(get(".onAttach", envir = globalenv()), silent = TRUE)
    if(onAttach){
        if(class(.onAttach) == "function"){
            .onAttach()
        }
        if(..onAttach_test){        
            try(assign(".onAttach", value = ..onAttach, envir = ..onAttach_envir), silent = TRUE)
        }
    }
    
    ## ** run .onLoad function
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

    ## ** source C++ code
    test.src <- try(validPath(file.path(path, name, "src"), type = "dir", method = "package.source"), silent = TRUE)
    if(c.code && identical(test.src,TRUE)){
    
        fileNames <- list.files(file.path(path, name, "src"))
        if(length(fileNames)>0){
            fileExts <- tools::file_path_sans_ext(fileNames)
            indexC <- grep("cpp", x = tools::file_ext(fileNames), 
                           fixed = FALSE)
            if(trace==0){
                lapply(file.path(path,name,"src",setdiff(fileNames[indexC],"RcppExports.cpp")), 
                       Rcpp::sourceCpp, 
                       rebuild = rebuild) 
            }else if(trace == 1){
                cat("C++ code: \n")
                pbapply::pblapply(file.path(path,name,"src",setdiff(fileNames[indexC],"RcppExports.cpp")), 
                                  Rcpp::sourceCpp, 
                                  rebuild = rebuild)
            }else{
                cat("C++ code: \n")
                lapply(file.path(path,name,"src",setdiff(fileNames[indexC],"RcppExports.cpp")),
                       function(iFile){
                           cat("  > ",iFile," ",sep = "")
                           Rcpp::sourceCpp(iFile, rebuild = rebuild)
                           cat("\n")
                       })
            }
        }
    }
  
  return(invisible(TRUE))
  
}



