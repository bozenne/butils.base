## * buildPackage
#' @title Integrative function for building packages
#' @description  Convenient way to compile, test or install a package
#' 
#' @param package the name of the package
#' @param version the version of the package
#' @param path the position of the directory containing the package
#' @param register.c.routine should the function \code{package_native_routine_registration_skeleton} be used to register C++ routines.
#' @param compile.attributes should the function \code{compile.attributes} be used to enerates the bindings required to call C++ functions from R for functions adorned with the Rcpp::export attribute.
#' @param update.collate should the collate field of the DESCRIPTION file be updated according the content of the R directory.
#' @param update.date should the date field of the DESCRIPTION file be updated with the date of the date.
#' @param roxygenise should the documentation be generated using \code{roxygenise}
#' @param build should the package be build
#' @param options.build additional options used to build the package
#' @param untar should the package be unzipped
#' @param check should the CRAN test be applied to the package
#' @param options.check additional options used to check the package
#' @param install should the package be installed on the computer.
#' @param options.install additional options used to install the package
#' @param trace How the execution of the function should be traced. If 2, also display the execution of compile.attributes.
#' @param clear.existing should the existing directories containing the tests, the unziped package or the archive file be removed.
#' @param clear.r.check Should the directory where the package has been tested be removed at the end of the execution.
#' @param clear.install Should the directory where the package has been unzipped be removed at the end of the execution.
#' 
#' @details Reminder [not related to the function]
#' exportPattern("^[^\\.]") : exporte toutes les fonctions ne commencant pas par un point
#' exportPattern("^[[:alpha:]]+") : exporte toutes les fonctions commencant par une lettre
#' 
#' @keywords function package
#' 
#' @examples 
#' buildPackage("butils")
#' 
#' @export
buildPackage <- function(package, version = NULL, path = pathGitHub(), 
                         register.c.routine = FALSE, compile.attributes = TRUE, update.collate = FALSE, update.date = TRUE, roxygenise = TRUE,
                         build = TRUE, options.build = NULL, 
                         untar = TRUE, 
                         check = FALSE, options.check = NULL, 
                         install = TRUE, options.install = "--build", 
                         trace = 2,  clear.existing = TRUE, clear.r.check = TRUE, clear.install = TRUE){
  
  if(!is.null(path)){
    oldWD <- getwd()
    setwd(path)
    on.exit(setwd(oldWD))
  }
  
  if(is.null(version)){
    version <- readDescription(package, path = ".", field = "Version")
  }
  
  packageVersion <- paste(package, version, sep = "_")
  path.Wpackage <- package
  path.WpackageVersion <- packageVersion
  test.src <- try(validPath(file.path(path.Wpackage, "src"), type = "dir", method = "buildPackage"), silent = TRUE)
  
  if(register.c.routine && identical(test.src,TRUE)){
    if(trace>=2)cat(">> add Native Routine Registration \n")
    txtRegistration <- capture.output(tools::package_native_routine_registration_skeleton(dir = path.Wpackage))
    txtRegistration <- paste(txtRegistration,collapse="\n")
    con <- file(file.path(path.Wpackage,"src","declareRoutines.c"), "wb") 
    writeBin(txtRegistration, con) 
    close(con) 
  }
  
  if(compile.attributes && identical(test.src,TRUE)){
    if(trace>=2)cat(">> compile.attributes \n")
    Rcpp::compileAttributes(pkgdir = path.Wpackage, verbose = (trace>=3))
  } 
  if(update.collate){
    writeCollate(package, path = path, trace = (trace>=2))
  }
  if(update.date){
    writeDate(package, path = path, trace = (trace>=2))
  }
  # crlf2lf("testCpp")
  # pdf2qpdf("MRIaggr")
  
  if(clear.existing){ 
    if(trace>=2)cat(">> clearDir \n")
    cleanDir(path.WpackageVersion, test = FALSE, trace = (trace>=2))
    if(trace>=2)cat("\n")
  }
  if(roxygenise){
    if(trace>=2)cat(">> roxygenise \n")
    roxygen2::roxygenise(package)
    
    if(trace>=2)cat("\n")
  } 
  
  if(build){
    if(trace>=2)cat(">> build \n")
    system(paste0("R CMD build ",options.build," ",package))
    if(trace>=2)cat("\n")
  } 
  
  if(untar){
    if(trace>=2)cat(">> untar \n")
    untar(paste0(packageVersion,".tar.gz"), exdir = paste0(packageVersion))  
  }
  
  if(check){
    if(trace>=2)cat(">> CRAN check \n")
    system(paste0("R CMD check ",options.check," ",packageVersion,"/",package))
    if(clear.r.check){cleanDir(paste0(path.Wpackage,".Rcheck"), test = FALSE)}
    if(trace>=2)cat("\n")
  }
  
  if(install){
    if(trace>=2)cat(">> install package \n")
    system(paste0("R CMD INSTALL ",options.install," ",packageVersion,"/",package))
    if(clear.install){cleanDir(path.WpackageVersion, test = FALSE, trace = (trace>=2))}
    if(trace>=2)cat("\n")
  }
  
  
}






## * crlf2lf
#' @title Normalize .cpp files
#' @description Convert .cpp files with CRLF line ending in LF line ending
#'
#' @param dir.name the path to the directory containing the cpp files
#'
#' @references  http://lists.r-forge.r-project.org/pipermail/rcpp-devel/2012-April/003731.html Dirk Eddelbuettel edd at debian.org (Mon Apr 23 18:53:37 CEST 2012)
crlf2lf <- function(dir.name){ 
  cat("Convert file ending from CRLF to LF (dir : ",dir.name,") \n")
  
  if("src" %in% list.files(dir.name) == FALSE){
    cat("\'src\' directory not found \n")
    return(invisible(NULL))
  }
  
  file_cpp <- tools::list_files_with_exts(paste(dir.name,"/src",sep=""), exts="cpp", full.names = FALSE)
  
  if(length(file_cpp)==0){
    cat("no \'.cpp\' files founded in the \'src\' directory \n")
    return(invisible(NULL))
  }
  
  n.files <- length(file_cpp)
  cat(n.files,"files founded \n")
  test <- 0
  
  for(iter_file in file_cpp){
    if(test==0){
      valid <- FALSE
      while(valid==FALSE){
        test <- readline(paste("should the file \'",iter_file,"\' be converted ? (1/0) : ",sep=""))
        test <- as.numeric(test)
        
        if(test %% 1 != 0 || test>n.files){
          test <- cat("incorrect answer. Answer should be 1 (yes), 0 (no) or -1 (exit) \n")
        }else{valid <- TRUE}
      }
    }
    
    if(test>=1){
      con <- file(paste(dir.name,"/src/",iter_file,sep=""), "rb") 
      bin <- readBin(con, raw(), 100000) 
      bin <- bin[ which(bin != "0d") ] 
      close(con) 
      
      Sys.sleep(1) 
      
      con <- file(paste(dir.name,"/src/",iter_file,sep=""), "wb") 
      writeBin(bin, con) 
      close(con) 
      cat("done \n") 
      
      test <- test-1
    }
    if(test<0){
      cat("exit \n") 
      break}
  }
}





