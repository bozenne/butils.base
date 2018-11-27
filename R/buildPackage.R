## * buildPackage (documentation)
#' @title Integrative function for building packages
#' @description  Convenient way to compile, test or install a package
#' @name buildPackage
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
#' \dontrun{
#' buildPackage("butils")
#' }

## * buildPackage (code)
#' @rdname buildPackage
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
    txtRegistration <- utils::capture.output(tools::package_native_routine_registration_skeleton(dir = path.Wpackage))
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

## * Associated functions
## ** readDescription
#' @title Load description file
#' @description Extract the description file of a package
#' 
#' @param package the name of the package
#' @param path the path to the directory containing the package
#' @param field should a specific field of the description file be extracted
#' @param rm.blanck should the initial blanck be removed?
#' @param rm.comma should any comma be removed?
#'
#' @examples 
#' \dontrun{
#' readDescription <- butils:::readDescription
#' readDescription("butils", path = pathGitHub())
#' readDescription("butils", path = pathGitHub(), field = "Collate")
#' readDescription("butils", path = pathGitHub(), field = "Collate", rm.blanck = FALSE)
#' 
#' readDescription("butils", path = pathGitHub(), field = "Version")
#' readDescription("butils", path = pathGitHub(), field = "Imports")
#' 
#' readDescription("riskRegression", path = pathGitHub(), field = "Collate")
#' readDescription("lava.penalty", path = pathGitHub(), field = "Suggests")
#' }
readDescription <- function(package, path, field = NULL,
                             rm.blanck = TRUE, rm.comma = TRUE){
  
  validPath(path, type = "dir", method = "readDescription")
  validPath(file.path(path, package), type = "dir", method = "readDescription")
  
  file.description <- readLines(file.path(path,package,"DESCRIPTION"))
  
  if(is.null(field)){return(file.description)}
  
  # select field
  if(length(grep(field,file.description))==0){return(NULL)}
  indexLine <- grep(field,file.description)
  indexPP <- grep(":",file.description,fixed=TRUE)
  if(indexLine == utils::tail(indexPP, 1)){ # if field is the last one in the description file go to the end
    indexLine_end <-  length(file.description)
  }else{ # else go just before the next field
    indexLine_end <-  min(indexPP[indexPP>indexLine])-1
  }
  file.description <- file.description[seq(indexLine, indexLine_end)]
  
  # remove initial blanck
  if(rm.blanck){
    file.description[1] <- gsub(paste0(field,":"),"",file.description[1]) # remove field name
     
    file.description <- trimws(file.description, which = "left") # remove blancks
    file.description <- file.description[sapply(file.description, nchar)>0] # remove first line
  }
  # remove comma
  if(rm.comma){
    file.description <- gsub(",", "", x = file.description)
  }
  
  # export
  return(file.description)
  
}

## ** writeDescription
#' @title Update description file
#' @description Overwrite the description file of a package
#' 
#' @inheritParams readDescription
#' @param newfile the content of the new description file
#' @param trace should the execution of the function be traced
#'
#' @examples 
#' \dontrun{
#' file <- readDescription("butils", path = pathGitHub())
#' writeDescription("butils", path = pathGitHub(), file)
#' }
writeDescription <- function(package, path, newfile, trace = TRUE){
  
  validPath(path, type = "dir", method = "writeDescription")
  validPath(file.path(path, package), type = "dir", method = "writeDescription")
  validPath(file.path(path, package, "DESCRIPTION"), type = "file", method = "writeDescription")
  
  ## write
  con <- file(file.path(path,package,"DESCRIPTION")) 
  writeLines(text = newfile, con = con) 
  close(con)
  
  return(invisible(TRUE))
}

## ** writeCollate
#' @title write cpllate field
#' @description write thecpllate field in the DESCRIPTION file of a package
#' 
#' @inheritParams readDescription
#' @param trace should the execution of the function be traced
#' 
#' @examples 
#' \dontrun{
#' writeCollate("butils", path = pathGitHub())
#' }
writeCollate <- function(package, path, trace = TRUE){
  
  ## read description file
  file.description <- readDescription(package, path = path)
  
  ## count white space
  line1 <- readDescription(package, path = path, field = "Collate", rm.blanck = FALSE)[2] # 2 because the first line is the field name
  line0 <- readDescription(package, path = path, field = "Collate", rm.blanck = TRUE)[1]
  space <- stringr::str_pad("", width=nchar(line1)-nchar(line0), side="right")
  
  ## list all R files
  Rfiles <- list.files(file.path(path,package,"R"), pattern = c(".R$",".r$"))
  
  ## new collapse
  TOadd <- c("Collate:",paste0(space,"'",Rfiles,"'"))
  
  indexLine <- grep("Collate:",file.description)
  test.change  <- FALSE 
  
  if(length(indexLine) == 0){
    if(trace){cat(">> add \'collate\' field to the DESCRIPTION file")}
    file.description <- c(file.description, TOadd)
    test.change <- TRUE
  }else{
    testPP <- grep(":",file.description,fixed=TRUE)>indexLine
    if(any(testPP)){
      indexLine_end <- min(grep(":",file.description,fixed=TRUE)[testPP])-1
    }else{
      indexLine_end <-  length(file.description)
    }
    
    if(!identical(TOadd,file.description[indexLine:indexLine_end])){
      if(trace){cat(">> update \'collate\' field in the DESCRIPTION file")}
      file.descriptionOLD <- file.description
      
      file.description <- c(file.description[1:(indexLine-1)],TOadd)
      if(indexLine_end<length(file.descriptionOLD)){
        file.description <- c(file.description,
                              file.descriptionOLD[(indexLine_end+1):length(file.descriptionOLD)]
        )
      }
      test.change <- TRUE
    }
  }
  
  if(test.change){
    writeDescription(package, path = path, newfile = file.description)
    if(trace){cat("\n")}
  }
  return(invisible(TRUE))
}

## ** writeDate
#' @title write date field
#' @description write the date field in the DESCRIPTION file of a package
#' 
#' @inheritParams readDescription
#' @param trace should the execution of the function be traced
#' 
#' @examples 
#' \dontrun{
#' writeDate("butils", path = pathGitHub())
#' }
writeDate <- function(package, path, trace = TRUE){
 
  ## read description file
  file.description <- readDescription(package, path = path)
  indexLine <- grep("Date:",file.description)
  
  newDate <- paste0("Date: ",format(Sys.time(), "%Y-%m-%d"))
  
  if(length(indexLine) == 0){
    warning("writeDate_package: \'Date\' field is missing in DESCRIPTION \n")
  }else if(file.description[indexLine] != newDate){
    if(trace){cat(">> update \'Date\' field in the DESCRIPTION file (",newDate,")")}
    file.description[indexLine] <- newDate
    writeDescription(package, path = path, newfile = file.description)
    if(trace){cat("\n")}
  }
  
  return(invisible(TRUE))
}

## ** crlf2lf
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

## ** cleanDir
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






