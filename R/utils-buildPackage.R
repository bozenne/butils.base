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
  if(indexLine == tail(indexPP, 1)){ # if field is the last one in the description file go to the end
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
