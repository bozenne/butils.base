## * valid functions (documentation)
#' @title Check argument in functions
#' @description Check the validity of the arguments in functions
#' @name validFCTs
#' 
#' @param value1 the value of the (first) argument to be checked
#' @param name1 the name of the (first) argument.
#' @param valid.length the acceptable length(s) for the argument. If \code{NULL} no test is performed.
#' @param valid.values the acceptable value(s) for the argument. If \code{NULL} no test is performed. Can also be "character" or "character_or_logical".
#' @param refuse.NULL should an error be output if value is \code{NULL}.
#' @param refuse.duplicates should an error be output if value contains duplicated values.
#' @param type Either "dir" or "file" to check whether to path points to an existing directory or file.
#' @param method the name of the function using the argument.
#' @param addPP add ": " after the name of the function in the error message.
#' @param extension filter the files by the type of extention.
#' @param check.fsep display a warning when the separator is not correctly specified.
#' 
#' @return An invisible \code{TRUE} or an error message.
#' 
#' @keywords function check

## * validCharacter
#' @rdname validFCTs
#' @export
validCharacter <- function(value1, name1 = as.character(substitute(value1)), valid.length, 
                           valid.values = "character", refuse.NULL = TRUE, refuse.duplicates = FALSE, 
                           method = NULL, addPP = TRUE){
  
  if(!is.null(method) && addPP){
    method <- paste0(method, ": ")
  }
  
  if(is.null(value1)){
    
    if(refuse.NULL == TRUE){
      stop(method, "\'", name1, "\' must not be NULL \n")
    }
    
  }else{
    
    #### check size
    n.value1 <- length(value1)
    
    if(!is.null(valid.length) && n.value1 %in% valid.length == FALSE){
      stop(method, "\'", name1, "\' must have length ", paste(valid.length, collapse = " or "), "  \n", 
           "length(", name1, ") : ", n.value1, "\n")
    }
    
    #### check duplicates
    if(refuse.duplicates == TRUE && any(duplicated(value1))){
      stop(method, "\'", name1, "\' contains duplicated values: ", "\"",paste(unique(value1[duplicated(value1)]), collapse = "\" \""), "\" \n")
    }
    
    #### check values
    if(identical(valid.values,"character")){
      
      if(any(is.character(value1) == FALSE)){
        stop(method, "\'", name1, "\' must be a ", if(n.value1 == 1){"character"}else{"vector of characters"}," \n", 
             "is(", name1, ") : ", paste(methods::is(value1), collapse = " "), "\n")
      }
      
    } else if(identical(valid.values,"character_or_logical")){
      
      if(any( (is.character(value1) == FALSE) * (is.logical(value1) == FALSE) > 0 )){
        stop(method, "\'", name1, "\' must be a ", if(n.value1 == 1){"character or logical"}else{"vector of characters or logicals"}," \n", 
             "is(", name1, ") : ", paste(methods::is(value1), collapse = " "), "\n")
      }
      
    } else if(!is.null(valid.values) && any(value1 %in% valid.values == FALSE)){
      
      stop(method, "wrong specification of \'", name1, "\' \n", 
           "valid values for \'", name1, "\' : ", if(refuse.NULL == FALSE){"NULL"}, " \"", paste(valid.values, collapse = "\" \""), "\" \n", 
           "refused value",if(sum(value1 %in% valid.values == FALSE)>1){"s"}," for \'", name1, "\' : \"", paste(value1[value1 %in% valid.values == FALSE], collapse = "\" \""), "\"\n")
      
    }
    
  }
  
  return(invisible(TRUE))
  
}

## * validPath (code)
#' @rdname validFCTs
validPath <- function(value1, name1 = as.character(substitute(value1)), type,
                      method = NULL, addPP = TRUE, extension = NULL, check.fsep = FALSE){
  
  if(!is.null(method) && addPP){
    method <- paste0(method, ": ")
  }
    
  validCharacter(type, valid.length = 1, valid.values = c("file", "dir"))
  
  try_path <- switch(type,
                     file = file.exists(value1),
                     dir = dir.exists(value1)
  )
  
  if(try_path == FALSE){
    stop(method, "\'", name1, "\' does not lead to an existing ",switch(type,"file"="file","dir"="directory")," \n", 
         "proposed value: \"", value1, "\"\n", 
         "current path: ", getwd(), "\n")
  }
  
  
  if(type == "dir"){ 
    if(check.fsep == TRUE && substr(value1, start = nchar(value1), stop = nchar(value1)) != "/"){
      warning(method, "possible bad specification of \'", name1, "\' \n", 
              "it should end with a fsep (e.g. \"/\") \n")
    }
  }else if(type == "file" && !is.null(extension)){
    fileExtension <- tools::file_ext(value1) 
    if(fileExtension %in% extension == FALSE){
      stop(method, "\'", name1, "\' has not the expected extension \n", 
           "proposed extension: \"", fileExtension, "\" \n", 
           "expected extension: \"", paste(extension, collapse = "\" \""), "\"\n")
    }
  }
  
  return(invisible(TRUE))
}

