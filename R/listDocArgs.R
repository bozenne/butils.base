### listArgs.R --- 
#----------------------------------------------------------------------
## author: Brice Ozenne
## created: okt  3 2017 (09:55) 
## Version: 
## last-updated: May 12 2024 (11:28) 
##           By: Brice Ozenne
##     Update #: 126
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

## * listDocArgs (documentation)
#' @title Get all the arguments from the Rd files
#' @description Create a file in inst containing the description of all the arguments from the Rd file of a package
#' @name listDocArgs
#' 
#' @param dir the path to the directory containing the R package
#' @param trace should the execution of the function be traced
#'
#' @return a list containing:
#' \itemize{
#' \item argByFunction name of the arguments of the functions/methods that are documented.
#' \item docByFunction description of the arguments of the functions/methods that are documented.
#' \item functionNames name of the functions/methods that are documented.
#' \item dfAll data.frame object gathering function names, argument names, and description of the arguments
#' }
#' 
#' @examples
#' \dontrun{
#' res <- listDocArgs(file.path(pathGitHub(),"butils.base"))
#' res$functionNames
#' res$argByFunction
#' }
#'

## * listDocArgs (code)
#' @export
listDocArgs <- function(dir,trace=TRUE){
  
    dir.man <- file.path(dir,"man")
    file_Rd <- list.files(dir.man, pattern = ".Rd")
  
    if(length(file_Rd)==0){
        cat("no \'.Rd\' files found in the \'",dir,"\' directory \n",sep="")
        return(invisible(NULL))
    }
  
    n.files <- length(file_Rd)
    if(trace==TRUE){cat(n.files,"files found \n")}
  
    #### loop over files to extract args, function name, and doc
    ls.args <- NULL
    ls.doc <- NULL
    vec.function <- NULL
    vec.Ufunction <- NULL
    df.argsdoc <- NULL
  
    if(trace){
        pb <- utils::txtProgressBar(max = n.files)
    }
  
    for(iter_file in 1:n.files){

        ## extract sections
        ls.sections <- extractSectionFromRD(file.path(dir.man,file_Rd[iter_file]))
        if("usage" %in% names(ls.sections) == FALSE){next}
        if("arguments" %in% names(ls.sections) == FALSE){next}
        
        ## extract function name
        expr.tempo <- gregexpr("\\((?>[^()]|(?R))*\\)", ls.sections$usage, perl = TRUE)
        vec.tempo <- substring(ls.sections$usage[[1]], expr.tempo[[1]], expr.tempo[[1]] + attr(expr.tempo[[1]], "match.length") - 1)

        name.tempo <- ls.sections$usage[[1]]
        for(iFunction in vec.tempo){
            name.tempo <- sub(iFunction,"",name.tempo, fixed = TRUE)
        }
        # handle S3 methods
        name.tempo <- gsub("\\method{","",name.tempo, fixed = TRUE)
        name.tempo <- gsub("}{",".",name.tempo, fixed = TRUE)
        name.tempo <- gsub("}","",name.tempo, fixed = TRUE)
        name.tempo <- gsub(" <- value","<-",name.tempo, fixed = TRUE)

        
        function.name <- strsplit(name.tempo, split = "\n", fixed = TRUE)[[1]]
        Ufunction.name <- paste(function.name,collapse = " | ")

        ## extract documentation
        vec.doc <- strsplit(gsub("\\n","",ls.sections$arguments), split = "\\\\item\\{(.*?)\\}")[[1]]
        vec.doc <- vec.doc[vec.doc!=""]
        
        ## extract arguments
        arg.tempo <- gsub("\\n","",ls.sections$arguments)
        for(iDoc in vec.doc){
            arg.tempo <- sub(iDoc,"",arg.tempo, fixed = TRUE)
        }
        vec.args <- strsplit(arg.tempo, split = "\\item", fixed = TRUE)[[1]]
        vec.args <- vec.args[vec.args!=""]

        ## remove leading and ending bracket
        vec.doc <- unname(sapply(vec.doc, gsub, pattern = "^\\{|\\}$", replacement = ""))
        vec.args <- unname(sapply(vec.args, gsub, pattern = "^\\{|\\}$", replacement = ""))

        # export
        ls.args <- c(ls.args, list(vec.args))
        ls.doc <- c(ls.doc, list(vec.doc))
        vec.function <- c(vec.function,function.name)
        vec.Ufunction <- c(vec.Ufunction,Ufunction.name)
        df.argsdoc <- rbind(df.argsdoc, data.frame(fct = Ufunction.name, args = vec.args, doc = vec.doc))
        if(trace){
            utils::setTxtProgressBar(pb, iter_file)
        }
    }
    if(trace){close(pb)}
    names(ls.args) <- vec.Ufunction
    names(ls.doc) <- vec.Ufunction
  
    # export
    return(list(argByFunction=ls.args,
                docByFunction=ls.doc,
                functionNames=vec.function,
                dfAll=df.argsdoc))
  
}

## * extractSectionFromRD
#' @title Extract sections from an .rd file
#' @description Extract sections from an .rd file
#' 
#' @param file the path to a .rd file
#'
#' @examples 
#' 
#' \dontrun{
#' path2packages <- file.path(pathGitHub(),"butils.base")
#' all.files <- list.files(file.path(path2packages,"man"), full.names = TRUE)
#' extractSectionFromRD(all.files[1])
#' }
extractSectionFromRD <- function(file){

    ## open file
    file.size <- file.info(file)$size
    con <- file(file, "rb") 
    file.character <- readChar(con, file.size*10) 
    close(con)

    
    ## remove roxygen header
    test.header <- length(grep("Generated by roxygen2",file.character,fixed = TRUE))
    if(test.header>0){
        file.character <- gsub("% Generated by roxygen2(.*?)\\.R","",file.character)
    }

    ## rm \r
    test.slashR <- length(grep(pattern="\r",x=file.character))>0
    file.character <- gsub(pattern="\r",replacement="",file.character)

    ## identify the content of each section
    # https://stackoverflow.com/questions/41749058/r-parse-nested-parentheses
    regex.matched <- gregexpr("\\{(?>[^{}]|(?R))*\\}", file.character, perl = TRUE)
    file.sections <- substring(file.character, regex.matched[[1]], regex.matched[[1]] + attr(regex.matched[[1]], "match.length") - 1)
    n.sections <- length(file.sections)

    ## identify the name of each section
    file.rm <- file.character
    for(iSection in 1:n.sections){
        file.rm <- sub(file.sections[iSection], "", file.rm, fixed = TRUE)
    }
    name.tempo <- strsplit(file.rm,"\n")[[1]]
    name.sections <- gsub("\\","",name.tempo[name.tempo!=""],fixed = TRUE)

    
    ## clean sections
    # remove leading and ending bracket
    file.sections2 <- unname(sapply(file.sections, gsub, pattern = "^\\{|\\}$", replacement = ""))

    # remove duplicated newline
    file.sections2 <- unname(sapply(file.sections2, gsub, pattern = "\\n\\n", replacement = "\\\n"))

    # removing leading|ending newlines
    file.sections2 <- unname(sapply(file.sections2, gsub, pattern = "^\\n|\\n$", replacement = ""))

    ## export
    out <- as.list(stats::setNames(file.sections2, name.sections))
    return(out)        
}


#----------------------------------------------------------------------
### listArgs.R ends here
