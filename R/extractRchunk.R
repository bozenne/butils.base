### extractRchunk.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: nov 15 2017 (09:16) 
## Version: 
## Last-Updated: May 12 2024 (11:05) 
##           By: Brice Ozenne
##     Update #: 145
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## * extractRchunk (documentation)
#' @title Extract code chunks from an .org file
#' @description Collect code chunks from an .org file and put them into an .R file.
#'
#' @param file [character] name of the file from which the R chunks should be extracted.
#' @param newfile [character] name of the R file be created.
#' @param header [character vector] optional additional strings to be added at the beginning of the file.
#' @param sep.header [character] string indicating headers.
#' @param sep.start.SRC [character] string indicating the start of a code block.
#' @param sep.end.SRC [character] string indicating the end of a code block.
#' @param rm.export.none [logical] should chunks of code with \code{:export none} be ignored?
#' @param rm.noexport [logical]  should header with \code{:noexport:} be ignored?
#' @param index.chunk [logical]  should the code block be numbered in the .R file?
#' @param overwrite  [logical] Can the existing R file be overwritten?
#'

## * extractRchunk (code)
#' @export
extractRchunk <- function(file, newfile = NULL, header = NULL,
                          sep.header = NULL, sep.start.SRC = NULL, sep.end.SRC = NULL,
                          rm.export.none = TRUE, rm.noexport = TRUE, index.chunk = TRUE,
                          overwrite = FALSE){

    ## ** define tags
    type <- tolower(tools::file_ext(file))
    
    if(type == "org"){
        sep.header <- "\\*+ "
        sep.start.SRC <- "\\#\\+BEGIN_SRC R"
        sep.end.SRC <- "\\#\\+END_SRC"
        if(rm.noexport){
            rm.noexport <- ":noexport:"
        }
        
        if(length(grep(".org$",file))==0){
            stop("file must have a .org extension \n")
        }

        if(is.null(newfile)){
            newfile <- gsub(".org$",".R",file)
        }
            
    }else if(type == "rmd"){
        sep.header <- "\\#"
        sep.start.SRC <- "\\`\\`\\`\\{r"
        sep.end.SRC <- "\\`\\`\\`$"
        rm.noexport <- NULL
        
        if(length(grep(".rmd$",file))==0){
            stop("file must have a .rmd extension \n")
        }

        if(is.null(newfile)){
            newfile <- gsub(".rmd$",".R",file)
        }
            
        }else if(is.null(sep.header)  || is.null(sep.start.SRC) || is.null(sep.end.SRC)){
            stop("if the file is not a org and rmd file the tags must be defined \n",
                 "please specify arguments \'sep.header\', \'sep.start.SRC\',  and \'sep.end.SRC\' \n")
        }
        
    if(is.null(newfile)){
        stop("Please specify the name of the R file to be created using the argument \'newfile\' \n")
    }
    
    ## ** open file    
    if(file.exists(file)==FALSE){
        stop("file does not exist \n")
    }
    
    if(file.exists(newfile) && (overwrite == FALSE)){
        stop("corresponding R file already exists \n",
             "set argument \'overwrite\' to TRUE to overwrite it \n")
    }
    
    con <- file(file, "rb") 
    file.line <- readLines(con) 
    close(con)

    ## ** find headers (if any)
    index.header <- grep(paste0("^",sep.header),file.line, value = FALSE)
    n.header <- length(index.header)
        
    ## ** find chunk (if any)
    index.start.chunk <- grep(paste0("^",sep.start.SRC),
                              x = file.line, value = FALSE)
    index.end.chunk <- grep(paste0("^",sep.end.SRC),
                            x = file.line, value = FALSE)
    n.chunk <- length(index.start.chunk)
    if(length(index.end.chunk) != n.chunk){
        if(length(index.start.chunk)>length(index.end.chunk)){
            ## when the next chunk start before the end of the previous one
            index.pb <- which(index.start.chunk[-1]<c(index.end.chunk,rep(0,length(index.start.chunk)-length(index.end.chunk)-1)))
            if(length(index.pb)>0){
                stop("Number of sep.start.SRC exceed the number of sep.end.SRC. \n",
                     "Possible issue between line: ",index.start.chunk[-1][index.pb[1]]," and ",index.end.chunk[index.pb[1]],"\n")
            }else{
                stop("Number of sep.start.SRC exceed the number of sep.end.SRC. \n")
            }
        }else{
            stop("Number of sep.end.SRC exceed the number of sep.start.SRC. \n")
        }
        
    }
    if(length(index.end.chunk) == 0){
        stop("No R code to extract \n")
        return(NULL)
    }

    ## ** group all
    df.extract <- rbind(data.frame(type = "header",
                                   index.start = index.header,
                                   index.stop = index.header,
                                   index = NA),
                        data.frame(type = "chunk",
                                   index.start = index.start.chunk,
                                   index.stop = index.end.chunk,
                                   index = 1:n.chunk))
    
    df.extract <- df.extract[order(df.extract$index.stop),,drop=FALSE]
    n.extract <- NROW(df.extract)

    ## ** level of the headings
    indexDT.header <- which(df.extract$type == "header")
    index.header <- df.extract[indexDT.header,"index.start"]
    vec.star <- unlist(lapply(strsplit(file.line[index.header], split = "* ",fixed=TRUE),"[[",1))
    df.extract$level <- as.integer(NA)
    df.extract[indexDT.header, "level"] <- nchar(vec.star)


    ## ** remove non exported sections
    df.extract$export <- TRUE
    if(!is.null(rm.noexport) && any(grepl(":noexport:",file.line[index.header]))){
        line.noexport <- index.header[grep(":noexport:",file.line[index.header])]
        indexDF.noexport <- which(df.extract$index.start %in% line.noexport)

        ## also avoid exporting chunk and following subsections
        for(iIndex in indexDF.noexport){ ## iIndex <- indexDF.noexport[1]
            iIndex.remaining <- (iIndex+1):NROW(df.extract)
            test.stop <- df.extract[iIndex.remaining,"level"] <= df.extract[iIndex,"level"]
            indexDF.noexport.stop <- c(iIndex.remaining[which(test.stop)],NROW(df.extract)+1)[1]
            df.extract[iIndex:(indexDF.noexport.stop-1),"export"] <- FALSE
        }
    }

    ## ** create string
    file.content <- NULL
    
    for(iE in 1:n.extract){ # iE <- 1
        iType <- as.character(df.extract[iE,"type"])
        iStart <- df.extract[iE,"index.start"]
        iEnd <- df.extract[iE,"index.stop"]
        if(iType == "chunk"){
            test.export.none <- length(grep(":exports none",file.line[iStart],fixed = TRUE))
            if(rm.export.none && test.export.none==1){
                next
            }
            iStart <- iStart + 1
            iEnd <- iEnd - 1
        }
        if(df.extract[iE,"export"]==FALSE){
            next
        }
        add.before <- switch(iType,
                             "header"=NULL,
                             "chunk"=if(index.chunk){paste0("## chunk ",df.extract[iE,"index"],"")}else{NULL})

        prefix.add <- switch(iType,
                             "header"="## ",
                             "chunk"=NULL)
        toAdd <- paste0(prefix.add,file.line[iStart:iEnd])
        
        add.after <- switch(iType,
                            "header"=if(iE<n.extract&&df.extract[iE+1,"type"]=="chunk"){""}else{NULL},
                            "chunk"="")
        
        file.content <- c(file.content,add.before,toAdd,add.after)
    }

    ## ** write file
    if(!is.null(header)){
        file.content <- c(header,file.content)
    }
    con <- file(newfile) 
    writeLines(text = file.content, con = con) 
    close(con)

    ## ** export
    return(invisible(file.content))
    
    
}



##----------------------------------------------------------------------
### extractRchunk.R ends here
