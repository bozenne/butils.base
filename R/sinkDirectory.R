### sinkDirectory.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar 22 2018 (09:24) 
## Version: 
## Last-Updated: May 12 2024 (12:05) 
##           By: Brice Ozenne
##     Update #: 28
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## * sinkDirectory (documentation)
##' @title Import All .rds Files in a Directory.
##' @description Import all .rds files in a directory.
##' @name sinkDirectory
##' 
##' @param path [character] path to the directory.
##' @param string.keep [regexp] character string indicating files to import. 
##' @param string.exclude [regexp] character string indicating files to ignore.  
##' @param addMissingCol  [logical] if a dataset does not have the same columns as
##' the other, the necessary empty columns are added to it with \code{NA} as values.
##' @param fixed [logical] If \code{TRUE}, \code{pattern} is a string to be matched as is.
##' Argument passed to \code{\link{grep}}.
##' @param trace [logical] should a progress bar be displayed tracking how many files
##' have been imported.
##'
##' @details The function first read the name of all the files in the directory.
##' Then if the argument \code{string.keep} is not \code{NULL}, it only retains the
##' files whose name contain \code{string.keep}.
##' Then if the argument \code{string.exclude} is not \code{NULL}, it excludes the
##' files whose name contain \code{string.exclude}.
##'
##' Each file must contain a \code{data.table} object with the same columns,
##' so that they can be combined.
##' @return A \code{data.table} object.
##' @author Brice Ozenne
##'

## * sinkDirectoy (code)
##' @rdname sinkDirectory
##' @export
sinkDirectory <- function(path, string.keep = NULL, string.exclude = NULL,
                          addMissingCol = FALSE, fixed = FALSE,
                          trace = TRUE){

    ## ** import all files
    if(!dir.exists(path)){

        possible.dirs <- setdiff(list.dirs(file.path(path,".."), full.names = FALSE), "")
        
        stop("Directory ",path," does not exists \n",
             "Existing dir. in parent dir.: \"",paste0(possible.dirs,collapse = "\" \""),"\"\n")
    }
    
    allFiles <- list.files(path)

    if(length(allFiles)==0){
        warning("Empty directory \n")
        return(NULL)
    }
    index.file <- 1:length(allFiles)

    ## ** subset files
    if(!is.null(string.keep)){
        index.file <- intersect(index.file,grep(string.keep,allFiles,fixed = fixed))
    }
    if(!is.null(string.exclude)){
        index.file <- setdiff(index.file,grep(string.exclude,allFiles,fixed = fixed))
    }

    n.files <- length(index.file)
    if(n.files==0){
        warning("No file matching the query \n")
        return(NULL)
    }
    
    ## ** merge files
    dt.merge <- NULL
    if(trace){
        cat("read ",n.files," files \n", sep = "")
        pb <- utils::txtProgressBar(max = n.files)
    }
    for(iFile in 1:n.files){ # iFile <- 1

        ## *** read file 
        iFilename <- allFiles[index.file[iFile]]
        iRes <- data.table::as.data.table(readRDS(file = file.path(path,iFilename)))
        if(NROW(iRes)==0){next}
        iRes[,iFile := iFilename]

        ## *** solve pb with missing columns        
        if(!is.null(dt.merge) && addMissingCol==TRUE && NCOL(dt.merge)!=NCOL(iRes)){

            missing <- setdiff(names(dt.merge),names(iRes))
            if(!is.null(missing)){
                for(iMiss in missing){ ## iMiss <- missing[1]
                    vec.tempo <- dt.merge[1,.SD,.SDcols = iMiss][[1]]
                    vec.tempo[1] <- NA                    
                    iRes[, c(iMiss) := vec.tempo[1]]                    
                }
            }
            missing <- setdiff(names(iRes),names(dt.merge))
            if(!is.null(missing)){                
                for(iMiss in missing){ ## iMiss <- missing[1]
                    vec.tempo <- iRes[1,.SD,.SDcols = iMiss][[1]]
                    vec.tempo[1] <- NA                    
                    dt.merge[, c(iMiss) := vec.tempo[1]]
                }
            }

        }

        ## *** merge
        dt.merge <- rbind(dt.merge,iRes)
        if(trace){utils::setTxtProgressBar(pb,iFile)}
    }
    if(trace){close(pb)}

    ## ** export
    return(dt.merge)
}

##----------------------------------------------------------------------
### sinkDirectory.R ends here
