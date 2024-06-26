### object2script.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: apr 15 2018 (16:52) 
## Version: 
## Last-Updated: May 12 2024 (12:02) 
##           By: Brice Ozenne
##     Update #: 113
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## * object2script
##' @title Display the Source Code Corresponding to an Object
##' @description Display the source code corresponding to an object.
##' @name object2script
##'
##' @param object a vector, matrix, data.frame, data.table, or a list of these elements.
##' @param digits [interger,>0] indicating the number of decimal places to be used. Passed to \code{round}.
##' @param print [logical] should the result be displayed.
##' @param final.space [character] Character string to be added at the end. For internal use.
##' @param ... for compatibility with the generic function.
##' 
##' @details Used by emacs-config.
##' 
##' @examples
##' ## vector
##' vec <- 1:10
##' object2script(vec)
##' object2script(setNames(vec, 1:10))
##' 
##' ## matrix
##' M <- matrix(rnorm(16), 4, 4)
##' object2script(M)
##'
##' colnames(M) <- c("a","b","c","d")
##' object2script(M)
##' 
##' rownames(M) <- c("a","b","c","d")
##' object2script(M)
##'
##' ## data.frame
##' df <- data.frame(matrix(rnorm(16), 4, 4),"a")
##' object2script(df, digits = 5)
##' 
##' ## data.table
##' dt <- data.table(matrix(rnorm(16), 4, 4),"a")
##' object2script(dt, digits = 5)
##'
##' ## list
##' l <- list(vec, dt = dt, df = df)
##' object2script(l)
##'
##' @export
`object2script` <-
    function(object, ...) UseMethod("object2script")

## * object2script.numeric
##' @rdname object2script
##' @export
object2script.numeric <- function(object, digits = NULL, print = TRUE, final.space = "\n", ...){

    ## ** check
    dots <- list(...)
    if(length(dots)>0){
        txt.s <- if(length(dots)>1){"s"}else{""}
        txt <- paste0("\"",paste0(names(dots), collapse = "\" \""),"\"")
        stop("Unknown argument",txt.s,": ",txt,"\n")
    }
    
    if(is.vector(object)){
        name.object <- names(object)
        
        value <- object
        if(!is.null(digits)){
            value <- round(value, digits = digits)
        }

        if(!is.null(name.object)){
            value.txt <- paste0("\"",name.object,"\" = ",value)
        }else{
            value.txt <- value
        }

        txt <- paste0("c(",paste0(value.txt, collapse = ", "),")",final.space)
    }else{
        stop("unknown class: only support vector/matrix/data.frame/data.table")
    }

    ## export
    if(print){cat(txt)}
    return(invisible(txt))
}

## * object2script.character
##' @rdname object2script
##' @export
object2script.character <- function(object, print = TRUE, final.space = "\n", ...){

    ## ** check
    dots <- list(...)
    if(length(dots)>0){
        txt.s <- if(length(dots)>1){"s"}else{""}
        txt <- paste0("\"",paste0(names(dots), collapse = "\" \""),"\"")
        stop("Unknown argument",txt.s,": ",txt,"\n")
    }
    
    if(is.vector(object)){
        name.object <- names(object)
        
        value <- object

        if(!is.null(name.object)){
            value.txt <- paste0("\"",name.object,"\" = ",value)
        }else{
            value.txt <- value
        }

        txt <- paste0("c(",paste0(value.txt, collapse = ", "),")",final.space)
    }else{
        stop("unknown class: only support vector/matrix/data.frame/data.table")
    }

    ## export
    if(print){cat(txt)}
    return(invisible(txt))
}


## * object2script.matrix
##' @rdname object2script
##' @export
object2script.matrix <- function(object, digits = NULL, print = TRUE, final.space = "\n", ...){

    ## ** check
    dots <- list(...)
    if(length(dots)>0){
        txt.s <- if(length(dots)>1){"s"}else{""}
        txt <- paste0("\"",paste0(names(dots), collapse = "\" \""),"\"")
        stop("Unknown argument",txt.s,": ",txt,"\n")
    }
    
    object.rownames <- rownames(object)
    object.colnames <- colnames(object)
    n.col <- NCOL(object)
    n.row <- NROW(object)
    if(!is.null(digits)){
        object <- round(object, digits = digits)
    }
    
    if(!is.null(object.rownames) && !is.null(object.colnames)){
        type <- "matrix"
    }else if(!is.null(object.rownames)){
        type <- "row"
    }else if(!is.null(object.colnames)){
        type <- "col"
    }else{
        type <- "col"
    }

    if(type == "matrix"){        
        value.txt <- paste0(as.double(object), collapse = ", ")
        rownames.txt <- paste0("\"",paste(object.rownames,collapse="\", \""),"\"")
        colnames.txt <- paste0("\"",paste(object.colnames,collapse="\", \""),"\"")
        M.txt <- paste0("matrix(c(",value.txt,"), \n",
                        "nrow = ",length(object.rownames),", \n",
                        "ncol = ",length(object.colnames),", \n",                        
                        "dimnames = list(c(",rownames.txt,"),c(",colnames.txt,")) \n) \n") 
    }else if(type == "row"){
        end <- c(rep(",", n.row-1),"\n)")
        value.txt <- apply(object,1,paste,collapse = ", ")
        if(!is.null(object.rownames)){
            value.txt <- paste0("\"",object.rownames,"\" = ",value.txt)
        }
        vecValue.txt <- paste(paste0("c(",value.txt,")", end), collapse =" \n      ")
        M.txt <- paste0("rbind(",vecValue.txt,"\n")
    }else if(type == "col"){
        end <- c(rep(",", n.col-1),"\n)")
        value.txt <- apply(object, 2 , paste, collapse = ", ")
        vecValue.txt <- paste0("c(",value.txt,")", end)
        if(!is.null(object.colnames)){
            vecValue.txt <- paste0("\"",object.colnames,"\" = ",vecValue.txt)
        }
        vecValue.txt <- paste(vecValue.txt, collapse = " \n      ")
        M.txt <- paste0("cbind(",vecValue.txt,final.space)
    }

    ## export
    if(print){cat(M.txt)}
    return(invisible(M.txt))
}

## * object2script.data.frame
##' @rdname object2script
##' @export
object2script.data.frame <- function(object, digits = NULL, print = TRUE, final.space = "\n", ...){

    ## ** check
    dots <- list(...)
    if(length(dots)>0){
        txt.s <- if(length(dots)>1){"s"}else{""}
        txt <- paste0("\"",paste0(names(dots), collapse = "\" \""),"\"")
        stop("Unknown argument",txt.s,": ",txt,"\n")
    }
    
    object.names <- names(object)
    object.class <- sapply(object, class)
    n.col <- NCOL(object)
    
    if(!is.null(digits) && "numeric" %in% object.class){
        for(iCol in which(object.class == "numeric")){
            object[[iCol]] <- round(object[[iCol]], digits = digits)
        }        
    }
    if("character" %in% object.class || "factor" %in% object.class){
        for(iCol in which(object.class %in% c("character","factor"))){
            object[[iCol]] <- paste0("\"",object[[iCol]],"\"")
        }        
    }
    end <- c(rep(",", n.col-1),")")
    value.txt <- apply(object, 2 , paste, collapse = ", ")
    value.txt <- paste0("\"",object.names,"\" = c(",value.txt,")")
    vecValue.txt <- paste(paste0(value.txt, end), collapse =" \n           ")
    M.txt <- paste0("data.frame(",vecValue.txt,final.space)

    ## export
    if(print){cat(M.txt)}
    return(invisible(M.txt))
}

## * object2script.data.table
##' @rdname object2script
##' @export
object2script.data.table <- function(object, digits = NULL, print = TRUE, final.space = "\n", ...){

    ## ** check
    dots <- list(...)
    if(length(dots)>0){
        txt.s <- if(length(dots)>1){"s"}else{""}
        txt <- paste0("\"",paste0(names(dots), collapse = "\" \""),"\"")
        stop("Unknown argument",txt.s,": ",txt,"\n")
    }
    
    object.names <- names(object)
    object.class <- sapply(object, class)
    n.col <- NCOL(object)
    
    if(!is.null(digits) && "numeric" %in% object.class){
        for(iCol in which(object.class == "numeric")){
            object[[iCol]] <- round(object[[iCol]], digits = digits)
        }        
    }
    if("character" %in% object.class || "factor" %in% object.class){
        for(iCol in which(object.class %in% c("character","factor"))){
            object[[iCol]] <- paste0("\"",object[[iCol]],"\"")
        }        
    }
    end <- c(rep(",", n.col-1),")")
    value.txt <- apply(object, 2 , paste, collapse = ", ")
    value.txt <- paste0("\"",object.names,"\" = c(",value.txt,")")
    vecValue.txt <- paste(paste0(value.txt, end), collapse =" \n           ")
    M.txt <- paste0("data.table(",vecValue.txt,final.space)

    ## export
    if(print){cat(M.txt)}
    return(invisible(M.txt))
}

## * object2script.list
##' @rdname object2script
##' @export
object2script.list <- function(object, digits = NULL, print = TRUE, ...){

    ## ** check
    dots <- list(...)
    if(length(dots)>0){
        txt.s <- if(length(dots)>1){"s"}else{""}
        txt <- paste0("\"",paste0(names(dots), collapse = "\" \""),"\"")
        stop("Unknown argument",txt.s,": ",txt,"\n")
    }
    
    name.list <- names(object)
    n.list <- length(object)

    ## extract for each element of the list
    res.ls <- vector(mode = "character", length=n.list)
    for(iL in 1:n.list){
        res.ls[iL] <- object2script(object[[iL]], digits = digits, print = FALSE, final.space = "")
        if(!is.null(name.list) && name.list[iL] != ""){
            res.ls[iL] = paste0(name.list[iL]," = ",res.ls[iL])
        }
    }

    ## concatenate 
    end <- c(rep(",\n     ", n.list-1),")\n")
    list.txt <- paste0("list(",paste(paste(res.ls,end),collapse = ""))

    ## export
    if(print){cat(list.txt)}
    return(invisible(list.txt))
}

##----------------------------------------------------------------------
### object2script.R ends here
