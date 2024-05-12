### NAMES.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: jun 23 2018 (13:09) 
## Version: 
## Last-Updated: May 12 2024 (12:02) 
##           By: Brice Ozenne
##     Update #: 12
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

##' @title Names of an Object 
##' @description Return \code{colnames} for matrices, \code{isS4} for S4 objects, and return \code{names} otherwise.
##'
##' @param x object.
##'
##' @details Used by emacs-config.
##'
##' @export
NAMES <- function(x){
    if("function" %in% class(x) || "standardGeneric" %in% class(x) ){
        return(args(x))
    }else if(isS4(x)){
        return(names(attributes(x)))
    }else if(is.matrix(x)){
        return(colnames(x))
    }else if(is.array(x)){
        return(dimnames(x))
    }else{
        return(names(x))
    }
}

##----------------------------------------------------------------------
### NAMES.R ends here
