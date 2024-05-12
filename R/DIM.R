### DIM.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: jun 22 2018 (15:06) 
## Version: 
## Last-Updated: May 12 2024 (12:01) 
##           By: Brice Ozenne
##     Update #: 11
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

##' @title Dimensions of an Object
##' @description Return \code{length} for vectors, lists, S4 objects, and use \code{dim} otherwise.
##'
##' @param x object.
##'
##' @details Used by emacs-config.
##' 
##' @export
DIM <- function(x){
    if(isS4(x)){
        length(attributes(x))
    } else if(data.table::is.data.table(x)){
        return(dim(x))
    } else if(is.data.frame(x)||is.matrix(x)){
        return(dim(x))
    } else if(is.list(x)||is.vector(x)||is.null(dim(x))){
        return(length(x))
    } else {
        return(dim(x))
    }
}

######################################################################
### DIM.R ends here
