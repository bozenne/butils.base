### dependencyPackage.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: maj  4 2018 (09:38) 
## Version: 
## Last-Updated: maj  4 2018 (09:54) 
##           By: Brice Ozenne
##     Update #: 8
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

##' @title Extract the Names of the Dependences of a Package
##' @description Extract the names of the dependences of a package.
##' 
##' @param name [character] The name of the package.
##' @param path [character] The path to the directory containing the package.
##' @param field [character vector] which type of dependency should be considered?
##' 
##' @return character vector containing the names of the packages
##' @export
dependencyPackage <- function(name, path, field = c("Imports","Depends","Suggests")){

    field <- match.arg(field, choices = c("Imports","Depends","Suggests"), several.ok = TRUE)
    validPath(path, type = "dir", method = "package.source")
    validPath(file.path(path, name), type = "dir", method = "package.source")
    
    vec.package <- NULL
    if("Imports" %in% field){
        vec.package <- c(vec.package,
                         readDescription(name, path = path, field = "Imports", rm.comma = TRUE, rm.blanck = TRUE)
                         )
    }
    if("Depends" %in% field){
        vec.package <- c(vec.package,
                         readDescription(name, path = path, field = "Depends", rm.comma = TRUE, rm.blanck = TRUE)
                         )
    }
    if("Suggests" %in% field){
        vec.package <- c(vec.package,
                         readDescription(name, path = path, field = "Suggests", rm.comma = TRUE, rm.blanck = TRUE)
                         )
    }
    
    ## remove version in parenthesis
    vec.package <- gsub( " *\\(.*?\\) *", "", vec.package)
    
    ## remove space
    vec.package <- gsub( "^\\s+|\\s+$", "", vec.package)
    
    ##
    vec.package <- setdiff(vec.package, "R")

    return(vec.package)
}
    
    
######################################################################
### dependencyPackage.R ends here
