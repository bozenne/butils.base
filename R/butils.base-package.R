#' @docType package
#' @title butils.base package
#' @name butils.base
#' @description Tools to source R code or install R packages 
#' 
#' @import devtools
#' @import pipeR
#' @import Rcpp
#' @import selectr 
#' @import XML
#' @import data.table
#' @importFrom data.table as.data.table
#' @importFrom methods is
#' @importFrom roxygen2 roxygenise
#' @importFrom rvest html_attr html_nodes
#' @importFrom stats na.omit setNames
#' @importFrom stringr str_pad
#' @importFrom tools file_ext file_path_sans_ext list_files_with_exts
#' @importFrom utils capture.output setTxtProgressBar tail txtProgressBar  
#' @importFrom xml2 read_html
#' @importFrom zoo as.Date
NULL

# NOTE: selectr and XML are needed due to indirect dependency
