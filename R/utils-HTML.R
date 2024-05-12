## * readHtmlLinks (documentation)
#' @title links of a url page
#' 
#' @description Get the links of a url page.
#'
#' @param link [character] the url page.
#' @param xpath [character] a string used to extract the relevant expression.
#' @param ... additional arguments to be passed to \code{read_html}.
#' 
#' @details inspired from http://stackoverflow.com/questions/27297484/r-using-rvest-package-instead-of-xml-package-to-get-links-from-url 
#' 
#' @keywords function url
#' @examples 
#' readHtmlLinks("https://cran.r-project.org/")
#' readHtmlLinks("https://github.com/bozenne/BuyseTest")
#' 

## * readHtmlLinks (code)
#' @export
readHtmlLinks <- function(link, xpath = ".//a", ...){

    content <- xml2::read_html(link, ...)
    ## class(content)
    
    ## note: ".//a" = selectr::css_to_xpath("a", prefix = ".//")
    
    links <- xml2::xml_attr(xml2::xml_find_all(content, xpath = xpath), attr = "href") ## rvest::html_nodes
    
    return(links)
}
