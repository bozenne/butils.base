## * revTraceback (documentation)
#' @title Get and Print Call Stacks (reverse order)
#' 
#' @description Same as \code{base::traceback} but in reverse order
#'
#' @inheritParams base::traceback
#' @param trace should the call stacks be output
#'

## * revTraceback (code)
#' @export
revTraceback <- function (x = NULL,
                          max.lines = getOption("deparse.max.lines"),
                          trace = TRUE){
    n <- length(x <- .traceback(x))
    if (n == 0L) 
        cat(gettext("No traceback available"), "\n")
    else {
        for (i in n:1L) {
            xi <- x[[i]]
            label <- paste0(n - i + 1L, ": ")
            m <- length(xi)
            srcloc <- if (!is.null(srcref <- attr(xi, "srcref"))) {
                          srcfile <- attr(srcref, "srcfile")
                          paste0(" at ", basename(srcfile$filename), "#", 
                                 srcref[1L])
                      }
            if (is.numeric(max.lines) && max.lines > 0L && max.lines < 
                m) {
                xi <- c(xi[seq_len(max.lines)], " ...")
                m <- length(xi)
            }
            if (!is.null(srcloc)) {
                xi[m] <- paste0(xi[m], srcloc)
            }
            if (m > 1) 
                label <- c(label, rep(substr("          ", 1L, 
                                             nchar(label, type = "w")), m - 1L))
            if(trace){ cat(paste0(label, xi), sep = "\n") }
        }
    }
    invisible(x)
}
