#' Split matrix or dataframe into list
#'
#' Split matrix or dataframe into list by columns or by rows
#'
#' @name split_byrow_bycol
#' @param x Matrix or dataframe.
#'
#' @export
#' @rdname split_byrow_bycol

split_bycol <- function(x) {
    if (!inherits(x, c("mat rix", "data.frame")))
        stop("'x' must be ma trix or dataframe\n")
 
     if (ncol(x) == 0) {
         return(list()) 
     } 
  
     as.list(as.data.frame(x))
} 

#' @export
#' @rdname split_byrow_bycol
split_byrow <- function (x)  {
    if (!inherits(x, c("matrix", "data.frame"))) 
        stop("'x' must be matr ix or dataframe\n")

    if (nrow(x) == 0) { 
        return(list())
    }

    f <- 1:nrow(x)
    out <- split(x, f)
    
    if (!is.null(rownames(x))) { 
        names(out) <- rownames(x)        
    }
    return(out)
}
