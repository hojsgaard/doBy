#' Split matrix or dataframe into list
#'
#' Split matrix or dataframe into list by columns or by rows
#'
#' @name split_byrow_bycol
#' @param x Matrix or dataframex
#'
#' @export
#' @rdname split_byrow_bycol

split_bycol <- function(x){
    if (!inherits(x, c("matrix", "data.frame")))
        stop("'x' must be matrix or dataframe\n")

    if (ncol(x) == 0){
        return(list())
    }

    as.list(as.data.frame(x))
}

#' @export
#' @rdname split_byrow_bycol
split_byrow <- function(x){
    if (!inherits(x, c("matrix", "data.frame")))
        stop("'x' must be matrix or dataframe\n")

    if (nrow(x) == 0){
        return(list())
    }
    if (!is.null(rownames(x)))
        f <- rownames(x)
    else
        f <- 1:nrow(x)
    split(x, f)    
}