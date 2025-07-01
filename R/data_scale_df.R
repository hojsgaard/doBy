#' @title Scale numeric variables in a data frame
#'
#' @description Applies `base::scale()` to numeric, integer, or
#'     logical columns in a data frame. Non-numeric columns are left
#'     unchanged.
#' @concept data_handling
#' @param x A data frame or matrix.
#' @param center Logical; if TRUE, center the variables.
#' @param scale Logical; if TRUE, scale the variables.
#'
#' @details
#' If `x` is not a data frame, `base::scale()` is applied directly.
#'
#' @return An object of the same class as `x`.
#'
#' @examples
#' scale_df(iris)
#'
#' @name scale_df
#' @export
scale_df <- function(x, center = TRUE, scale = TRUE){

    if (!is(x, "data.frame")){
        return(scale(x, center=center, scale=scale))
    } else { ## x is dataframe
        
        b <- sapply(x,
                    function(z){is(z, c("numeric")) || is(z, c("integer")) || is(z, c("logical")) })
        
        if (!any(b)){ ## x only has numeric values
            return(scale(x, center=center, scale=scale))
        } else { ## x is dataframe with non-numerics
            
            x2 <- x[,b, drop=FALSE]
            x2 <- scale(x2, center=center, scale=scale)
            x[, b] <- x2
            
            if (!is.null(a <- attributes(x2)$"scaled:center"))
                attr(x, "scaled:center") <- a
            
            if (!is.null(a <- attributes(x2)$"scaled:scale"))
                attr(x, "scaled:scale") <- a
            
            return(x)            
        }            
    }
}

