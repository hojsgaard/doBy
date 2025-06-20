#' @title Group-wise scaling of data
#' @description Splits a data frame or matrix by grouping variables and scales numeric variables within each group.
#'
#' @param formula Grouping structure: a formula, character vector, or variables as `as.quoted`.
#' @param data A data frame or matrix.
#' @param center Logical; if TRUE, center the variables.
#' @param scale Logical; if TRUE, scale the variables.
#'
#' @return A list of data frames or matrices (same class as input), one per group.
#'
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{summaryBy}}, \code{\link{transformBy}}, \code{\link{orderBy}}
#' @keywords utilities
#'
#' @examples
#' scale_by(iris, ~Species)
#' scale_by(iris, ~1)
#'
#' ## Combine result into one data frame:
#' a <- scale_by(iris, ~Species)
#' d <- do.call(rbind, a)
#'
#' ## Old interface
#' scaleBy(~Species, data = iris, center = TRUE, scale = FALSE)
#' scaleBy(~1, data = iris)
#'
#' @rdname by_scale
#' @export
scaleBy <- function(formula, data = parent.frame(), center = TRUE, scale = TRUE) {
  lapplyBy(formula, data = data, scale_df, center = center, scale = scale)
}

#' @rdname by_scale
#' @export
scale_by <- function(data, formula, center = TRUE, scale = TRUE) {
  do.call(scaleBy, list(formula = formula, data = data, center = center, scale = scale))
}



## #' @title Scale a dataframe or matrix
## #' @description Split a dataframe into a list according to the levels
## #'     of variables in the dataframe and scale the numeric variables
## #'     in each dataframe in the list.
## #' @name by_scale
## #'
## #' @param formula Variables to split data frame by, as `as.quoted`
## #'     variables, a formula or character vector.
## #' @param data A dataframe or matrix
## #' @param center Logical, should data be centered.
## #' @param scale Logical, should data be scaled.
## #' 
## #' @return A list of objects of same class as `x`
## #' 
## #' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
## #' @seealso \code{\link{orderBy}}, \code{\link{order_by}},
## #'     \code{\link{summaryBy}}, \code{\link{summary_by}},
## #'     \code{\link{transformBy}}, \code{\link{transform_by}} 
## #' @keywords utilities
## #' 
## #' @examples
## #'
## #' scaleBy(~Species, data=iris, center=TRUE, scale=FALSE)
## #' scaleBy(~1, data=iris, center=TRUE, scale=FALSE)
## #'
## #' scale_by(iris, ~Species)
## #' scale_by(iris, ~1)
## #' 
## #' ## Not combine list of dataframes to one dataframe e.g. as:
## #' a <- scale_by(iris, ~Species)
## #' d <- do.call(rbind, a)

## #' @rdname by_scale
## #' @export
## scaleBy <- function(formula, data=parent.frame(), center=TRUE, scale=TRUE){
##     lapplyBy(formula, data=data, scale_df, center=center, scale=scale)        
## }

## #' @rdname by_scale
## #' @export
## scale_by <- function(data, formula, center=TRUE, scale=TRUE){
##     arg <- list(formula=formula, data=data, center=center, scale=scale)
##     do.call(scaleBy, arg)
## }   




