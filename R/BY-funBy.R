#' @title A template function for creating groupwise functions
#' 
#' @description A template function for creating groupwise functions
#'
#' @name by-fun
#' 
#' @aliases formulaFunBy xyFunBy
#'
#' @param formula A formula of the form \code{y ~ x} (which must be variable
#'     names in \code{data}).
#' @param xy A character vector with one or two elements (which must be variable
#'     names in \code{data}).
#' @param group A right hand sided formula or a character vector defining the
#'     grouping of data
#' @param data A data frame
#' @param FUN The function to be applied
#' @param class The class to give the result of the returned value of the
#'     created function.
#' @param \dots Further arguments passed on to \code{FUN}
#' @return A function
#' @note This function is a recent addition and has not been thoroughly tested.
#'     Please report bugs.
#'
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{parseGroupFormula}}
#' @keywords utilities
#' @examples
#' 
#' 
#' ## Example: Create a function for creating groupwise t-tests
#' 
#' mydata <- data.frame(y=rnorm(32), x=rnorm(32),
#' g1=factor(rep(c(1,2),each=16)), g2=factor(rep(c(1,2), each=8)),
#' g3=factor(rep(c(1,2),each=4))) 
#' 
#' 
#' t.testBy1 <- function(formula, group, data, ...){
#'   formulaFunBy(formula, group, data, FUN=t.test, class="t.testBy1", ...)
#' }
#' 
#' t.testBy2 <- function(formula, group, data, ...){
#'   xyFunBy(formula, group, data, FUN=t.test, class="t.testBy1", ...)
#' }
#' 
#' 
#' t.testBy1(y~g1, ~g2+g3, data=mydata)
#' t.testBy2(y~x, ~g2+g3, data=mydata)
#' 
#' 

#' @rdname by-fun
formulaFunBy <- function(formula, group, data, FUN, class=NULL, ...){
  grpData <- splitBy( group, data=data )
  res <- lapply( grpData, function(.xx){ FUN(formula, data=.xx, ...)})
  if (!is.null(class))
    class(res) <- class
  res
}

#' @rdname by-fun
xyFunBy <- function(xy, group, data, FUN, class=NULL, ...){
  if ( !(class(xy) %in% c("character", "formula") ))
    stop(" 'xy' must be a character vector or a two-sided formula ")
  if (class(xy)=="formula"){
    vars <- all.vars(xy)
  } else {
    vars = xy
  }
  grpData <- splitBy( group, data=data )
  if (length(vars)==2){
    res <- lapply(grpData, function(dd){
      x <- dd[,vars[1]]
      y <- dd[,vars[2]]
      FUN(x, y, ...)})
  }
  else{
    res <- lapply(grpData, function(dd){
      x <- dd[,vars[1]]
      FUN(x, ...)})	
  }
  if (!is.null(class))
    class(res) <- class
  res
}




## createFunBy <- function(formula, data, FUN, class=NULL, ...){
##   mm <- parseGroupFormula(formula)
##   groupData <- splitBy(mm$groupFormula, data=data)
##   res <- lapply(groupData, function(xx) FUN(mm$model, data=xx, ...))
##   ##attr(res, "splitByData") <- attributes(groupData)
##   if (!is.null(class))
##     class(res) <- class
##   res
## }


