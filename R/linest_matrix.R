## #############################################################################
#'
#' @title Linear estimates matrix
#' 
#' @description Generate matrix specifying linear estimate.
#'
#' @name linest-matrix
#'
## #############################################################################
#'
#' @param object Model object
#' @param effect A vector of variables. For each configuration of
#'     these the estimate will be calculated.
#' @param at A list of values of covariates (including levels of some
#'     factors) to be used in the calculations
#'
#' @details Check this
#'
#' @seealso \code{\link{LSmeans}}, \code{\link{linest}}
#' @keywords utilities
#' 
#' @examples
#' 
#' ## Two way anova:
#' 
#' data(warpbreaks)
#'
#' ## An additive model
#' m0 <- lm(breaks ~ wool + tension, data=warpbreaks)
#'
#' ## Estimate mean for each wool type, for tension="M":
#' K <- linest_matrix(m0, at=list(wool=c("A", "B"), tension="M"))
#' K
#' 
#' ## Vanilla computation:
#' K %*% coef(m0)
#'
#' ## Alternative; also providing standard errors etc:
#' linest(m0, K)
#' esticon(m0, K)
#'
#' ## Estimate mean for each wool type when averaging over tension;
#' # two ways of doing this
#' K <- linest_matrix(m0, at=list(wool=c("A", "B")))
#' K
#' K <- linest_matrix(m0, effect="wool")
#' K
#' linest(m0, K)
#'
#' ## The linear estimate is sometimes called to "least squares mean"
#' # (LSmeans) or popupulation means.
#' # Same as
#' LSmeans(m0, effect="wool")
#'
#' ## Without mentioning 'effect' or 'at' an average across all
#' #predictors are calculated:
#' K <- linest_matrix(m0)
#' K
#' linest(m0, K)
#' 
#' ## Because the design is balanced (9 observations per combination
#' #of wool and tension) this is the same as computing the average. If
#' #the design is not balanced, the two quantities are in general not
#' #the same.
#' mean(warpbreaks$breaks)
#'
#' ## Same as 
#' LSmeans(m0)
#'
#' ## An interaction model 
#' m1 <- lm(breaks ~ wool * tension, data=warpbreaks)
#' 
#' K <- linest_matrix(m1, at=list(wool=c("A", "B"), tension="M"))
#' K
#' linest(m1, K)
#' K <- linest_matrix(m1, at=list(wool=c("A", "B")))
#' K
#' linest(m1, K)
#' K <- linest_matrix(m1, effect="wool")
#' K
#' linest(m1, K)
#' LSmeans(m1, effect="wool")
#' 
#' K <- linest_matrix(m1)
#' K
#' linest(m1, K)
#' LSmeans(m1)



#' @rdname linest-matrix
linest_matrix <- function(object, effect=NULL, at=NULL){
  UseMethod("linest_matrix")
}

## FIXME: linest_matrix.default: Should be a check of what 'object' is
#' @rdname linest-matrix
linest_matrix.default <- function(object, effect=NULL, at=NULL){
    res <- get_linest_list( object, effect, at )
    res <- .finalize_linest_list ( res )
    class(res) <- c("linest_matrix", "matrix")
    res
}

.finalize_linest_list <- function (aa){
    res               <- lapply( aa, function( mm ) apply( mm, 2, mean ) )
    res               <- do.call(rbind, res)
    attr(res, "at")   <- attr(aa, "at")
    attr(res, "grid") <- attr(aa, "grid")
    attr(res, "offset") <- attr(aa, "offset")
    res
}

print.linest_matrix <- function(x, ...){
  prmatrix(x)
  ## atr <- attributes(x)[c("at","grid")]
  ## aa <- !unlist(lapply(atr, is.null))
  ## str(atr[aa])
  invisible(x)
}

## This is the workhorse for generating the "contrast matrix"
#' @rdname linest-matrix
get_linest_list <- function(object, effect=NULL, at=NULL){
    ##cat(".get_linest_list\n")
    trms     <- delete.response( terms(object) )
    fact.lev <- get_xlevels( object )            ## factor levels
    ##cat("fact.lev:\n"); print(fact.lev)
    cov.ave  <- .get_covariate_ave( object, at )  ## average of covariates (except those mentioned in 'at')
    ##cat("cov.ave:\n"); print(cov.ave)
    vartype  <- get_vartypes( object )           ## which are factors and which are numerics
    ##cat("vartype:\n"); print(vartype)
    at.factor.name <- intersect( vartype$factor, names(at) )
    cov.ave.name   <- names( cov.ave )
    effect         <- setdiff( effect, at.factor.name )

    # tmp <- list(fact.lev=fact.lev, cov.ave=cov.ave, vartype=vartype, at.factor.name=at.factor.name, cov.ave.name=cov.ave.name, effect=effect, at=at)
    # print(tmp)

    if (is.null(effect)){
        if (length( at.factor.name ) > 0){
            new.fact.lev <- at[ at.factor.name ]
        } else {
            new.fact.lev <- NULL
        }
    } else {
        new.fact.lev  <- set_xlevels( fact.lev, at=at )
        new.fact.lev  <- new.fact.lev[c(effect, at.factor.name)]#
    }
    if (is.null(new.fact.lev)){
        ##cat("No 'effect' and no 'at'-factors; hence just a global average... \n")
        ## print(fact.lev)
        ## print(cov.ave.name)

        if ( length(fact.lev) > 0 ){
            ##cat("yes there are factors\n")
            newdata <- expand.grid( fact.lev )
            if (length( cov.ave.name ) > 0){
                ##cat("yes there are covariates\n")
                newdata[, cov.ave.name] <- cov.ave
            }
        } else {
            if (length( cov.ave.name ) > 0){
                ##cat("yes there are covariates\n")
                newdata <- matrix(unlist(cov.ave), nrow=1L)
                colnames(newdata) <- cov.ave.name
                newdata <- as.data.frame( newdata )
            } else {
                ##cat("there are no factors or covariates\n")
                newdata <- data.frame(1)
            }
        }

        XXlist <- list(get_X(object, newdata))
        ## cat("XXlist:\n"); print(XXlist)
        attr(XXlist,"at")   <- at[intersect(vartype$numeric, names(at))]
        attr(XXlist,"grid") <- NULL
    } else {
        ##cat("The general case; there are 'effect' factors or 'at' factors...\n")
        grid.data <- expand.grid(new.fact.lev)
        grid.data <- as.data.frame(lapply(grid.data, as.character), stringsAsFactors=FALSE)
        XXlist    <- list()
        for (ii in 1:nrow(grid.data)){
            config    <- grid.data[ ii, ,drop=FALSE ]
            fact.lev2 <- set_xlevels(fact.lev,  at=config)

            newdata   <- expand.grid( fact.lev2 )
            newdata[, cov.ave.name]  <- cov.ave
            XX             <- get_X(object, newdata, at)
            XXlist[[ ii ]] <- XX
        }

        grid.data[, names(cov.ave) ] <- cov.ave
        attr(XXlist,"at") <- at
        attr(XXlist,"grid") <- grid.data
        attr(XXlist,"offset") <- attr(XX, "offset")
    }
    class(XXlist) <- "linestList"
    XXlist
}

## --------------------------------------------------------------------

setOldClass("linest_matrix")

setAs("linest_matrix", "matrix",
      function(from){
          attr(from, "at") <- attr(from, "grid") <- NULL
          class(from) <- "matrix"
          from
      })

setAs("linest_matrix","Matrix",
      function(from){
          attr(from, "at") <- attr(from, "grid") <- NULL
          class(from) <- "matrix"
          as(from, "Matrix")
      })









