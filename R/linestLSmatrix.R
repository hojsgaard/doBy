## #############################################################################
#'
#' @title Compute linear estimates, including LS-means (aka population means or
#'     marginal means)
#' 
#' @description Compute linear estimates for a range of models. One example of
#'     linear estimates is LS-means (least squares means, also known as
#'     population means and as marginal means).
#'
#' @name ls-means
#'
## #############################################################################
#' 
#' @details There are restrictions on the formulas allowed in the model object.
#'     For example having \code{y ~ log(x)} will cause an error. Instead one
#'     must define the variable \code{logx = log(x)} and do \code{y~logx}.
#' 
#' @aliases LSmeans LSmeans.default LSmeans.lmerMod popMeans popMeans.default
#'     popMeans.lmerMod
#' @param object Model object
#' @param effect A vector of variables. For each configuration of these the
#'     estimate will be calculated.
#' @param at A list of values of covariates (including levels of some factors)
#'     to be used in the calculations
#' @param level The level of the (asymptotic) confidence interval.
#' @param ...  Additional arguments; currently not used.
#' @return A dataframe with results from computing the contrasts.
#' @note The \code{LSmeans} method is a recent addition to the package, and it
#'     will eventually replace the \code{popMeans} method.
#' 
#' Please report unexpected behaviour.
#' 
#' Some of the code has been inspired by the \bold{lsmeans} package.
#' @section Warning: Notice that \code{LSmeans} and \code{LSmatrix} fails if
#' the model formula contains an offset (as one would have in connection with
#' e.g. Poisson regression. It is on the todo-list to fix this
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{LSmatrix}}, \code{\link{linest}}
#' @keywords utilities
#' @examples
#' 
#' 
#' ## Two way anova:
#' 
#' data(warpbreaks)
#' 
#' m0 <- lm(breaks ~ wool + tension, data=warpbreaks)
#' m1 <- lm(breaks ~ wool * tension, data=warpbreaks)
#' LSmeans(m0)
#' LSmeans(m1)
#' 
#' ## same as:
#' K <- LSmatrix(m0);K
#' linest(m0, K)
#' K <- LSmatrix(m1);K
#' linest(m1, K)
#' 
#' LSmatrix(m0, effect="wool")
#' LSmeans(m0, effect="wool")
#' 
#' LSmatrix(m1, effect="wool")
#' LSmeans(m1, effect="wool")
#' 
#' LSmatrix(m0, effect=c("wool","tension"))
#' LSmeans(m0, effect=c("wool","tension"))
#' 
#' LSmatrix(m1, effect=c("wool","tension"))
#' LSmeans(m1, effect=c("wool","tension"))
#' 
#' 
#' ## Regression; two parallel regression lines:
#' 
#' data(Puromycin)
#' 
#' m0 <- lm(rate ~ state + log(conc), data=Puromycin)
#' ## Can not use LSmeans / LSmatrix here because of
#' ## the log-transformation. Instead we must do:
#' Puromycin$lconc <- log( Puromycin$conc )
#' m1 <- lm(rate ~ state + lconc, data=Puromycin)
#' 
#' LSmatrix(m1)
#' LSmeans(m1)
#' 
#' LSmatrix(m1, effect="state")
#' LSmeans(m1, effect="state")
#' 
#' LSmatrix(m1, effect="state", at=list(lconc=3))
#' LSmeans(m1, effect="state", at=list(lconc=3))
#' 
#' ## Non estimable contrasts
#' 
#' ## ## Make balanced dataset
#' dat.bal <- expand.grid(list(AA=factor(1:2), BB=factor(1:3),
#'                             CC=factor(1:3)))
#' dat.bal$y <- rnorm(nrow(dat.bal))
#' 
#' ## ## Make unbalanced dataset
#' #      'BB' is nested within 'CC' so BB=1 is only found when CC=1
#' #       and BB=2,3 are found in each CC=2,3,4
#' dat.nst <- dat.bal
#' dat.nst$CC <-factor(c(1,1,2,2,2,2,1,1,3,3,3,3,1,1,4,4,4,4))
#' 
#' mod.bal  <- lm(y ~ AA + BB*CC,    data=dat.bal)
#' mod.nst  <- lm(y ~ AA + BB : CC, data=dat.nst)
#' 
#' LSmeans(mod.bal, effect=c("BB", "CC"))
#' LSmeans(mod.nst, effect=c("BB", "CC"))
#' LSmeans(mod.nst, at=list(BB=1, CC=1))
#' 
#' LSmeans(mod.nst, at=list(BB=1, CC=2))
#' ## Above: NA's are correct; not an estimable function
#' 
#' if( require( lme4 )){
#'  warp.mm <- lmer(breaks ~ -1 + tension + (1|wool), data=warpbreaks)
#'  LSmeans(warp.mm, effect="tension")
#'  class(warp.mm)
#'  fixef(warp.mm)
#'  coef(summary(warp.mm))
#'  vcov(warp.mm)
#'  if (require(pbkrtest))
#'    vcovAdj(warp.mm)
#' }
#' 
#' 
#' 
#' LSmeans(warp.mm, effect="tension")
#' 
#' 
#' @export LSmeans
#'

#' @rdname ls-means
LSmatrix <- function(object, effect=NULL, at=NULL){
  UseMethod("LSmatrix")
}

## FIXME: LSmatrix.default: Should be a check of what 'object' is
#' @rdname ls-means
LSmatrix.default <- function(object, effect=NULL, at=NULL){
    res <- .get_linest_list( object, effect, at )
    res <- .finalize_linest_list ( res )
    class(res) <- c("LSmatrix", "matrix")
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

print.LSmatrix <- function(x,...){
  prmatrix(x)
  ## atr <- attributes(x)[c("at","grid")]
  ## aa <- !unlist(lapply(atr, is.null))
  ## str(atr[aa])
  invisible(x)
}

.get_linest_list <- function(object, effect=NULL, at=NULL){
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

    #' tmp <- list(fact.lev=fact.lev, cov.ave=cov.ave, vartype=vartype, at.factor.name=at.factor.name, cov.ave.name=cov.ave.name, effect=effect, at=at)
    #' print(tmp)

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


#' @rdname ls-means
LSmeans <- function(object, effect=NULL, at=NULL, level=0.95,...){
    UseMethod("LSmeans")
}

#' @rdname ls-means
LSmeans.default <- function(object, effect=NULL, at=NULL, level=0.95,...){
    K   <- LSmatrix(object, effect=effect, at=at)
    out <- linest(object, K, level=level, ...)
    out
}

#' @rdname ls-means
#' @param adjust.df Should denominator degrees of freedom be adjusted?
LSmeans.lmerMod <- function(object, effect=NULL, at=NULL, level=0.95, adjust.df=TRUE, ...){
    K   <- LSmatrix(object, effect=effect, at=at)
    out <- linest(object, K, level=level, adjust.df=adjust.df, ...)
    out
}


























#' @rdname ls-means
popMeans         <- LSmeans

#' @rdname ls-means
popMeans.default <- LSmeans.default

#' @rdname ls-means
popMeans.lmerMod <- LSmeans.lmerMod

## --------------------------------------------------------------------


setOldClass("LSmatrix")

setAs("LSmatrix","matrix",
      function(from){
          attr(from,"at")<- attr(from,"grid")<-NULL
          class(from)<-"matrix"
          from
      })

setAs("LSmatrix","Matrix",
      function(from){
          attr(from,"at")<- attr(from,"grid")<-NULL
          class(from)<-"matrix"
          from
          as(from,"Matrix")
      })

