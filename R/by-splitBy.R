###########################################################################
#'
#' @title Split a data frame
#' @description Split a dataframe according to the levels of variables
#'     in the dataframe. The variables to split by can be given as a
#'     formula or as a character vector.
#' @name by-split
#' 
###########################################################################
#'
#' @param formula Variables to split data frame by, as ‘as.quoted’ variables,
#'     a formula or character vector.
#' @param data A data frame
#' @param drop Logical indicating if levels that do not occur should be
#'     dropped. Deprecated; levels that do not occur are ignored.
#' 
#' @return A list of dataframes.
#' 
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{orderBy}}, \code{\link{summaryBy}},
#'     \code{\link{transformBy}}
#' @keywords utilities
#' @examples
#' 
#' data(dietox, package="doBy")
#' splitBy(formula = ~Evit + Cu, data = dietox)
#' splitBy(formula = c("Evit", "Cu"), data = dietox)
#' 
#' splitBy(~Month, data=airquality)
#' splitBy("Month", data=airquality)


#' @export 
#' @rdname by-split
split_by <- function(data, formula, drop=TRUE){
    arg <- list(formula=formula, data=data, drop=drop)
    do.call(splitBy, arg)
}

#' @export 
#' @rdname by-split
splitBy <-function (formula, data = parent.frame(), drop=TRUE) {
                                        #, return.matrix=FALSE){

    if (!inherits(data, "tbl_df")) is.tib = FALSE
    else {is.tib = TRUE; data = as.data.frame(data)}
    
    data.var  <- names(data)
    if (!(inherits(formula, c("formula", "character"))))        
        stop("'formula' must be a right hand sided formula or a character vector")
    
    if (inherits(formula, "formula")){
        rhs       <- formula[[2]]
        rhs.var   <- all.vars(rhs)
    } else {
        rhs.var <- formula
    }
    
    cls <- lapply(data, class)
    factor.columns <- which(unlist(lapply(cls, function(x) any( x %in% "factor"))))
                                        #print(factor.columns)

    cls       <- lapply(data, class)
    num.idx   <- cls %in% c("numeric","integer")
    fac.var   <- data.var[ !num.idx ]
    
    rhs.fac   <- intersect( rhs.var, data.var )
    if ("." %in% rhs.var){
        ## need all factors not mentioned elsewhere as grouping factors
        rhs.fac <- union( fac.var, rhs.fac)
    }
    ## str(list(rhs.var=rhs.var, rhs.fac=rhs.fac, fac.var=fac.var))
    
    rh.trivial <- length( rhs.var ) == 0 #; cat(sprintf("rh.trivial=%d\n", rh.trivial))
    
    ## FIXME rhs.fac, rhs.var -- clean up!!!
    ## Use: data, rhs.fac, rh.trivial
    if ( rh.trivial ){
        grps <- rep.int(1, nrow(data))
        unique.grps <- 1
        rh.idx    <- 1
    } else {
        grps <- .get_rhs_string( data, rhs.fac, sep.string="|")
        unique.grps <- unique(grps)
        rh.idx    <- match(unique.grps, grps)
    }
    
    out_list <- vector("list", length(unique.grps))

    names(out_list) <- unique.grps
    for (ii in 1:length(unique.grps)){
        dd <- data[unique.grps[ ii ] == grps,,drop=FALSE]
        if (drop && length(factor.columns)>0){
            for (jj in 1:length(factor.columns)){
                dd[, factor.columns[jj]] <- factor(dd[, factor.columns[jj]])
            }
        }
        if (is.tib) dd <- as_tibble(dd)
        out_list[[ ii ]] <- dd
    }
    
    idxvec <- vector("list", length(unique.grps))
    names(idxvec) <- unique.grps
    for (ii in 1:length(unique.grps)){
        idxvec[[ii]] <- which(grps == unique.grps[ii])
    }
    
    groupid <- data[rh.idx, rhs.fac, drop=FALSE]
    rownames(groupid) <- 1:nrow(groupid)
    
    attr(out_list,"groupid") <- groupid
    attr(out_list,"idxvec")  <- idxvec
    attr(out_list,"grps")    <- grps
    
    class(out_list) <- c("splitByData", "list")    
    out_list
}



#' @export
print.splitByData <- function(x, ...){
#  print(attr(x,"groupid"))
    print(cbind(listentry=names(x), attr(x,"groupid")))
    return(invisible(x))
}

