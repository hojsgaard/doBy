#' @title Ordering (sorting) rows of a data frame
#' 
#' @description Ordering (sorting) rows of a data frame by the certain variables
#'     in the data frame. This function is essentially a wrapper for the order()
#'     function - the important difference being that variables to order by can
#'     be given by a model formula.
#'
#' @name by-order
#' 
#' @details The sign of the terms in the formula determines whether sorting
#'     should be ascending or decreasing; see examples below
#' 
#' @param formula The right hand side of a formula
#' @param data A data frame
#' @return The ordered data frame
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk} and Kevin Wright
#' @seealso \code{\link{transformBy}}, \code{\link{splitBy}}
#' @keywords utilities
#' @examples
#' 
#' data(dietox)
#' orderBy(~ Time + Evit, data=dietox)
#' ## Sort decreasingly by Time
#' orderBy(~ - Time + Evit, data=dietox)
#' 

#' @rdname by-order
orderBy <- function(formula, data){
    formula <- unlist(rhsf2list(formula))
    dplyr::arrange_(data, formula)
}

#' @rdname by-order
order_by <- function(data, formula){
    formula <- unlist(rhsf2list(formula))
    dplyr::arrange_(data, formula)    
}

#' @rdname by-order
orderByOLD <- function (formula, data){

  myrank <- function(x){
    rv  <- rep(NA,length(x))
    r   <- rank(cdat[!is.na(cdat)])
    rv[!is.na(cdat)]  <- r
    rv[is.na(rv)]     <- max(r)+1
    rv
  }

  form <- formula
  dat  <- data
  
  if(form[[1]] != "~")
    stop("Error: Formula must be one-sided.")

  formc <- as.character(form[2])
  formc <- gsub(" ","",formc)
  if(!is.element(substring(formc,1,1),c("+","-")))
    formc <- paste("+",formc,sep="")

  vars <- unlist(strsplit(formc, "[\\+\\-]"))
  vars <- vars[vars!=""] # Remove spurious "" terms

  signs <- formc  
  for (i in 1:length(vars)){
    signs <- gsub(vars[i],"",signs)
  }
  signs <- unlist(strsplit(signs,""))

  orderlist <- list()
  for(i in 1:length(vars)){
    csign <- signs[i]
    cvar  <- vars[i]
    #cat("csign:", csign, "cvar:", cvar, "\n")
    cdat  <- dat[,cvar]
    #print(cdat)
    #cdat<<-cdat
    if(is.factor(cdat)){
      if(csign=="-")
        orderlist[[i]] <- -myrank(cdat)
      else
        orderlist[[i]] <- myrank(cdat)
    }
    else {
      if(csign=="-")
        orderlist[[i]] <- -cdat
      else
        orderlist[[i]] <- cdat
    }
  }

  ##orderlist <<- orderlist
  #print(orderlist)
  dat[do.call("order",orderlist),,drop=FALSE]
}


















# orderBy <- function (formula, data, na.last = TRUE, decreasing = FALSE){
#   data <- data
#   form <- unlist(strsplit(paste(formula)[2],"\\+"))
#   form <- gsub(" ","",form)
  
#   dodo<-data[,form,drop=FALSE]
  
#   z <- NULL
#   for (j in 1:ncol(dodo)){
#     z <- c(z, list(dodo[,j]))
#   }
#   if (any(diff(sapply(z, length)) != 0))
#     stop("argument lengths differ")
#   ans <- sapply(z, is.na)
#   ok <- if (is.matrix(ans)){
#     !apply(ans, 1, any)
#   } else {
#     !any(ans)
#   }
  
#   if (all(!ok))
#     return(integer(0))
#   z[[1]][!ok] <- NA
#   ans <- do.call("order", c(z, decreasing = decreasing))
#   keep <- seq(along = ok)[ok]
#   ord<-ans[ans %in% keep]
#   return(data[ord,])

# }
