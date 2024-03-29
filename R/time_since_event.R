#' @title Calculate "time since event" in a vector.
#' 
#' @description Calculate "time since event" in a vector.
#'
#' @details Events are coded as 1 in numeric vector (and non-events are
#'     coded with values different from 1). \code{timeSinceEvent} will give the
#'     time since event (with and without sign). In a logical vector, events are
#'     coded as TRUE and all non-events as FALSE.
#' 
#' @param yvar A numerical or logical vector specifying the events
#' @param tvar An optional vector specifying time
#' @return A dataframe with columns 'yvar', 'tvar', 'abs.tse' (absolute time
#'     since nearest event), 'sign.tse' (signed time since nearest event) and
#'     'run' (indicator of the time window around each event).
#'
#' @note NA's in yvar are converted to zeros.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{subSeq}}, \code{\link{rle}}
#' @keywords utilities
#' @examples
#' 
#' ## Events:
#' yvar <- c(0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0,
#'           0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0)
#' 
#' ## Plot results:
#' tse <- timeSinceEvent(yvar)
#' plot(sign.tse ~ tvar, data=tse, type="b")
#' grid()
#' rug(tse$tvar[tse$yvar==1], col=4, lwd=4)
#' points(scale(tse$run), col=tse$run, lwd=2)
#' lines(abs.tse + .2 ~ tvar, data=tse, type="b", col=3)
#' 
#' ## Find times for which time since an event is at most 1:
#' tse$tvar[tse$abs <= 1]
#' 
#' yvar <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 
#' 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
#' )
#' 
#' tvar <- c(207, 208, 208, 208, 209, 209, 209, 209, 210, 210, 211, 211, 
#' 211, 212, 213, 213, 214, 214, 215, 216, 216, 216, 216, 217, 217, 
#' 217, 218, 218, 219, 219, 219, 219, 220, 220, 221, 221, 221, 221, 
#' 222, 222, 222)
#' 
#' timeSinceEvent(yvar, tvar)
#' 
#' 
#' 
#' @export timeSinceEvent
timeSinceEvent <- function(yvar, tvar=seq_along(yvar)) {

    if (!(is.numeric(yvar) | is.logical(yvar))) {
        stop("yvar must be either numeric or logical")
    }
    
    yvar[is.na(yvar)] <- 0
    
    event.idx <- which(yvar == 1)
    
    if (length(event.idx) == 0) {
        return(NULL)
    }
    
    n.event <- length(event.idx)

    ## find event times
    event.time <- tvar[event.idx]

    ## get time difference to each event
    rrr <-  do.call(rbind, lapply(event.idx, function(ii) tvar-tvar[ii]))
    abs.tse <- apply(abs(rrr), 2, min)


    ## get the event windows (~ symmetrical around event time)
    ewin<-rep.int(NA, length(yvar))
    if (n.event > 1) {
        ff <- event.time[1:(n.event-1)] + diff(event.time) / 2
        ewin[tvar <= ff[1]] <- 1
        for (ii in 2:(length(ff) - 0)){
            ewin[tvar > ff[ii - 1] & tvar <= ff[ii] ] <- ii
        }
        ewin[tvar > ff[length(ff)]] <- n.event
    } else {
        ewin[] <- n.event
    }

  ## get the signs
    ggg <- list()
    for (ii in 1:(length(event.idx))){
        ggg[[ii]] <- rrr[ii, ewin == ii]
    }
    ggg <- unlist(ggg)
    sign.tse <- sign(ggg) * abs.tse

    run <- cumsum(yvar)

    un <- unique(run)
    tlist <- list()
    for (ii in 1:length(un)){
        vv <- un[ii]
        yy <- yvar[run == vv]
        tt <- tvar[run == vv]
        tt <- tt - tt[1]
        tlist[[ii]] <- tt
    }
    tae <- unlist(tlist)
    tae[run == 0] <- NA

    yvar2 <- rev(yvar)
    tvar2 <- rev(tvar)

    run2 <- cumsum(yvar2)
    un2 <- unique(run2)
    tlist2 <- list()
    for (ii in 1:length(un2)){
        vv <- un2[ii]
        yy <- yvar2[run2 == vv]
        tt <- tvar2[run2 == vv]
        tt <- tt - tt[1]
        tlist2[[ii]] <- tt
  }
    tbe <- unlist(tlist2)
    tbe[run2==0] <- NA

    tbe <- rev(tbe)
    run[run==0]<-NA
    
    ans <- cbind(data.frame(yvar=yvar, tvar=tvar), abs.tse, sign.tse, ewin=ewin,
                 run, tae=tae, tbe=tbe)
    ans
}













## .timeSinceEvent <- function(yvar, tvar=seq_along(yvar)){

##   if (!(is.numeric(yvar) | is.logical(yvar))){
##     stop("yvar must be either numeric or logical")
##   }

##   yvar[is.na(yvar)] <- 0

##   event.idx <- which(yvar==1)
##   if (length(event.idx)==0){
##     return(NULL)
##   }

##   res <- vector("list", length(event.idx))
##   for (kk in seq_along(event.idx)){
##     jj <- event.idx[kk]
##     res[[kk]] <- tvar-tvar[jj]
##   }

##   res      <-do.call(rbind, res)
##   #print(res)
##   abs.tse  <-apply(abs(res),2,min)
##   #print(abs.tse)

##   sgn 	<- rep.int(0, length(yvar))
##   sss 	<- subSeq(yvar,item=0)
##   for (ii in 1:nrow(sss)){
##     idx  <- sss[ii,1]:sss[ii,2]
##     idx1 <- seq_along(idx)
##     uuu  <- abs.tse[idx]
##     fff <- .find.inc.dec(uuu)
##     sgn[idx] <- fff
##   }



##   #print(sgn)
##   sign.tse <- abs.tse * sgn
##   eventWindow <- rep(NA, length(abs.tse))

##   #print(sign.tse)

##   curr.state <- 1
##   eventWindow[1] <- curr.state
##   for (jj in 2:(length(eventWindow)-1)){
##     if (sign.tse[jj] <= 0 & sign.tse[jj-1] >0 ){
##       curr.state <- curr.state + 1
##     }
##     eventWindow[jj] <- curr.state
##   }
##   eventWindow[length(eventWindow)] <- curr.state
##   #print(eventWindow)

##   run <- cumsum(yvar)
##   un <- unique(run)
##   tlist <- list()
##   for (ii in 1:length(un)){
##     vv <- un[ii]
##     yy <- yvar[run==vv]
##     tt <- tvar[run==vv]
##     tt <- tt - tt[1]
##     tlist[[ii]] <- tt
##   }
##   timeAfterEvent <- unlist(tlist)
##   timeAfterEvent[run==0] <- NA

##   yvar2 <- rev(yvar)
##   tvar2 <- rev(tvar)

##   run2 <- cumsum(yvar2)
##   un2 <- unique(run2)
##   tlist2 <- list()
##   for (ii in 1:length(un2)){
##     vv <- un2[ii]
##     yy <- yvar2[run2==vv]
##     tt <- tvar2[run2==vv]
##     tt <- tt - tt[1]
##     tlist2[[ii]] <- tt
##   }
##   timeBeforeEvent <- unlist(tlist2)
##   timeBeforeEvent[run2==0] <- NA

##   timeBeforeEvent <- rev(timeBeforeEvent)
##   run[run==0]<-NA

##   #aux <- cbind(yvar,tvar, run, timeAfterEvent, timeBeforeEvent)
##   aux <- cbind(run, tae=timeAfterEvent, tbe=timeBeforeEvent)
##   ans <- cbind(data.frame(yvar=yvar, tvar=tvar), abs.tse, sign.tse, ewin=eventWindow, aux)
##   return(ans)
## }






## .find.inc.dec <- function(ttt){
##   ans <- rep.int(0,length(ttt))
##   max.ttt <- max(ttt)
##   wm <- which(ttt==max.ttt)
##   if (max(wm)==length(ttt)){ ## The last element is maximal
##     mmm <- max(wm)
##     ans[] <- 1
##   } else {
##     if (min(wm)==1){ ## The first element is maximal
##       mmm <- 1
##       ans[] <- -1
##     } else {
##       mmm <- ceiling(mean(wm))
##       ans[1:(mmm-1)] <- 1
##       ans[mmm:length(ans)] <- -1
##     }
##   }
##   ans
## }
## # ttt <- c(1,1,1,2,2,3,4,4,4,5,5,5,3,2,2,2,1,1)
## # ttt <- c(1,1,1,2,2,3,4,4,4,5,5,4)
## # ttt <- rev(ttt)




