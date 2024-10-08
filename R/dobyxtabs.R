#' @export
doby.xtabs <-
function (formula = ~., data = parent.frame(),
          subset, na.action, exclude = c(NA,NaN), drop.unused.levels = FALSE,
          FUN=sum) 
{

  ep      <- FALSE;
  sh.eprint<- function(x){
    if (ep==TRUE){str <- deparse(substitute(x)); cat(paste("E>>", str," =", x),fill=TRUE)}}
  
  if (!missing(formula) && !inherits(formula, "formula")) 
    stop("formula is incorrect")
  if (any(attr(terms(formula), "order") > 1)) 
    stop("interactions are not allowed")
  if (missing(na.action)) 
    na.action <- getOption("na.action")
  
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  m$... <- m$exclude <- m$drop.unused.levels <- NULL ## FIXME ...
  m$... <- m$exclude <- m$FUN <- NULL
  m[[1]] <- as.name("model.frame")
  mf <- eval(m, parent.frame())
  if (length(formula) == 2) {
    by <- mf
    y <- NULL
  }
  else {
    i <- attr(attr(mf, "terms"), "response")
    by <- mf[-i]
    y <- mf[[i]]
  }
  
  by <- lapply(by, function(u) {
    if (!is.factor(u)) 
      u <- factor(u, exclude = exclude)
    u[, drop = drop.unused.levels]
  })

  x <- if (is.null(y)) 
    do.call("table", by)
  else if (NCOL(y) == 1){
    tapply(y, by, FUN) 
  }
  else {
    z <- lapply(as.data.frame(y), tapply, by, FUN)    
    array(unlist(z), dim = c(dim(z[[1]]), length(z)), dimnames = c(dimnames(z[[1]]), 
                                                        list(names(z))))
  }
  x[is.na(x)] <- 0
  class(x) <- c("xtabs", "table")
  attr(x, "call") <- match.call()
  x
  value <- x
  return(value)
}

######################################################################
