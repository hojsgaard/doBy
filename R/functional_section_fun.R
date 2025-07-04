#' Set default values in a functions arguments
#'
#' `set_default()` takes a function and returns a new version with
#' updated default values for specified arguments.
#'
#' This is useful when you want to programmatically create specialized
#' versions of a function with certain arguments preset to default
#' values.
#'
#' @param fun A function whose arguments will get new default values.
#' @param nms Character vector of argument names, or something coercible to that
#'   (e.g. a call to `v()`).
#' @param vls Optional vector or list of values to use as defaults.
#'   If `nms` is a named list, `vls` can be `NULL`.
#' @concept functional
#' @return A new function with updated default values in its formals.
#' 
#' @details
#' - The specified arguments will be moved to the end of the formal argument list in the returned function.
#' - You can supply arguments as a named list or as separate names and values.
#'
#' @examples
#' ## Simple example
#' f1 <- function(x, y, z) { x + y + z }
#'
#' ## Add defaults for x and y
#' set_default(f1, list(x = 1, y = 2))
#' # function (z, x = 1, y = 2) { x + y + z }
#'
#' ## Same using separate vectors of names and values
#' set_default(f1, c("x", "y"), c(1, 2))
#'
#' ## Works with v() style if supported
#' # set_default(f1, v(x, y), c(1, 2))
#'
#' ## Another example with more arguments
#' f2 <- function(a, b, c, d) { a + b + c + d }
#' set_default(f2, list(b = 10, d = 5))
#'
#' @export
set_default <- function(fun, nms, vls = NULL) {
    args <- nms_vls_to_list(nms, vls)

    nms <- names(args)
    vls <- args

    fmls <- formals(fun)

    i <- match(nms, names(fmls))
    j <- c(setdiff(seq_along(fmls), i), i) # new order

    fmls <- fmls[j]
    fmls[nms] <- vls
    formals(fun) <- fmls
    fun
}





## #' @export
## set_default <- function(fun, nms, vls=NULL){

##     args <- nms_vls_to_list(nms, vls)

##     nms <- names(args)
##     vls <- args
    
##     fmls <- formals(fun)
    
##     i <- match(nms, names(fmls))
##     j <- c(setdiff(seq_along(fmls), i), i) # new order
    
##     fmls <- fmls[j]
##     fmls[nms] <- vls
##     formals(fun) <- fmls
##     fun    
## }











#' @title Section a function and set default values in function
#' 
#' @description Section a functions domain by fixing certain
#'     arguments of a function call.
#'
#' @name section_fun
#' @concept functional 
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk} based on code
#'   adapted from the curry package.
#' 
#' @param fun Function to be sectioned
#' @param nms Either a named list of the form name=value where each
#'     name is the name of an argument of the function (in which case
#'     `vls` is ignored) or a character vector of names of arguments.
#' @param vls A vector or list of values of the arguments
#' 
#' @param method One of the following: 1) "args" (default); based on
#'     substituting fixed values into the function argument list as
#'     default values). For backward compatibility can also be "def".
#'     2) "body" for substituting fixed values into the function
#'     body. For backward compatibility can also be "sub". 3) "env":
#'     (for environment); using an auxillary argument for storing
#'     sectioned values. 
#'     
#'
#' @param envir Environment
#' @param object An object from section_fun (a scaffold object).
#'
#' @seealso [bquote_fun_list()]
#' @details Let E be a subset of the cartesian product X x Y where X
#'     and Y are some sets. Consider a function f(x,y) defined on
#'     E. Then for any x in X, the section of E defined by x (denoted
#'     Ex) is the set of $y$s in Y such that (x, y) is in
#'     E. Correspondingly, the section of f(x,y) defined by x is the
#'     function $f_x$ defined on Ex given by $f_x(y)=f(x,y)$.
#'
#' `section_fun` is a wrapper for calling `set_default` (default
#'     method), `section_fun_env` or `section_fun_sub`. Notice that
#'     creating a sectioned function with `section_fun_sub` can be
#'     time consuming.
#'
#' @return A new function: The input function `fun` but with certain
#'     arguments fixed at specific values.
#' 
#' @examples
#'
#' f  <- function(x, y){x + y}
#' 
#' f_ <- section_fun(f, list(y = 10),    method="args") ## "def"" is default
#' f_ <- section_fun(f, nms="y", vls=10, method="args") ## SAME AS ABOVE
#' f_
#' f_(x=1)
#'
#' f_ <- section_fun(f, list(y = 10),    method="body") ## 
#' f_ <- section_fun(f, nms="y", vls=10, method="body") ## SAME AS ABOVE
#' f_
#' f_(x=1)
#'
#' f_ <- section_fun(f, list(y = 10),    method="env")
#' f_ <- section_fun(f, nms="y", vls=10, method="env") ## SAME AS ABOVE
#' f_
#' f_(x=1)
#' get_section(f_)
#' get_fun(f_)
#'
#'  
#' ## With more complicated values:
#' g <- function(A, B) {
#'   A + B
#' }
#' g_ <- section_fun(g, list(A = matrix(1:4, nrow=2)))
#' g_ <- section_fun(g, "A", list(matrix(1:4, nrow=2)))
#' g_(diag(1, 2))
#'
#' g_ <- section_fun(g, list(A = matrix(1:4, nrow=2)))
#'
#' ## Using built in function
#' set.seed(123)
#' rnorm5 <- section_fun(rnorm, list(n=5)) 
#' rnorm5(0, 1)
#'
#' set.seed(123)
#' rnorm(5)
#'
#' 
NULL

## FIXME: If nms is a list it should be reported if not all elements
## of nms have names. Guess the same applies to caracas.



#' @rdname section_fun
#' @export
section_fun <- function(fun, nms, vls=NULL, method="args") {
    method_ <- match.arg(method, c("args", "env", "body", "def", "sub"))

    args <- nms_vls_to_list(nms, vls)

    switch(method,
           "args"=,
           "def"={set_default(fun, args)},
           "body"=,
           "sub"={section_fun_sub_worker(fun, args)},
           "env"={section_fun_env_worker(fun, args)}
           )        
}



#' @rdname section_fun
#' @export
section_fun_sub <- function(fun, nms, vls=NULL, envir=parent.frame()){

    args <- nms_vls_to_list(nms, vls)
    section_fun_sub_worker(fun, args)
}


section_fun_sub_worker <- function(fun, args, envir=parent.frame()){

    fmls  <- formals(fun)
    idx <- match(names(args), names(fmls))
    idx <- idx[!is.na(idx)]
    if (length(idx) > 0){
        fmls  <- fmls[-idx]
    }


    nn <- names(fmls)
    vv <- paste0(fmls)
    nc <- nchar(vv)
    fmls2 <- ""
    
    if (length(fmls)>0){
        fmls2 <-
            sapply(1:length(fmls), function(i){
                if (nc[i] > 0){
                    paste0(nn[i], "=", vv[i])
                } else {
                    nn[i]
                }    
            })
    }

    hd <- paste0("function(", paste0(fmls2, collapse = ", "), ")")

    aux <- sapply(1:length(args),
                  function(i){
                      nm <- names(args)[i]
                      ss <- deparse(args[[i]])
                      ss <- paste0(ss, collapse = " ")
                      paste0(nm, " = ", ss)
                  })
    
    bd1 <- paste0("\n ## section\n ", paste0(aux, collapse=";\n "), "\n ## section (end)")
    bd1
    
    bd2 <- deparse(body(fun))

    bd2 <- bd2[2:(length(bd2) - 1)]
    bd2 <- gsub("^ *", "", bd2) ## Remove leading whites
    bd2 <- paste0(paste0(bd2, collapse="\n "))
    
    bd <- paste0("\n{ ", paste0(c(bd1, bd2), collapse="\n "), "\n}")
    
    ff  <- paste0(c(hd, bd), collapse="")
    out <- eval(parse(text=ff))
    environment(out) <- environment(fun)
    out

}


nms_vls_to_list <- function(nms, vls){
    ## cat("nms: "); print(nms); cat(" vls: "); print(vls)

    all_has_names <- function(x){
        (!is.null(names(x))) &&
            all(nchar(names(x)) > 0)            
    }
    if (!(is.atomic(nms) || is.list(nms))){
        stop("'nms' must be list or atomic")
    }
    if (inherits(nms, "list")){
        if (!is.null(vls)) {
            warning("vls ignored")
        }
        out <- nms
    } else {
        if (all_has_names(nms)){
            vls <- as.list(nms)
            out <- vls
        } else {                   
            if (length(nms) != length(vls)){
                stop("'nms' and 'vls' must have same length\n")
            }
            vls <- as.list(vls)
            names(vls) <- nms
            out <- vls
        }        
    }
    return(out)
}




#' @rdname section_fun
#' @export
section_fun_env <- function(fun, nms, vls=NULL) {

    args <- nms_vls_to_list(nms, vls)
    section_fun_env_worker(fun, args)
    ##.partial(fun, args)
}


section_fun_env_worker <- function(fun, args){
    fun <- as.section_function(fun)
    apply_args(fun, args)
}



as.section_function <- function(fun) {
  if (!inherits(fun, c("function", "section_function")))
    stop("Can not create section_function\n")

  if (inherits(fun, 'section_function')) {
    section_function_update(fun)
  } else {
    from <- parent.frame()
    section_function_create(fun, from)
  }
}


section_function_create <- function(fun, from = parent.frame()) {

  arg_env <- new.env(parent = emptyenv())
  assign('args',     list(), envir = arg_env)
  assign('args_end', list(), envir = arg_env)
  
  fmls <- get_formals(fun)
  arg_getter <- get_args(arg_env)
  do_section_function(fun, arg_env, fmls, from)  
}


section_function_update <- function(fun, from = parent.frame()) {
  #cat("section_function_update\n")
  arg_env <- clone_environment(attr(fun, "arg_env"))
  fmls <- get_formals(fun)
  #cat("formals:\n"); print(fmls)

  fmls[names(get_section(fun))] <- NULL
  
  fun_orig <- environment(fun)$fun
  arg_getter <- get_args(arg_env)
  do_section_function(fun_orig, arg_env, fmls, from)
}

do_section_function <- function(fun, arg_env, fmls, from){
    ## fmls <- get_formals(fun)
    arg_getter <- get_args(arg_env)
    
    new_fun <- function() {}    
    formals(new_fun) <- fmls
    body(new_fun) <- bquote(
    {
        . <- "use get_section(function_name) to see section"
        . <- "use get_fun(function_name) to see original function"
        args <- arg_getter()
        do.call(.(fun), args)
    },
    list(fun = substitute(fun, from))
    )
  
  structure(new_fun, class = c('section_function', 'function'), arg_env = arg_env)
}




get_formals <- function(fun){
  if (is.primitive(fun)) {
    fmls <- formals(args(fun))
  } else {
    fmls <- formals(fun)
  }
  fmls
}

## SOME UTILITIES

#' @rdname section_fun
#' @export
get_section <- function(object){
    if (!inherits(object, "section_function"))
        stop("'object' must be section_function object\n")
    attr(object, "arg_env")$args
}

#' @rdname section_fun
#' @export
get_fun <- function(object){
    if (!inherits(object, "section_function"))
        stop("'object' must be section_function object\n")
    environment(object)$fun
}

#' @export
print.section_function <- function(x, ...){
  x2 <- x
  attributes(x2) <- NULL
  print.default(x2)
  return(invisible(x))
}


#' @export
summary.section_function <- function(object, ...){

    cat("Original function: \n")
    print(get_fun(object))

    cat("Section: \n")
    str(get_section(object))

    return(invisible(object))
}


get_args <- function(added_env) {
    function() {
        ## cat("in get_args:\n")
        env <- parent.frame()
        args <- names(formals(sys.function(sys.parent(1))))
        ## cat("args:\n ");print(args) ## To be specified in call
        
        if (length(args) > 0) {
            vals <- mget(args, envir = env)
            ## cat("vals:\n"); str(vals)  ## To be specified in call
            
            ellipsis <- names(vals) == '...'
            if (any(ellipsis)) {
                vals <- append(vals, eval(quote(list(...)), env), which(ellipsis))
                vals[ellipsis] <- NULL
            }
            vals <- vals[!vapply(vals, is_missing_arg, logical(1))]
        } else {
            vals <- NULL
        }
        
        ## cat("when done: \n")
        ## str(list(args=added_env$args, vals=vals, args_end=added_env$args_end))   
        out <- c(added_env$args, vals, added_env$args_end)
        ## str(out)
        out
  }
}


apply_args <- function(fun, args, last = FALSE) {
  fmls <- formals(fun)
  #cat("+++ formals: \n"); print(fmls)
  arg_env <- attr(fun, 'arg_env')

  fmls[names(args)] <- NULL
  formals(fun) <- fmls[!names(fmls) %in% names(args)]
  #cat("+++ +++ formals: \n"); print(formals(fun))
  if (last) {
    assign('args_end', append(args, arg_env$args_end), envir = arg_env)
  } else {
    ##arg_env <- clone_environment(arg_env)
    common <- intersect(names(arg_env$args), names(args))

    if (length(common) > 0)
      arg_env$args[common] <- NULL    
    assign('args', append(arg_env$args, args), envir = arg_env)
  }
  structure(fun, class = c('section_function', 'function'), arg_env = arg_env)
}



is_missing_arg <- function(x) {
    identical(x, quote(expr = ))
}

clone_environment <- function(e1){
  as.environment(as.list(e1, all.names=TRUE))
}
