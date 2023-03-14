## Move to doBy
##
## https://stackoverflow.com/questions/70735907/how-to-evaluate-in-a-formula-in-r

##' @title Formula operations and coercion.
##'
##' @description Formula operations and coercion as a supplement to `update.formula()`
##'
##' @name formula_ops
##' 
##' @param x1 Character vector to be coerced to formulas.
##' @param f1,f2 Formulas to be coerced to character vectors.
##' @param object Character vector or formula.
##' @param noint Boolean.
##' @param string Boolean.
##' @param n Positive integer.
##' 
##' @examples
##'
##' formula_poly("z", 2)
##' formula_poly("z", 2, noint=TRUE)
##'
##' any2_rhs_chr(c("a", "b", "z"))
##' any2_rhs_chr(c("a*b", "z"))
##'
##' any2_rhs_chr(y~a+b+z)
##' any2_rhs_chr(y~a+b+z, string=TRUE)
##' any2_rhs_chr(y~a+b+z)
##' any2_rhs_chr(y~a*b+z)
##' any2_rhs_chr(y~a*b+z, string=TRUE)
##'
##' any2_lhs_chr(y~a*b+z)
##' any2_lhs_chr(log(y)~a*b+z)      ## Not what one might expect
##' any2_lhs_chr(cbind(y, u)~a*b+z) ## Not what one might expect
##'
##' formula_chr_to_form(c("a*b", "z"))
##' formula_chr_to_form(c("a*b", "z"), "y")
##' formula_chr_to_form(c("a*b", "z"), "log(y)")
##'
##' formula_add(y~a*b+z, ~-1)
##' formula_add(y~a*b+z, ~a:b)
##'


##' @rdname formula_ops
##' @export
formula_add <- function(f1, f2){

    stopifnot_formula(f1)
    stopifnot_formula(f2)

    ## Right-hand-side    
    f1_rhs <- any2_rhs_chr(f1)
    f2_rhs <- any2_rhs_chr(f2)
    o_rhs <- c(f1_rhs, f2_rhs)

    o_rhs <- o_rhs|> paste0(collapse="+") 
    ## str(o_rhs)
    ## Remove redunancies    
    o_rhs <- simplify_rhs(o_rhs)



    
    ## Left-hand-side
    f1_lhs <- any2_lhs_chr(f1)
    f2_lhs <- any2_lhs_chr(f2)
    
    if ((length(f1_lhs) > 0) && (length(f2_lhs) > 0))
        stop("Can not handle two left-sides\n")

    o_lhs <- c(f1_lhs, f2_lhs)
    formula_chr_to_form(o_rhs, o_lhs)
}


##' @rdname formula_ops
##' @export
formula_poly <- function(x1, n, noint=FALSE){
    if (n > 1){
        b <- paste0(x1, "^", 2:n)
        o <- paste0("I(", b, ")", collapse = "+")
        o <- paste0(x1, "+", o)
    } else {
        o <- x1
    }

    if (noint){
        o <- paste0("-1 +", o)
    }
    formula(paste("~", o))
}

##' @rdname formula_ops
##' @export
formula_nth <- function(f1, n){
    fs <- any2_rhs_chr(f1, string=TRUE)
    fs2 <- paste0("(", fs, ")^", n)
    o <- any2_rhs_frm(fs2)
    o
}

##' @rdname formula_ops
##' @export
formula_to_interaction_matrix <- function(f1){
    aa <- any2_rhs_chr(f1)  |> strsplit(":")
    nms <- unique(unlist(aa))
    mm <- matrix(0, nrow=length(nms), ncol=length(nms), dimnames=list(nms, nms))
    for(i in 1:length(aa)){
        g <- aa[[i]]
        mm[g, g] <- 1
    }
    mm
}


##' @rdname formula_ops
##' @param rhs,lhs right-hand-side and left-hand-side for formula (as characters)
##' @export
formula_chr_to_form <- function(rhs, lhs=character(0)){
    rhs <- to_str(rhs)
    o <- paste0(lhs, "~", rhs)    
    as.formula(o)
}

## UTILITIES

to_str <- function(x, collapse="+"){
    paste0(x, collapse=collapse)
}

##' rdname formula_ops
##' @export
terms_labels <- function(f1){
    terms(f1)  |> attr("term.labels") |> sort()
}


simplify_rhs <- function(object){
    UseMethod("simplify_rhs")
}

simplify_rhs.formula <- function(object){
    l <- terms_labels(object)
    to_str(l) |> any2_rhs_frm()
}

simplify_rhs.character <- function(object){
    o <- any2_rhs_frm(object)  |>
        terms_labels() |>
        to_str()
    o
}

## Bad name
formula_rhs_to_chr <- function(f1, string=TRUE){
    f1 <- terms_labels(f1)
    if (length(f1) == 0)
        f1 <- "-1"
    if (string)
        f1 <- f1 |> to_str()
    f1
}


is_rhsf <- function(x){
    is(x, "formula") && ((x |> terms() |> attr("response")) == 0)
}

stopifnot_rhsf <- function(x){
    if (!is_rhsf(x))
        stop("argument is not a right-hand-sided formula\n")
}

stopifnot_formula <- function(a){
    if (!is(a, "formula"))
        stop("argument is not a formula")
}

stopifnot_chr <- function(a){
    if (!is(a, "character"))
        stop("argument is not a character")
}



## RETURNING FORMULAS

##' @export
any2_rhs_frm.character <- function(object){
    paste0("~", object)  |> as.formula()
}

##' @export
any2_rhs_frm.formula <- function(f1){
    formula(delete.response(terms(f1)))    
}

##' @export
any2_lhs_frm.character <- function(object){
    stopifnot_chr(object)
    paste0(object, "~ 1")  |> as.formula()
}

formula_lhs_to_chr <- function(f1){
    if (is(f1, "formula")){
        r <- terms(f1) |> attr("response")
        if (r > 0)
            f1 <- (terms(f1) |> attr("variables"))[[r+1]] |> as.character()
        else
            f1 <- character(0)
    }
    f1
}

##' @export
any2_lhs_frm.formula <- function(f1){
    o <- f1 |> formula_lhs_to_chr()
    as.formula(paste0(o, "~1"))
}


## RETURNING CHARACTERS

##' @export
any2_rhs_chr.character <- function(object, string=TRUE){
    object <- object |> to_str()
    rev(strsplit(object,"\\s~\\s")[[1]])[1]
}

##' @export
any2_rhs_chr.formula <- function(object, string=TRUE){
    formula_rhs_to_chr(object, string=string)    
}

##' @export
any2_lhs_chr.character <- function(object, string=TRUE){
    object2 <- strsplit(object,"\\s~\\s")[[1]]
    if (length(object2) == 2)
        object2[1]
    else
        character(0)
}

##' @export
any2_lhs_chr.formula <- function(object, string=FALSE){
    r <- terms(object) |> attr("response")
    if (r > 0)
        object <- (terms(object) |> attr("variables"))[[r+1]] |> as.character()
    else
        object <- character(0)
    object
}


## EXPORTED

##' @rdname formula_ops
##' @export
any2_rhs_frm <- function(object){
    UseMethod("any2_rhs_frm")    
}

##' @rdname formula_ops
##' @export
any2_lhs_frm <- function(object){
    UseMethod("any2_lhs_frm")
}

##' @rdname formula_ops
##' @export
any2_rhs_chr <- function(object, string=FALSE){
    UseMethod("any2_rhs_chr")
}

##' @rdname formula_ops
##' @export
any2_lhs_chr <- function(object, string=FALSE){
    UseMethod("any2_lhs_chr")
}


## ##' @export
## formula_to_rhs <- function(f1){
##     terms(f1) |> delete.response() |> formula()
## }

## FIXME: REPLACE
## formula_chr_to_rhs <- function(x1){
##     o <- to_str(x1)
##     as.formula(paste("~", o))
## }



