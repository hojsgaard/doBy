#' Align and combine fixed-effect coefficients from multiple models
#'
#' @description
#' Extracts and aligns the fixed-effect estimates from a list of fitted model objects,
#' returning them in a single tidy data frame with consistent columns for easy comparison.
#' Works with a mix of model types such as `lm`, `glm`, `gls`, `lmer`, etc.
#' 
#' For models without p-values (e.g., `lmer`), the function computes approximate
#' Wald statistics and two-sided normal p-values.
#'
#' @param models A named list of fitted model objects. Each element should be a
#' model that can be passed to `broom::tidy()`.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{model}{The name of the model (from the list).}
#'   \item{term}{The term name (coefficient).}
#'   \item{estimate}{The estimated coefficient.}
#'   \item{std.error}{The standard error.}
#'   \item{statistic}{The Wald statistic (estimate / std.error).}
#'   \item{p.value}{Two-sided normal p-value.}
#' }
#'
#'
#' @examples
#' # Example using the built-in CO2 dataset
#' data(CO2)
#'
#' # Fit models
#' lm_fit  <- lm(uptake ~ conc + Type + Treatment, data = CO2)
#' glm_fit <- glm(uptake ~ conc + Type + Treatment, family = Gamma(identity), data = CO2)
#'
#' # Combine estimates
#' models_list <- list(lm = lm_fit, glm = glm_fit)
#' result <- align_coefs(models_list)
#' print(result)
#'
#' @importFrom purrr imap_dfr
#' @export
align_coefs <- function(models) {
  purrr::imap_dfr(models, function(mod, label) {
    out <- broom::tidy(mod)
    
    # Filter to fixed effects only if 'effect' column exists
    if ("effect" %in% names(out)) {
      out <- out |> dplyr::filter(.data$effect == "fixed")
    }
    
    # Add missing columns with NA
    if (!"std.error" %in% names(out)) out$std.error <- NA_real_
    if (!"statistic" %in% names(out)) out$statistic <- NA_real_
    if (!"p.value" %in% names(out)) out$p.value <- NA_real_

    # Compute missing statistic and p.value if possible
    out |> 
      dplyr::mutate(
        statistic = ifelse(is.na(.data$statistic) & !is.na(.data$estimate) & !is.na(.data$std.error),
                           .data$estimate / .data$std.error, .data$statistic),
        p.value = ifelse(is.na(.data$p.value) & !is.na(.data$statistic),
                         2 * (1 - pnorm(abs(.data$statistic))),
                         .data$p.value),
        model = label
      ) |> 
      dplyr::select(dplyr::all_of(c("model", "term", "estimate", "std.error", "statistic", "p.value")))
  })
}



##' @title Add predicted values of different types to dataframe
##' 
##' @param data dataframe or tibble
##' @param model model object
##' @param var name of new variable in dataframe / tibble
##' @param type type of predicted value
##' @param transformation A possible transformation of predicted variable, e.g. reciprocal(), log() etc
##' @return dataframe / tibble
##' @author Søren Højsgaard
##' @examples
##' data(cars)
##' lm1 <- lm(dist ~ speed + I(speed^2), data=cars)
##' lm1 |> response() |> head()
##' cars <- cars |> add_pred(lm1)
##' cars |> head()
##' cars <- cars |> add_resid(lm1)
##' cars
##' 
##' @export
add_pred <- function (data, model, var = "pred", type = NULL, transformation=NULL) 
{
    pred2 <- function (model, data, type = NULL) 
    {
        if (is.null(type)) {
            stats::predict(model, data)
        }
        else {
            stats::predict(model, data, type = type)
        }
    }

    pp <- pred2(model, data, type = type)
    if (!is.null(transformation)){
        pp <- transformation(pp)
    }
    data[[var]] <- pp
    data
}

## ##' @title Reciprocal function
## ##' @description  A function returning the reciprocal of its argument
## ##' @param x An R object for which 1/x makes sense
## ##' @author Søren Højsgaard
## ##' @export
## reciprocal <- function(x){
##   1/x    
## }

## ##' @title Power function
## ##' @description  A function returning x raised to the power p.
## ##' @param x An object for which x^p makes sense
## ##' @param p A power
## ##' @author Søren Højsgaard
## ##' @export
## pow <- function(x, p){
##   x^p    
## }


##' @title Add residuals of different types to dataframe
##' 
##' @param data dataframe or tibble
##' @param model model object
##' @param var name of new variable in dataframe / tibble
##' @param type type of residual value 
##' @return dataframe / tibble
##' @author Søren Højsgaard
##' @examples
##' data(cars)
##' lm1 <- lm(dist ~ speed + I(speed^2), data=cars)
##' lm1 |> response() |> head()
##' cars <- cars |> add_pred(lm1)
##' cars |> head()
##' cars <- cars |> add_resid(lm1)
##' cars 
##'
##' @export
add_resid <- function (data, model, var = "resid", type) {

    resid2 <- function(model, type){
        UseMethod("resid2")
    }
    resid2.lm <- function(model,
                          type=c("working", "response", "deviance", 
                                 "pearson", "partial", "rstandard", "rstudent")){
        type <- match.arg(type)

        if (identical(type, "rstandard")){
            return(stats::rstandard(model))                        
        }
        
        if (identical(type, "rstudent")){
            return(stats::rstudent(model))                       
        }

        return(stats::residuals(model, type=type))
    }

    resid2.merMod <- function(model,
                              type=c("deviance", "response")){
        return(residuals(model, type=type))
    }
    
    
    if (missing(type))
        type="working"


    data[[var]] <- resid2(model, type)
    data
}


##' @title Get response variable from model
##' @param object lm or glm object 
## ' @param data dataframe or tibble
## ' @param model model object
## ' @param var name of new variable in dataframe / tibble
## ' @param type type of residual value
##' @examples
##' data(cars)
##' lm1 <- lm(dist ~ speed + I(speed^2), data=cars)
##' lm1 |> response() |> head()
##' cars <- cars |> add_pred(lm1)
##' cars |> head()
##' cars <- cars |> add_resid(lm1)
##' cars 
##' @export
response <- function(object){

    is_lm <- function(object) {
        identical(class(object), "lm")
    }
    
    is_glm <- function(object) {
        cls <- class(object)
        (all(c("lm", "glm") %in% cls)) && (length(cls) == 2)
    }
    
    obs <- function(object){
        UseMethod("obs")
    }
    
    obs.lm <- function(object) {
        obs <- model.response(model.frame(object))        
        
        if (is_glm(object)){
            wgt <- unname(model.weights(model.frame(object)))
            if (!is.null(wgt)) {
                obs <- obs * wgt
            }
        }
        return(obs)
    }
    return(obs(object))
}



##' @title Add interaction columns to data frame
##' @param .data dataframe
##' @param .formula right hand sided formula
##' @return dataframe
##' @author Søren Højsgaard
##' @export
add_int <- function(.data, .formula) {

    ff <- rhsf2list(.formula)
    lapply(ff, function(g){
        if (length(g)>1){
            var <- paste(g, collapse="_")
            ia <- apply(.data[,g], 1, function(r) paste(r, collapse="_"))
            .data[[var]] <<- ia
        }})
    .data
}


rhsf2list <- function (.formula) {
    if (is.character(.formula)) 
        return(list(.formula))
    if (is.numeric(.formula)) 
        return(lapply(list(.formula), "as.character"))
    if (is.list(.formula)) 
        return(lapply(.formula, "as.character"))
    .formula0 <- .formula[[length(.formula)]]
    .formula1 <- unlist(strsplit(paste(deparse(.formula0), collapse = ""), 
                             " *\\+ *"))
    .formula2 <- unlist(lapply(.formula1, strsplit, " *\\* *| *: *| *\\| *"), 
                    recursive = FALSE)
    .formula2
}
