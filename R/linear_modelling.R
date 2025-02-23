
##' @title Add predicted values of different types to dataframe
##' @param data dataframe or tibble
##' @param model model object
##' @param var name of new variable in dataframe / tibble
##' @param type type of predicted value 
##' @return dataframe / tibble
##' @author Søren Højsgaard
##' @export
add_pred <- function (data, model, var = "pred", type = NULL) 
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

    data[[var]] <- pred2(model, data, type = type)
    data
}

##' @title Add residuals of different types to dataframe
##' @param data dataframe or tibble
##' @param model model object
##' @param var name of new variable in dataframe / tibble
##' @param type type of residual value 
##' @return dataframe / tibble
##' @author Søren Højsgaard
##' @export
add_resid <- function (data, model, var = "resid", type) 
{
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

        return(stats::residuls(model))
    }
    data[[var]] <- resid2(model, type)
}

##' @title Returns the response in a regression model.
##' @param object Currently an lm og glm.
##' @return A vector (or a matrix)
##' @author Søren Højsgaard
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
        obs <- unname(model.response(model.frame(object)))        
        
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



