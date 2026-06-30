#' @title Model stability for glm objects
#' @description A way of assessing variability in selected models
#'     across different datasets.
#' @param data. A data frame
#' @param model A glm object
#' @param M Number of searches
#' @param method Method for generating data. Either `subgroup` like in
#'     bootstrapping or `resample` by resampling the entire datasets
#'     with replications.
#' @param mc.cores Number of cores, defaults to getOption("mc.cores", 2L)
#' @param ... Additional arguments to be passed to \code{\link{step}}
#' @note This is a recent addition. Suggestions and comments are
#'     welcome. The addition may be removed without warning.
#' @details From `data.`, `M` datasets are generated. Options are
#'
#' * "resamples": M datasets with resamples of data with replacement, i.e. bootstrap samples.
#'
#' * "subgroups": M datasets with subgroups each consisting of n-M randomly selected observations.
#'
#' On each of these datasets, a stepwise selection
#'     (using `step`) is performed.
#' @return A list with several slots of information. The most
#'     important ones:
#'
#' * `rhs_matrix` : A matrix representatation of
#'     formulas for the different models. As many rows as there are
#'     predictors, as many columns as there are different models (the
#'     same model might be selected on several datasets.)
#'
#' * `model_freq` : How many time each model is selected.
#'
#' * `pred_freq` : How many time does each predictor appear in a
#'     model.
#'
#' * `model_parm` : How many parameters are in each model.
#'
#' @examples
#'
#' set.seed(1234)
#' dat <- personality[,1:20]
#' idx <- sample(1:nrow(dat), 0.6*nrow(dat))
#' train <- dat[idx, ]
#' test <- dat[-idx, ]
#' fit <- stats::glm(agreebl ~ ., data = train)
#'
#' M <- 8
#' 
#' #stab <- doBy::model_stability_glm(data=train, model=fit, M=M, method="resample", mc.cores=1)
#' stab <- doBy::model_stability_glm(data=train, model=fit, M=M, method="subgroups", mc.cores=1)
#' stab
#'
#' formula_list <- formula(stab)
#' fit_list <- formula(stab, fit=TRUE)
#'
#' formula_list
#' stab
#'
#' (cv.train <- doBy::cv_glm_fitlist(train, fit_list, K=5))
#'
#' (cv.test <- doBy::cv_glm_fitlist(test, fit_list, K=5))
#' ylim <- range(c(cv.train, cv.test))
#' par(mfrow=c(1,2))
#' plot(stab$model_parm, cv.train, ylim=ylim)
#' plot(stab$model_parm, cv.test, ylim=ylim)

#'
#' 
#' @export
model_stability_glm <- function(data., model, M=10, method=c("resample", "subgroups"), mc.cores= getOption("mc.cores", 2L), ...){
    method <- match.arg(method)
    data_list <- generate_data_list(data., M=M, method=method)
    
    lhs <- as.character(model$formula[[2]])
        
    fit_list <- data_list |>
        parallel::mclapply(function(dat..){
            model <- update(model, data=dat..)
            step(model, ...)
        }, mc.cores=mc.cores)
    
    fit_list2 <- parallel::mclapply(
                               fit_list, function(x){
                                   update(x, data=data.)
                               }, mc.cores=mc.cores
                           )
    rhs_nms  <- get_rhs_nms(model)    
    rhs_list <- get_predictor_list(fit_list2)
    rhs_matrix <- set_list2matrix(rhs_list, aggregate=TRUE)
    rhs_raw    <- set_list2matrix(rhs_list, aggregate=FALSE)    
    rhs_matrix <- as.data.frame(rhs_matrix)
    freq <- rhs_matrix$Freq__
    rhs_matrix$Freq__ <- NULL
    
    rhs_matrix = as(t(rhs_matrix), "dgCMatrix")
    rhs_raw = as(t(rhs_raw), "dgCMatrix")
    
    loc <- get_location(rhs_raw, rhs_matrix)    
    
    out <- list(
        call = match.call(),
        model = model,
        data. = data.,
        data_list = data_list,
        formula = formula(model),
        fit = fit_list2,        
        rhs_raw = rhs_raw,
        M = M,
        loc = loc,
        lhs = lhs,
        ##aic = sapply(fit_list2, function(x)AIC(x)),
        pred_freq = unname(rowSums(rhs_matrix)),
        model_parm = colSums(rhs_matrix),
        rhs_matrix = rhs_matrix,
        model_freq = freq)
    
    class(out) <- "model_stability_glm_class"
    out
}


#' @export
print.model_stability_glm_class <- function(x, ...){
  x$data. <- NULL
  print.default(x[c("rhs_matrix", "model_freq", "pred_freq", "model_parm")])    
}

#' @export
formula.model_stability_glm_class <- function(x, fit=FALSE, unique=TRUE, text=FALSE, ...){

    if (fit){
        text <- FALSE
    }
    
    f <- get_formulas(x, unique=unique, text=text)
    if (!fit){
        f
    } else {
        cl <- x$model$call
        lapply(f, function(z) {
            cl$formula <- z
            out <- update(eval(cl), data=x$data.)
            out
        }
        )
    }
}


#' @export
summary.model_stability_glm_class <- function(object, ...){

    freq_pred <- object$rhs_raw |> rowSums()
    num_pred <- object$rhs_matrix |> colSums()

    out <- list(number_of_models=ncol(object$rhs_raw),
                number_of_unique_models=ncol(object$rhs_matrix),                
                number_of_predictors=num_pred, 
                frequency_of_predictors=freq_pred)
    
    class(out) <- "model_stability_glm_summary_class"
    out
}



#' @export
print.model_stability_glm_summary_class <- function(x, ...){
    print.default(unclass(x))
}

#' @title Get formulas from model_stability_glm_class object
#' @param object A model_stability_glm_class object
#' @param unique If TRUE, return unique models
#' @param text If TRUE, return text (rather than formula).
#' @note This is a recent addition. Suggestions and comments are
#'     welcome. The addition may be removed without warning.
#' 
#' @export
get_formulas <- function(object, unique=TRUE, text=FALSE){

    if (unique){
        mmm <- object$rhs_matrix        
    } else {
        mmm <- object$rhs_raw        
    }

    
    rhs2formula <- function(object, i, text=FALSE) {
        out <- paste0(object$lhs, "~", paste0(names(which(mmm[,i]==1)), collapse="+"))
        if (!text)
            as.formula(out)
        else
            out
    }

    out <- lapply(1:ncol(mmm),
                  function(i){
                      rhs2formula(object, i, text=text)
                  })
    return(out)
}





#' @title Cross-validation for list of glm objects
#' 
#' @param data. A data frame
#' @param fit_list A list of glm objects
#' @param K Number of folds in cross validation.
#'
#' @details A wrapper for calling boot::cv.glm for each model in
#'     fit_list.
#' @importFrom boot cv.glm
#' @export
cv_glm_fitlist <- function(data., fit_list, K=10){
    fit_list <-
        lapply(fit_list,
               function(fit){
                   if(!inherits(fit, "glm"))
                       stop("fit must be an glm object")
                   update(fit, data=data.)
               })

    cv.error <-
        sapply(fit_list,
               function(fit){
                   boot::cv.glm(data., fit, K=K)$delta[1] 
               })
    
    return(cv.error)    
}




get_rhs_nms <- function(model){
  nms <- names(eval(model$call$data))
  lhs <- as.character(formula(model)[[2]])
  rhs_nms <- setdiff(nms, lhs)
  rhs_nms      
}

get_location <- function(rhs_raw, rhs_matrix){
    u <- apply(rhs_raw, 1, paste0, collapse='')
    z <- apply(rhs_matrix, 1, paste0, collapse='')

    loc <- sapply(u, function(i){
        (i == z)  |> which()
    })
    names(loc) <- NULL
    loc
}


get_predictor_list <- function(x){
  rhs_list <-
    lapply(x,
           function(s){
             s |> terms() |> attr("term.labels")  
           })
  rhs_list
}


get_rhs_matrix <- function(x, aggregate=FALSE){
  if (!inherits(x, "model_stability_glm_class"))
    stop("'x' is not model_stability_glm_class\n")
  rhs_list <- get_predictor_list(x$fit)
  rhs_raw <- set_list2matrix(rhs_list, aggregate=aggregate)
  t(rhs_raw)
}


#' @title Matrix representatation of list of vectors and vice versa
#' @param set_list list of vectors
#' @param aggregate should the vectors be aggregated
#' @param set_matrix matrix representatation
#' @note This is a recent addition. Suggestions and comments are
#'     welcome. The addition may be removed without warning.
#' 
#' @name set_list_set_matrix
#'
#' @examples
#' l <- list(c(1,2,3), c(3,2,4), c(3,2,4))
#' m1 <- set_list2matrix(l)
#' m1
#' matrix2set_list(m1)
#' 
#' m2 <- set_list2matrix(l, aggregate=TRUE)
#' m2
#' matrix2set_list(m2)
#' 
#' 
#' @export
set_list2matrix <- function(set_list, aggregate=FALSE){  
    aggregate_matrix <- function(mat){
        
        if (ncol(mat) > 0){
            mat <- mat |> as.data.frame()  |> table() |> as.data.frame.table()
            mat <- dplyr::filter(mat, .data$Freq>0)
            mat <- mat[order(mat$Freq, decreasing=TRUE),]
            mat <- sapply(mat, 
                          function(o){
                              as.numeric(as.character(o))
                          })
            
            if (is.null(dim(mat)))
                mat <- as.data.frame(t(mat))
            else
                mat
        } else { 
            as.data.frame(cbind(Freq=nrow(mat)))        
        }
    }
    
    ## Get unique terms across all lists
    unique_terms <- unique(unlist(set_list))
  
    ## Create an empty matrix M with 3 rows and columns equal to the number of unique terms
    M <- matrix(0, nrow = length(set_list), ncol = length(unique_terms), 
                dimnames = list(NULL, unique_terms))
    
    ## Iterate over each list and mark the presence of terms in the matrix
    for (i in seq_along(set_list)) {
        ##print(set_list[[i]])
        M[i, set_list[[i]]] <- 1
    }

    if (aggregate) {
        M2 <- aggregate_matrix(M)
        colnames(M2) <- c(colnames(M), "Freq__")
        return(M2)
    }
    else
        return(M)
}


#' @rdname set_list_set_matrix
#' @param set_matrix matrix representatation
#' @export
matrix2set_list<- function(set_matrix){
    has_freq <- "Freq__" %in% colnames(set_matrix)
    if (has_freq){
        set_matrix <- exclude_column_by_name(repeat_rows(set_matrix, set_matrix[,"Freq__"]), "Freq__")
    }
      nr <- nrow(set_matrix)
    out <- vector("list", nr)
    nms <- colnames(set_matrix)
    nms <- if(is.null(nms)) 1:ncol(set_matrix) else nms
    ##print(out); cat("nms:\n"); print(nms)
    
    for (i in 1:nrow(set_matrix)){
        ri <- set_matrix[i,]
      ##  print(ri)
        idx <- which(abs(ri)> 1e-5)
        ##cat("idx:\n");print(idx);print(nms[idx])
        out[[i]] <- nms[idx]
    }
    return(out)
}

repeat_rows <- function(M, v) {
  # Check that the length of v matches the number of rows in M
  if (length(v) != nrow(M)) {
    stop("The length of vector v must match the number of rows in matrix M.")
  }
  
  # Create an empty list to store the repeated rows
  repeated_rows <- vector("list", length = sum(v))
  
  # Fill the list with repeated rows
  index <- 1
  for (i in seq_along(v)) {
    for (j in seq_len(v[i])) {
      repeated_rows[[index]] <- M[i, , drop = FALSE]
      index <- index + 1
    }
  }
  
  # Combine the list into a new matrix
  new_matrix <- do.call(rbind, repeated_rows)
  return(new_matrix)
}

exclude_column_by_name <- function(M, colname) {
  # Check if the column name exists in the matrix
  col_index <- which(colnames(M) == colname)
  
  if (length(col_index) == 0) {
    stop(paste("Column", colname, "does not exist in the matrix."))
  }
  
  # Exclude the column by name
  M <- M[, -col_index, drop = FALSE]
  
  return(M)
}

## modelr::crossv_kfold(dat, k=4)[[1]][[1]] |> as.data.frame()

#' @title Generate data list
#' @param data. A data frame
#' @param M Number of folds
#' @param method Method for generating data
#' @details There are n rows in data. The function returns a list of M
#'     datasets. If method="subgroups" each dataset in the list has
#'     (approximately) n-M randomly selected rows from data. If
#'     method="resample" each dataset has n rows obtained by sampling
#'     with replacement from the original data. The function is
#'     heavily inspired by the boot::cv.glm function.
#' @note This is a recent addition. Suggestions and comments are
#'     welcome. The addition may be removed without warning.
#' 
#'
#' @examples
#' dat <- cars[1:15,]
#' set.seed(1411)
#' dl1 <- generate_data_list(dat, 4)
#' dl1[[1]]
#' dl1 |> sapply(nrow)
#' dl2 <- generate_data_list(dat, 4, method="resample")
#' dl2 |> sapply(nrow)
#' 
#' @export
generate_data_list <- function(data., M, method=c("subgroups", "resample")){
  
  method <- match.arg(method)
  
  sample0 <- function (x, ...) {
    x[sample.int(length(x), ...)]
  }
  
  out <- as.list(rep(NA, M))
  
  switch (method,
          "subgroups" = {
            n <- nrow(data.)
            f <- ceiling(n/M)
            s <- sample0(rep(1L:M, f), n)
            ms <- max(s)
            ##str(list(n=n, f=f, s=s, ms=ms))

            for (i in seq_len(ms)) {
              j.in <- seq_len(n)[(s != i)]
              di <- data.[j.in, , drop = FALSE]
              out[[i]] <- di
            }
          },
          "resample" = {
            for (i in 1:M){
              j.in <- sample(nrow(data.), replace=TRUE) |> sort()
              di <- data.[j.in, , drop = FALSE]
              out[[i]] <- di              
            }
          }
  )
  out
}


