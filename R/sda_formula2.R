#' Shrinkage Discriminant Analysis with a Formula Interface
#'
#' Fits a shrinkage discriminant analysis model using a formula and a data
#' frame. The fitted object stores the formula information and design-matrix
#' attributes required by [update()] and [predict()].
#'
#' @param formula A model formula. The left-hand side specifies the class
#'   variable and the right-hand side specifies the predictors. The shorthand
#'   `.` may be used on the right-hand side.
#' @param data. A data frame containing the response and predictor variables
#'   used in `formula`.
#' @param diagonal Logical. If `TRUE`, a diagonal covariance matrix is used.
#'   The default is `FALSE`.
#' @param ... Additional arguments passed to [sda::sda()].
#'
#' @details
#' `sda_formula()` constructs a model frame from `formula`, extracts the
#' response, and constructs a model matrix for the predictors. The intercept
#' column produced by [stats::model.matrix()] is removed before the matrix is
#' passed to [sda::sda()].
#'
#' The returned object stores the expanded formula, terms object, training
#' design matrix, predictor names, contrasts, and factor levels. Consequently,
#' formulas containing `.` can be modified with [update()], and new data can be
#' encoded in the same way as the training data.
#'
#' @return An object inheriting from classes `sda_formula` and `sda`. In
#'   addition to the components returned by [sda::sda()], the object contains:
#'   \describe{
#'     \item{call}{The matched call containing the expanded formula.}
#'     \item{formula}{The expanded model formula.}
#'     \item{response_name}{The name of the response variable.}
#'     \item{terms}{The model terms object.}
#'     \item{X_train}{The training design matrix without an intercept.}
#'     \item{predictor_names}{The column names of the training design matrix.}
#'     \item{contrasts}{The contrasts used to construct the design matrix.}
#'     \item{xlevels}{The factor levels used when fitting the model.}
#'   }
#'
#' @examples
#' fit <- sda_formula(Species ~ ., data. = iris)
#'
#' predict(fit)
#' predict(fit, newdata = iris[1:10, ])
#'
#' fit_reduced <- update(fit, . ~ . - Petal.Length)
#'
#' @author Søren Højsgaard
#' @export
sda_formula <- function(formula, data., diagonal = FALSE, ...) {
  cl <- match.call()

  mf <- model.frame(formula, data = data.)

  y_train <- model.response(mf)
  response_name <- names(mf)[1L]

  terms_full <- terms(mf)
  formula_expanded <- formula(terms_full)
  tt <- delete.response(terms_full)

  X_train <- model.matrix(tt, data = mf)
  X_train <- X_train[
    ,
    colnames(X_train) != "(Intercept)",
    drop = FALSE
  ]

  fit <- sda::sda(
    Xtrain = X_train,
    L = y_train,
    diagonal = diagonal,
    verbose = FALSE,
    ...
  )

  ## Store the expanded formula so that update() can resolve '.'.
  cl$formula <- formula_expanded

  fit$call <- cl
  fit$formula <- formula_expanded
  fit$response_name <- response_name
  fit$terms <- terms_full
  fit$X_train <- X_train
  fit$predictor_names <- colnames(X_train)
  fit$contrasts <- attr(X_train, "contrasts")
  fit$xlevels <- stats::.getXlevels(terms_full, mf)

  class(fit) <- c("sda_formula", class(fit))
  fit
}


#' Predict from an sda_formula Model
#'
#' Converts `newdata` to the same design-matrix representation used to fit the
#' model and then invokes the prediction method for `sda` objects. If
#' `newdata` is omitted, predictions are obtained for the training data.
#'
#' @param object An object of class `sda_formula`, normally returned by
#'   [sda_formula()].
#' @param newdata An optional data frame containing observations for which
#'   predictions are required. Factor levels must be compatible with those
#'   used to fit the model.
#' @param verbose Logical. Should the underlying `sda` prediction method print
#'   progress information? The default is `FALSE`.
#' @param ... Additional arguments passed to the prediction method for `sda`
#'   objects.
#'
#' @return The prediction object returned by the prediction method for `sda`
#'   objects.
#'
#' @examples
#' fit <- sda_formula(Species ~ ., data. = iris)
#' predict(fit, newdata = iris[1:5, ])
#'
#' @method predict sda_formula
#' @export
predict.sda_formula <- function(object, newdata, verbose = FALSE, ...) {
  if (is.null(object$predictor_names) || is.null(object$X_train)) {
    stop(
      "The fitted object does not contain the stored design-matrix ",
      "information. Refit it using sda_formula().",
      call. = FALSE
    )
  }

  if (missing(newdata)) {
    X_test <- object$X_train
  } else {
    tt <- delete.response(object$terms)

    mf_new <- model.frame(
      tt,
      data = newdata,
      xlev = object$xlevels,
      na.action = na.pass
    )

    X_test <- model.matrix(
      tt,
      data = mf_new,
      contrasts.arg = object$contrasts
    )

    X_test <- X_test[
      ,
      colnames(X_test) != "(Intercept)",
      drop = FALSE
    ]

    missing_predictors <- setdiff(
      object$predictor_names,
      colnames(X_test)
    )

    if (length(missing_predictors) > 0L) {
      stop(
        "Predictors missing from newdata: ",
        paste(missing_predictors, collapse = ", "),
        call. = FALSE
      )
    }

    X_test <- X_test[
      ,
      object$predictor_names,
      drop = FALSE
    ]
  }

  ## Remove the wrapper class and dispatch predict() to predict.sda().
  sda_object <- object
  class(sda_object) <- setdiff(class(sda_object), "sda_formula")

  predict(
    sda_object,
    Xtest = X_test,
    verbose = verbose,
    ...
  )
}

