#' Plot classification regions for LDA and QDA models
#'
#' Creates two-dimensional classification-region plots for an object returned
#' by [MASS::lda()] or [MASS::qda()].
#'
#' If two predictors are selected, one plot is produced. If more than two
#' predictors are selected, plots are produced for all pairwise combinations.
#'
#' @param object An object returned by [MASS::lda()] or [MASS::qda()].
#'
#' @param vars A character vector containing predictor names, or a numeric
#'   vector containing predictor indices. Numeric indices refer to positions
#'   among the predictors in the model formula, not to column positions in
#'   `data`. If `NULL`, all predictors in the model are used.
#'
#' @param data An optional data frame containing the variables used in the
#'   model. If `NULL`, the function attempts to recover the data from the
#'   original model call.
#'
#' @param type Character string specifying how the classification regions
#'   are constructed. Possible values are:
#'
#'   * `"refit"`: fit a separate LDA or QDA model using only the two
#'     predictors displayed in each plot.
#'   * `"slice"`: retain the original fitted model and construct a
#'     two-dimensional slice by holding all other predictors fixed.
#'
#'   The default is `"refit"`.
#'
#' @param at An optional named list specifying values at which predictors not
#'   displayed in a slice plot are held fixed. Predictors not included in
#'   `at` are held at their mean if numeric and at their most frequent value
#'   if categorical. This argument can only be used when `type = "slice"`.
#'
#' @param n Integer giving the number of grid points along each axis.
#'   Larger values produce smoother classification regions but require more
#'   computation.
#'
#' @param alpha Numeric value between 0 and 1 giving the transparency of the
#'   classification-region background.
#'
#' @param point_size Numeric value giving the size of the observed data
#'   points.
#'
#' @param show_points Logical. If `TRUE`, the observed data points are added
#'   to each plot.
#'
#' @param plot Logical. If `FALSE`, multiple plots are returned as a named
#'   list. If `TRUE`, multiple plots are combined using
#'   [patchwork::wrap_plots()] (and returned as a named list).
#'
#' @param ncol Optional integer giving the number of columns in the combined
#'   plot when `combine = TRUE`. If `NULL`, a suitable number of columns is
#'   chosen automatically.
#'
#' @details
#' With `type = "refit"`, a separate model is fitted for each displayed
#' predictor pair. For example, if the selected predictors are `x1`, `x2`,
#' and `x3`, the function fits models using `x1 + x2`, `x1 + x3`, and
#' `x2 + x3`. Predictors not included in a given pair are omitted from that
#' model.
#'
#' With `type = "slice"`, all plots are based on the original fitted model.
#' The two displayed predictors are varied over a grid, while all remaining
#' predictors are held fixed. Numeric predictors are held at their sample
#' means by default, while factors and character variables are held at their
#' most frequently observed values. These defaults can be overridden using
#' `at`.
#'
#' The observed points displayed in a slice plot retain their actual values
#' for predictors not shown on the axes. Consequently, the points do not
#' necessarily lie in the same slice as the displayed classification regions.
#'
#' For \eqn{p} selected predictors, the function produces
#' \eqn{p(p - 1) / 2} pairwise plots.
#'
#' Only models containing untransformed main-effect predictors are currently
#' supported. Interactions, polynomial terms, and transformed predictors are
#' not supported.
#'
#' @return
#' If exactly two predictors are selected, a [ggplot2::ggplot()] object.
#'
#' If more than two predictors are selected and `combine = FALSE`, a named
#' list of `ggplot` objects.
#'
#' If more than two predictors are selected and `combine = TRUE`, a combined
#' patchwork object.
#'
#' @seealso
#' [MASS::lda()], [MASS::qda()], [patchwork::wrap_plots()]
#'
#' @examples
#' fit <- MASS::lda(
#'     Species ~ Sepal.Length + Sepal.Width + Petal.Length,
#'     data = iris
#' )
#'
#' ## Refit a model using two selected predictors
#' plot_class_region(
#'     fit,
#'     vars = c("Sepal.Length", "Sepal.Width"),
#'     data = iris
#' )
#'
#' ## Numeric indices refer to positions among the model predictors
#' plot_class_region(
#'     fit,
#'     vars = c(1, 3),
#'     data = iris
#' )
#'
#'
#' ## Combine all pairwise plots
#' plot_class_region(
#'     fit,
#'     vars = c(1, 2, 3),
#'     data = iris
#' )

#' ## Return all pairwise plots as a named list for increased control
#' plots <- plot_class_region(
#'     fit,
#'     vars = c(1, 2, 3),
#'     data = iris
#' )
#' patchwork::wrap_plots(plots, nrow = 1)
#' 
#' ## Construct a slice through the original fitted model
#' plot_class_region(
#'     fit,
#'     vars = c("Sepal.Length", "Petal.Length"),
#'     data = iris,
#'     type = "slice"
#' )
#'
#' ## Specify the value of a predictor held fixed in the slice
#' plot_class_region(
#'     fit,
#'     vars = c("Sepal.Length", "Petal.Length"),
#'     data = iris,
#'     type = "slice",
#'     at = list(Sepal.Width = 3)
#' )
#'
#' @export
plot_class_region <- function(object,
                              vars = NULL,
                              data = NULL,
                              type = c("refit", "slice"),
                              at = NULL,
                              n = 250,
                              alpha = 0.25,
                              point_size = 2.4,
                              show_points = TRUE,
                              plot = TRUE,
                              #combine = FALSE,
                              ncol = NULL) {

    type <- match.arg(type)

    if (!inherits(object, c("lda", "qda"))) {
        stop(
            "'object' must be an object returned by ",
            "MASS::lda() or MASS::qda()."
        )
    }

    form <- formula(object)
    predictors <- attr(terms(form), "term.labels")

    if (any(grepl("[:*^()]", predictors))) {
        stop(
            "Only models with untransformed main-effect predictors ",
            "are currently supported."
        )
    }

    if (is.null(vars)) {
        vars <- predictors
    }

    ## Numeric indices refer to positions among the model predictors
    if (is.numeric(vars)) {
        if (
            anyNA(vars) ||
            any(vars != as.integer(vars)) ||
            any(vars < 1L) ||
            any(vars > length(predictors))
        ) {
            stop("'vars' contains invalid predictor indices.")
        }

        vars <- predictors[vars]
    }

    if (!is.character(vars) || length(vars) < 2L) {
        stop(
            "'vars' must contain at least two predictor names ",
            "or predictor indices."
        )
    }

    if (anyDuplicated(vars)) {
        stop("'vars' must not contain duplicate predictors.")
    }

    if (!all(vars %in% predictors)) {
        stop("All variables in 'vars' must be predictors in the model.")
    }

    if (type == "refit" && !is.null(at)) {
        stop("'at' can only be used when type = 'slice'.")
    }

    variable_pairs <- combn(vars, 2L, simplify = FALSE)

    plots <- lapply(
        variable_pairs,
        function(pair) {
            .plot_class_region_pair(
                object = object,
                vars = pair,
                data = data,
                type = type,
                at = at,
                n = n,
                alpha = alpha,
                point_size = point_size,
                show_points = show_points
            )
        }
    )

    names(plots) <- vapply(
        variable_pairs,
        function(pair) {
            paste(pair, collapse = "_vs_")
        },
        character(1)
    )

    ## Return one ggplot object when only one pair is requested
    if (length(plots) == 1L) {
        return(plots[[1L]])
    }

    ## Return a named list by default
    ## if (!combine) {
    ##     return(plots)
    ## }

    ## if (!requireNamespace("patchwork", quietly = TRUE)) {
    ##     stop("Package 'patchwork' is required when combine = TRUE.")
    ## }

    if (is.null(ncol)) {
        ncol <- ceiling(sqrt(length(plots)))
    }


    if (plot) {
        print(
            patchwork::wrap_plots(
                           plots,
                           ncol = ncol
                       )
        )
    }
    
    invisible(plots)
}


.plot_class_region_pair <- function(
    object,
    vars,
    data = NULL,
    type = c("refit", "slice"),
    at = NULL,
    n = 250,
    alpha = 0.25,
    point_size = 2.4,
    show_points = TRUE
    ) {
    

  if (!inherits(object, c("lda", "qda"))) {
    stop("'object' must be a MASS::lda- or MASS::qda-object.")
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("'ggplot2' must be installed.")
  }

  ## Hent modeldata fra objektets oprindelige kald,
  ## medmindre de gives direkte
  if (is.null(data)) {
    data <- tryCatch(
      eval(object$call$data, envir = environment(formula(object))),
      error = function(e) NULL
    )

    if (is.null(data)) {
      stop(
        "Could not find the original dataset. ",
        "Specify data with data = ..."
      )
    }
  }

  form <- formula(object)

  response <- all.vars(form)[1]
  predictors <- attr(terms(object), "term.labels")

  ## Denne funktion er beregnet til almindelige numeriske hovedvirkninger
  if (!all(predictors %in% names(data))) {
    stop(
      "The function supports only simple variable names."
    )
  }

  ## Numeriske indeks fortolkes blandt modellens forklarende variable
  if (is.numeric(vars)) {
    if (length(vars) != 2L ||
        any(vars < 1L) ||
        any(vars > length(predictors))) {
      stop(
        "'vars' must be valid indices among explanatory variables."
      )
    }

    vars <- predictors[vars]
  }

  if (!is.character(vars) || length(vars) != 2L) {
    stop("'vars' must be variable names or indices.")
  }

  if (!all(vars %in% predictors)) {
    stop("Chosen variables not part of model.")
  }

  if (!all(vapply(data[vars], is.numeric, logical(1)))) {
    stop("Variables must be numeric.")
  }

  model_data <- data[, c(response, predictors), drop = FALSE]
  model_data <- model_data[complete.cases(model_data), , drop = FALSE]

  model_data[[response]] <- factor(model_data[[response]])

  xvar <- vars[1]
  yvar <- vars[2]
  other_vars <- setdiff(predictors, vars)

  xr <- range(model_data[[xvar]])
  yr <- range(model_data[[yvar]])

  xpad <- 0.04 * diff(xr)
  ypad <- 0.04 * diff(yr)

  if (xpad == 0) xpad <- 1
  if (ypad == 0) ypad <- 1

  grid <- expand.grid(
    x = seq(xr[1] - xpad, xr[2] + xpad, length.out = n),
    y = seq(yr[1] - ypad, yr[2] + ypad, length.out = n)
  )

  names(grid) <- c(xvar, yvar)

  typical_value <- function(x) {
    if (is.numeric(x)) {
      median(x, na.rm = TRUE)
    } else {
      names(which.max(table(x)))
    }
  }

  fixed_values <- lapply(model_data[other_vars], typical_value)

  if (!is.null(at)) {
    if (!is.list(at) || is.null(names(at))) {
      stop("'at' must be a named list.")
    }

    unknown <- setdiff(names(at), other_vars)

    if (length(unknown)) {
      stop(
        "Variable i 'at', som ikke skal holdes faste: ",
        paste(unknown, collapse = ", ")
      )
    }

    fixed_values[names(at)] <- at
  }

  for (v in other_vars) {
    value <- fixed_values[[v]]

    if (is.factor(model_data[[v]])) {
      grid[[v]] <- factor(
        rep(value, nrow(grid)),
        levels = levels(model_data[[v]])
      )
    } else {
      grid[[v]] <- rep(value, nrow(grid))
    }
  }

  grid$.predicted <- predict(object, newdata = grid)$class
  model_data$.observed <- model_data[[response]]

  classes <- levels(model_data$.observed)

  shapes <- rep(
    c(16, 17, 15, 3, 7, 8, 0, 1, 2, 4),
    length.out = length(classes)
  )
  names(shapes) <- classes

  fixed_text <- if (length(fixed_values)) {
    paste(
      paste0(
        names(fixed_values),
        " = ",
        vapply(fixed_values, as.character, character(1))
      ),
      collapse = ", "
    )
  } else {
    NULL
  }

  ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = grid,
      ggplot2::aes(
        x = .data[[xvar]],
        y = .data[[yvar]],
        fill = .data$.predicted
      ),
      alpha = alpha
    ) +
    ggplot2::geom_point(
      data = model_data,
      ggplot2::aes(
        x = .data[[xvar]],
        y = .data[[yvar]],
        colour = .data$.observed,
        shape = .data$.observed
      ),
      size = 2.4
    ) +
    ggplot2::scale_shape_manual(values = shapes) +
    ggplot2::labs(
      title = paste(
        toupper(class(object)[1]),
        "decision regions"
      ),
      subtitle = if (length(fixed_values)) {
        paste("Other vars fixed at:", fixed_text)
      },
      x = xvar,
      y = yvar,
      fill = response,
      colour = response,
      shape = response
    ) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::theme_minimal()
}





## plot_class_region <- function(object,
##                               vars,
##                               data = NULL,
##                               type = c("refit", "slice"),
##                               at = NULL,
##                               n = 250,
##                               alpha = 0.25,
##                               point_size = 2.4,
##                               show_points = TRUE,
##                               plot = TRUE) {

##     type <- match.arg(type)

##     if (!inherits(object, c("lda", "qda"))) {
##         stop("'object' must be an object returned by MASS::lda() or MASS::qda().")
##     }

##     if (!requireNamespace("MASS", quietly = TRUE)) {
##         stop("Package 'MASS' is required.")
##     }

##     if (!requireNamespace("ggplot2", quietly = TRUE)) {
##         stop("Package 'ggplot2' is required.")
##     }

##     ## Extract formula and variable names
##     form <- formula(object)
##     response <- all.vars(form)[1]
##     predictors <- attr(terms(form), "term.labels")

##     ## Currently, only models with simple main effects are supported
##     if (any(grepl("[:*^()]", predictors))) {
##         stop(
##             "Only models with untransformed main-effect predictors ",
##             "are currently supported."
##         )
##     }

##     ## Recover the original data if data is not supplied explicitly
##     if (is.null(data)) {
##         data_expr <- object$call$data

##         if (is.null(data_expr)) {
##             stop(
##                 "The original data cannot be identified from the model call. ",
##                 "Supply it using 'data'."
##             )
##         }

##         data <- tryCatch(
##             eval(data_expr, envir = environment(form)),
##             error = function(e) NULL
##         )

##         if (is.null(data)) {
##             stop(
##                 "The original data could not be recovered. ",
##                 "Supply it explicitly using 'data'."
##             )
##         }
##     }

##     ## Numeric values in vars refer to positions among the model predictors
##     if (is.numeric(vars)) {
##         if (length(vars) != 2L ||
##             any(vars < 1L) ||
##             any(vars > length(predictors))) {
##             stop(
##                 "'vars' must contain two valid indices among the model predictors."
##             )
##         }

##         vars <- predictors[vars]
##     }

##     if (!is.character(vars) || length(vars) != 2L) {
##         stop(
##             "'vars' must contain exactly two predictor names or two predictor indices."
##         )
##     }

##     if (!all(vars %in% predictors)) {
##         stop("Both variables in 'vars' must be predictors in the model.")
##     }

##     if (!all(vars %in% names(data))) {
##         stop("Both variables in 'vars' must be present in 'data'.")
##     }

##     if (!all(vapply(data[vars], is.numeric, logical(1)))) {
##         stop("The two plotted variables must be numeric.")
##     }

##     if (type == "refit" && !is.null(at)) {
##         stop("'at' can only be used when type = 'slice'.")
##     }

##     required_variables <- unique(c(response, predictors))

##     if (!all(required_variables %in% names(data))) {
##         stop("Not all model variables are present in 'data'.")
##     }

##     model_data <- data[, required_variables, drop = FALSE]
##     model_data <- model_data[complete.cases(model_data), , drop = FALSE]
##     model_data[[response]] <- factor(model_data[[response]])

##     xvar <- vars[1]
##     yvar <- vars[2]

##     if (type == "refit") {

##         ## Refit the model using only the two plotted predictors
##         pair_formula <- reformulate(vars, response = response)

##         fit <- if (inherits(object, "lda")) {
##             MASS::lda(pair_formula, data = model_data)
##         } else {
##             MASS::qda(pair_formula, data = model_data)
##         }

##         fixed_values <- list()

##     } else {

##         ## Retain the original fitted model and construct a two-dimensional slice
##         fit <- object
##         other_variables <- setdiff(predictors, vars)

##         typical_value <- function(x) {
##             if (is.numeric(x)) {
##                 mean(x, na.rm = TRUE)
##             } else if (is.factor(x) || is.character(x)) {
##                 names(which.max(table(x)))
##             } else {
##                 stop(
##                     "Unsupported variable type among the predictors ",
##                     "that must be held fixed."
##                 )
##             }
##         }

##         fixed_values <- lapply(
##             model_data[other_variables],
##             typical_value
##         )

##         if (!is.null(at)) {
##             if (!is.list(at) || is.null(names(at))) {
##                 stop("'at' must be a named list.")
##             }

##             unknown <- setdiff(names(at), other_variables)

##             if (length(unknown) > 0L) {
##                 stop(
##                     "The following variables in 'at' are not fixed predictors: ",
##                     paste(unknown, collapse = ", ")
##                 )
##             }

##             fixed_values[names(at)] <- at
##         }
##     }

##     ## Construct the prediction grid
##     xrange <- range(model_data[[xvar]], na.rm = TRUE)
##     yrange <- range(model_data[[yvar]], na.rm = TRUE)

##     xpadding <- 0.04 * diff(xrange)
##     ypadding <- 0.04 * diff(yrange)

##     if (xpadding == 0) {
##         xpadding <- 1
##     }

##     if (ypadding == 0) {
##         ypadding <- 1
##     }

##     grid <- expand.grid(
##         seq(
##             xrange[1] - xpadding,
##             xrange[2] + xpadding,
##             length.out = n
##         ),
##         seq(
##             yrange[1] - ypadding,
##             yrange[2] + ypadding,
##             length.out = n
##         )
##     )

##     names(grid) <- c(xvar, yvar)

##     ## Add fixed predictor values when plotting a slice
##     if (type == "slice") {
##         other_variables <- setdiff(predictors, vars)

##         for (variable in other_variables) {
##             value <- fixed_values[[variable]]

##             if (is.factor(model_data[[variable]])) {
##                 grid[[variable]] <- factor(
##                     rep(value, nrow(grid)),
##                     levels = levels(model_data[[variable]])
##                 )
##             } else if (is.character(model_data[[variable]])) {
##                 grid[[variable]] <- rep(
##                     as.character(value),
##                     nrow(grid)
##                 )
##             } else {
##                 grid[[variable]] <- rep(
##                     as.numeric(value),
##                     nrow(grid)
##                 )
##             }
##         }
##     }

##     grid$.predicted_class <- predict(
##         fit,
##         newdata = grid
##     )$class

##     model_data$.observed_class <- model_data[[response]]

##     class_levels <- levels(model_data$.observed_class)

##     shape_values <- rep(
##         c(16, 17, 15, 3, 7, 8, 0, 1, 2, 4),
##         length.out = length(class_levels)
##     )

##     names(shape_values) <- class_levels

##     subtitle <- if (type == "refit") {

##         paste(
##             "Model refitted using",
##             paste(vars, collapse = " and ")
##         )

##     } else {

##         fixed_text <- paste(
##             paste0(
##                 names(fixed_values),
##                 " = ",
##                 vapply(
##     fixed_values,
##     function(x) {
##         if (is.numeric(x)) {
##             formatC(x, format = "f", digits = 2)
##         } else {
##             as.character(x)
##         }
##     },
##     character(1)
## )
##                 ## vapply(
##                     ## fixed_values,
##                     ## as.character,
##                     ## character(1)
##                 ## )
##             ),
##             collapse = ", "
##         )

##         paste("Slice of the full model:", fixed_text)
##     }

##     plot_object <- ggplot2::ggplot() +
##         ggplot2::geom_raster(
##             data = grid,
##             ggplot2::aes(
##                 x = .data[[xvar]],
##                 y = .data[[yvar]],
##                 fill = .data$.predicted_class
##             ),
##             alpha = alpha
##         )

##     if (show_points) {
##         plot_object <- plot_object +
##             ggplot2::geom_point(
##                 data = model_data,
##                 ggplot2::aes(
##                     x = .data[[xvar]],
##                     y = .data[[yvar]],
##                     colour = .data$.observed_class,
##                     shape = .data$.observed_class
##                 ),
##                 size = point_size
##             ) +
##             ggplot2::scale_shape_manual(values = shape_values)
##     }

##     plot_object +
##         ggplot2::labs(
##             title = paste(
##                 toupper(class(object)[1]),
##                 "classification regions"
##             ),
##             subtitle = subtitle,
##             x = xvar,
##             y = yvar,
##             fill = response,
##             colour = response,
##             shape = response
##         ) +
##         ggplot2::coord_cartesian(expand = FALSE) +
##         ggplot2::theme_minimal()
## }
