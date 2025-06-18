#' Variable utilities: parse, select, check, map, rename
#' 
#' These functions provide flexible tools for parsing and managing variable names
#' from formulas, unquoted names, or character vectors. Demonstrated using `CO2` dataset.
#' @param ... Variable input (unquoted, character vector, or formula)
#' @importFrom rlang enquos eval_tidy as_name abort is_formula f_rhs quo_get_expr
#' @export
vparse <- function(...) {
  dots <- enquos(...)

  if (length(dots) > 1) {
    # Case: multiple unquoted inputs
    return(unname(sapply(dots, as_name)))
  }

  # Single input
  q <- dots[[1]]
  expr <- quo_get_expr(q)

  # Case 1: formula (~Treatment + Type)
  if (is_formula(expr)) {
    return(all.vars(f_rhs(expr)))
  }

  # Case 2: single unquoted variable (symbol)
  if (is.symbol(expr)) {
    return(as_name(expr))
  }

  # Case 3: character vector
  val <- eval_tidy(q)
  if (is.character(val)) {
    return(val)
  }

  abort("Invalid input to vparse: expected unquoted names, a character vector, or a formula.")
}


#' Shorthand for vparse()
#'
#' A short and convenient alias for `vparse()`. Accepts unquoted names, character vectors, or a formula.
#'
#' @param ... Variable input in any accepted `vparse()` form
#'
#' @return A character vector of variable names
#' @export
v <- function(...) {
  vparse(...)
}



#' Select columns from a data frame using flexible input
#'
#' @param df A data frame
#' @param ... Variable input (unquoted, character vector, or formula)
#' @return A data frame with selected columns
#' @export
vselect <- function(df, ...) {
  vars <- vparse(...)
  df[, vars, drop = FALSE]
}

#' Check if variables exist in a data frame
#'
#' @param df A data frame
#' @param ... Variables to check
#' @return TRUE if all variables exist, otherwise error
#' @export
vcheck <- function(df, ...) {
  vars <- vparse(...)
  missing <- vars[!vars %in% names(df)]
  if (length(missing) > 0) {
    abort(paste("Missing variables in data frame:", paste(missing, collapse = ", ")))
  }
  TRUE
}

#' Apply a function to each parsed variable name
#'
#' @param .vars Variables to parse
#' @param .f Function to apply to each name
#' @return A list of results

#' @export
vmap <- function(.vars, .f) {
  q <- enquo(.vars)
  vars <- vparse(!!q)
  lapply(vars, .f)
}


#' Rename columns in a data frame
#'
#' @param df A data frame
#' @param rename_map A named character vector (old_name = new_name)
#' @return A data frame with renamed columns
#' @export
vrename <- function(df, rename_map) {
  stopifnot(is.character(rename_map), !is.null(names(rename_map)))
  old_names <- names(rename_map)
  new_names <- unname(rename_map)

  matched <- match(old_names, names(df))
  if (any(is.na(matched))) {
    abort("Some names in rename_map do not exist in the data frame.")
  }

  names(df)[matched] <- new_names
  df
}

# Example usage with CO2 dataset
#' @examples
#' data(CO2)
#' vparse(Treatment, Type)
#' vparse(c("Treatment", "Type"))
#' vparse(~Treatment + Type)
#' vselect(CO2, Treatment, Type)
#' vcheck(CO2, ~Treatment + Type)
#' vmap(~Treatment + Type, toupper)
#' vrename(CO2, c(Treatment = "Trt", Type = "Group"))


