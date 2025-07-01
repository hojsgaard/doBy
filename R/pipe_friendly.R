#' @title Pipe-friendly arithmetic helpers
#' @description
#' A set of simple, vectorized, pipe-friendly arithmetic functions for
#' transforming numeric data in pipelines. These helpers make common
#' operations like multiplication, division, addition, subtraction,
#' exponentiation, and reciprocals clearer when using the native pipe `|>`.
#'
#' @details
#' All functions are vectorized and support numeric vectors, scalars, or compatible objects.
#' They are designed to improve the readability of transformation pipelines.
#'
#' @param x A numeric vector or scalar.
#' @param k A numeric scalar for addition, subtraction, multiplication, or division.
#' @param p A numeric scalar exponent (for \code{pow}).
#'
#' @return A numeric vector or scalar resulting from the transformation.
#'
#' @examples
#' x <- c(1, 2, 3)
#'
#' # Multiplication and division
#' x |> mult(10)
#' x |> divide(2)
#'
#' # Addition and subtraction
#' x |> add(5)
#' x |> subtract(1)
#'
#' # Reciprocal
#' x |> reciprocal()
#'
#' # Power
#' x |> pow(2)
#'
#' # Combined use in pipelines
#' x |>
#'   mult(2) |>
#'   add(3) |>
#'   reciprocal()
#'
#' @name pipe_arithmetic
#' @concept pipe_arithmetic
NULL

#' @rdname pipe_arithmetic
#' @concept pipe_arithmetic
#' @export
reciprocal <- function(x) {
  1 / x
}

#' @concept pipe_arithmetic
#' @rdname pipe_arithmetic
#' @export
pow <- function(x, p) {
  x ^ p
}

#' @concept pipe_arithmetic
#' @rdname pipe_arithmetic
#' @export
add <- function(x, k) {
  x + k
}

#' @concept pipe_arithmetic
#' @rdname pipe_arithmetic
#' @export
subtract <- function(x, k) {
  x - k
}

#' @concept pipe_arithmetic
#' @rdname pipe_arithmetic
#' @export
mult <- function(x, k) {
  x * k
}

#' @concept pipe_arithmetic
#' @rdname pipe_arithmetic
#' @export
divide <- function(x, k) {
  x / k
}
