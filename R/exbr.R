#' Extract elements using brackets
#'
#' These two helper functions extract elements from lists, data frames, or vectors.
#' They are simple wrappers for the standard bracket operators in R:
#'
#' - `exbr1()` uses single brackets (`[`) and returns a **subset**.
#' - `exbr2()` uses double brackets (`[[`) and returns the **element itself**.
#'
#' These are safer and more flexible than `$`, especially when used with the base R pipe (`|>`)
#' or in functional programming.
#'
#' @param x A list, data frame, or vector.
#' @param which The index or name of the element(s) to extract.
#'
#' @return
#' - `exbr1()` returns a subset of `x`.
#' - `exbr2()` returns a single element from `x`.
#'
#' @examples
#' lst <- list(a = 1:3, b = 4:6)
#'
#' # Without pipe
#' exbr1(lst, "a")      # List with one element
#' exbr2(lst, "a")      # Just the vector 1:3
#'
#' # With base R pipe
#' lst |> exbr1("a")
#' lst |> exbr2("a")
#'
#' df <- data.frame(x = 1:5, y = letters[1:5])
#'
#' df |> exbr1("y")     # Returns a data frame with column 'y'
#' df |> exbr2("y")     # Returns column 'y' as a character vector
#'
#' @name exbrket_access
#' @aliases exbr1 exbr2
#' @export
exbr1 <- function(x, which) {
  x[which]
}

#' @rdname exbrket_access
#' @export
exbr2 <- function(x, which) {
  x[[which]]
}
