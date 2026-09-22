#' Plot Grouped Data with Elliptical Contours
#'
#' Creates a scatter plot of two numeric variables and adds elliptical contours
#' separately for each group. Several inner contours are drawn as black lines,
#' while an outer contour is displayed as a semi-transparent polygon.
#'
#' @param data A data frame containing the variables to be plotted.
#' @param x,y <[`data-masking`][rlang::args_data_masking]> Unquoted names of the
#'   numeric variables to be plotted on the horizontal and vertical axes.
#' @param groups Optional <[`data-masking`][rlang::args_data_masking]> unquoted
#'   name of a grouping variable. If supplied, points are colored by group and
#'   ellipses are computed separately for each group. If omitted, the ellipses
#'   are computed from all observations.
#' @param colors Optional vector of colors passed to
#'   [ggplot2::scale_color_manual()]. The vector may be named to associate
#'   colors explicitly with group levels. If `NULL`, the default ggplot2 color
#'   scale is used.
#' @param levels Numeric vector containing the confidence levels of the inner
#'   ellipse contours. Values must lie between zero and one. The default is
#'   `c(0.2, 0.5, 0.7, 0.9)`.
#' @param outer_level Numeric value giving the confidence level of the outer,
#'   filled ellipse. The default is `0.95`.
#' @param fill Color used to fill the outer ellipse. The default is `"grey60"`.
#' @param alpha Numeric value between zero and one controlling the transparency
#'   of the filled outer ellipse. The default is `0.15`.
#'
#' @details
#' Ellipses are calculated by [ggplot2::stat_ellipse()] using its default
#' distributional assumptions. The ellipses describe the estimated bivariate
#' distribution within each group; they are not confidence regions for the
#' group means.
#'
#' The ellipse levels supplied through `levels` are drawn in black and are not
#' included in the legend. The outer ellipse uses the same grouping structure
#' as the points, but has a common fill color and is also excluded from the
#' legend.
#'
#' A group must contain enough non-collinear observations for its covariance
#' matrix and ellipse to be estimated. Groups with too few observations or a
#' singular covariance matrix may produce warnings and no ellipse.
#'
#' @return A [`ggplot2::ggplot`] object. Additional ggplot2 layers and theme
#'   modifications can be added to the returned object.
#'
#' @examples
#' ## Without groups
#' ellipse_plot(iris, Sepal.Length, Sepal.Width)
#' 
#'
#' ## With groups
#' ellipse_plot(
#'   iris,
#'   Sepal.Length,
#'   Sepal.Width,
#'   groups = Species
#' )
#'
#' @seealso [ggplot2::stat_ellipse()], [ggplot2::geom_point()]
#'
#' @importFrom ggplot2 aes geom_point ggplot scale_color_manual stat_ellipse
#'   theme_minimal
#' @export
ellipse_plot <- function(data, x, y, groups = NULL,
                          colors = NULL,
                          levels = c(.2, .5, .7, .9),
                          outer_level = .95,
                          fill = "grey60",
                          alpha = .15) {
  has_groups <- !missing(groups) &&
    !identical(substitute(groups), quote(NULL))

  if (has_groups) {
    p <- ggplot(
      data,
      aes(
        x = {{ x }},
        y = {{ y }},
        color = {{ groups }},
        group = {{ groups }}
      )
    )
  } else {
    p <- ggplot(
      data,
      aes(
        x = {{ x }},
        y = {{ y }}
      )
    )
  }

  p <- p +
    geom_point() +
    stat_ellipse(
      geom = "polygon",
      level = outer_level,
      fill = fill,
      alpha = alpha,
      show.legend = FALSE
    ) +
    lapply(
      levels,
      \(lev) stat_ellipse(
        level = lev,
        color = "black",
        show.legend = FALSE
      )
    ) +
    theme_minimal()

  if (has_groups && !is.null(colors)) {
    p <- p + scale_color_manual(values = colors)
  }

  p
}
