#' @title Plot linear model object
#'
#' @param lm_fit An object of class 'lm'
#' @param format The format of the plot (or a list of plots if format is "list")
#' @param global_aes Currently no effect.
#'
#' @examples
#' data(income)
#' m1 <- lm(inc ~ race + educ, data=income)
#' plot_lm(m1)
#' plot_lm(m1, "2x2")
#' plot_lm(m1, "1x4")
#' plot_lm(m1, "4x1")
#' plot_lm(m1, "list")
#' 
#' @export
plot_lm <- function(lm_fit, format="2x2", global_aes=NULL) {
    
    if(!inherits(lm_fit, "lm"))
        stop("'lm_fit' must inherit form 'lm'\n")

    format <- match.arg(format, c("2x2", "1x4", "4x1", "list"))
    
    dd <- data.frame(
        fitted_values          = predict(lm_fit),
        resp        = predict(lm_fit)+resid(lm_fit),
        std_res = rstandard(lm_fit))


    pl1 <- ggplot(dd,
                  aes(x = .data$fitted_values,
                      y = .data$std_res)) +
        geom_point() + geom_hline(yintercept=c(0, -1.96, 1.95))
    
    pl2 <- ggplot(dd,
                  aes(x=.data$fitted_values,
                      y=.data$resp)) +
        geom_point() +  geom_smooth(method="lm", formula=y ~ x, se=FALSE)
    
    pl3 <- ggplot(dd,
                  aes(x=.data$fitted_values,
                      y=sqrt(abs(.data$std_res)))) +
        geom_point()

    pl4 <- ggplot(dd,
                  aes(sample = .data$std_res)) +
        stat_qq() + stat_qq_line() +
        labs(y="std_res", x = "theoretical quantiles")

    switch(format,
           "2x2"={ cowplot::plot_grid(pl1, pl2, pl3, pl4, nrow=2) },
           "1x4"={ cowplot::plot_grid(pl1, pl2, pl3, pl4, nrow=1) },
           "4x1"={ cowplot::plot_grid(pl1, pl2, pl3, pl4, nrow=4) },
           "list"={ list(pl1=pl1, pl2=pl2, pl3=pl3, pl4=pl4) }
           )

}


#' @title Two-way interaction plot 
#'
#' @description Plots the mean of the response for
#'     two-way combinations of factors, thereby illustrating possible
#'     interactions.
#'
#' @name interaction-plot
#'
#' @note This is a recent addition to the package and is subject to change.
#'
#' @param .data A data frame
#' @param .formula A formula of the form `y ~ x1 + x2`
#' @param interval Either `conf.int`, `boxplot` or `none`
#'
#' @examples
#'
#' income$educf <- cut(income$educ, breaks=3)
#' income |> interaction_plot(inc ~ race + educf)
#' income |> interaction_plot(inc ~ race + educf, interval="conf.int")
#' income |> interaction_plot(inc ~ race + educf, interval="boxplot")
#' income |> interaction_plot(inc ~ race + educf, interval="none")
#' 
#' @import dplyr
#' @import ggplot2

#' @export
interaction_plot <- function(.data, .formula, interval="conf.int"){

    interval = match.arg(tolower(interval), c("conf.int", "boxplot", "none"))

    if (!inherits(.formula, "formula")) stop("'.formula' is not formula")
    if (length(.formula) != 3)          stop("'.formula' is not two sided")
    
    lhs <- all.vars(.formula[[2]])
    rhs <- all.vars(.formula[[3]])
    if (length(rhs) < 2) stop("rhs must have at least two elements")

    rr <- sym(lhs)
    s1 <- sym(rhs[1])
    s2 <- sym(rhs[2])
    ## s2 <- sym(rhs[-1])

    # lapply(rhs, sym)
    
    ## str(list(rr, s1, s2))
    ## If lhs is transformed
    resp <- eval(.formula[[2]], .data)
    .data$RESP <- resp  ## KLUDGY
    rr2 <- sym("RESP")  ## KLUDGY

    dd1 <- .data |> group_by(!!sym(s1), !!sym(s2)) 
    tmp <- dd1 |> summarise(val = mean({{ rr2 }}),
                             sd  = sd({{ rr2 }}) / sqrt(n()),
                             lwr = .data$val - 1.96 * sd, upr=.data$val + 1.96 * sd,
                             .groups="keep")
    ##print(.data); print(dd1); print(tmp)

    switch(interval,
           "boxplot" = {
               ## BOXPLOT
               pp <- ggplot(.data, aes(x = factor(!!sym(s1)), y = !!sym(rr2),
                                       colour = !!sym(s2))) 
               
               pp + labs(y = deparse(.formula[[2]]), x=rhs[1]) +
                   geom_boxplot() + ## FIXME want conf. interval bars
                   geom_point(data = tmp, aes(y = .data$val)) +
                   geom_line(data = tmp, aes(y = .data$val, group = !!sym(s2))) + 
                   theme_bw()
           },
           "conf.int" = {
               ## mean +/- 2 sd
               pp <- ggplot(tmp, aes(x = factor(!!sym(s1)), y = .data$val,
                                     colour = !!sym(s2)))
               
               pp + labs(y = deparse(.formula[[2]]), x=rhs[1]) +
                   geom_point(data = tmp, aes(y = .data$val)) +
                   geom_line(data = tmp, aes(y = .data$val, group = !!sym(s2))) + 
                   geom_errorbar(aes(ymin=.data$lwr, ymax=.data$upr), data=tmp,
                                 width=.4, position=position_dodge(0.1)) +
                   geom_point(aes(x = factor(!!sym(s1)), y = !!sym(rr2),
                                       colour = !!sym(s2)), data=.data)
               

               
           },
           "none" = {
               pp <- ggplot(tmp, aes(x = factor(!!sym(s1)), y = .data$val,
                                     colour = !!sym(s2)))               
               pp + labs(y = deparse(.formula[[2]]), x=rhs[1]) + 
                   geom_point(data = tmp, aes(y = .data$val)) +
                   geom_line(data = tmp, aes(y = .data$val, group = !!sym(s2))) 
           })
}



#' Plot the response variable against the predictor variables.
#'
#' @param formula. A formula of the form y ~ x1 + x2 + ... + xn, where
#'     y is the response variable and x1, x2, ..., xn are the
#'     predictor variables. A dot as right hand side is allowed.
#' 
#' @param data. A data frame containing the variables in the formula.
#' @param geoms A list of ggplot2 geoms to be added to the plot.
#' @param global_aes A list of global aesthetics to be added to the plot.
#' @param plot A logical value indicating whether the plot should be displayed.
#' @param nrow,ncol Number of rows / columns in plot. 
#'
#' @return A list of ggplot2 plots.
#' @export
#'
#' @examples
#' library(ggplot2)
#' response_plot(iris, Sepal.Width ~ ., geoms=geom_point())
#' response_plot(iris, Sepal.Width ~ ., geoms=geom_point(), global_aes=list(color="Species"))
#' personality |> response_plot(easygon~., geoms=geom_point(), global_aes=NULL)
#' 
response_plot <- function(data., formula., geoms=NULL, global_aes=NULL, plot=TRUE, nrow=NULL, ncol=NULL) {  
    trms <- terms(formula., data=data.)
    ## trms
    yy <- as.character(formula.[[2]])
    xx <- setdiff(attr(trms, "term.labels"), yy)
    ##    list(x=xx, y=yy) |> str()

    aes_template <- lapply(xx, function(x_){
        c(list(x=x_, y=yy), global_aes) 
    })
    ## aes_template

    aes_list <- aes_template |> lapply(ggpubr_create_aes)
    ## aes_list
    
    plot_basic <- lapply(aes_list, function(z_){
        data.  |> ggplot(mapping = z_)    
    })
    ## plot_basic
    
    plot_list <-lapply(plot_basic, function(pl_) {
        pl_ + geoms
    } )
    if (plot){
        s <- cowplot::plot_grid(plotlist = plot_list, nrow=nrow, ncol=ncol)        
        print(s)
    }
    
    return(invisible(plot_list))
}


### Taken from ggpubr::create_aes to avoid circular dependencies

ggpubr_create_aes <- function (.list, parse = TRUE) 
{
  # if (missing(parse)) {
  #   parse <- base::getOption("ggpubr.parse_aes", default = TRUE)
  # }
#  if (parse) {
    return(create_aes.parse(.list))
#  }
#  else {
#    return(create_aes.name(.list))
#  }
}

create_aes.parse <- function (.list) 
{
#  .list <- .list %>% purrr::map(function(x) parse_expression(x))
  .list <- .list |> lapply(function(x) parse_expression(x))
  do.call(ggplot2::aes, .list)
}

parse_expression <- function (x) 
{
  if (is_parsable_aes(x)) {
    if (contains_space(x)) {
      if (!is_math_string(x)) 
        return(as.name(x))
    }
    x <- parse(text = x)[[1]]
  }
  x
}

is_numeric_char <- function (x) 
{
  if (is.character(x)) 
    res <- grepl("^[[:digit:]]+$", x)
  else res <- FALSE
  res
}

is_parsable_aes <- function (x) 
{
  is.character(x) & (!is_numeric_char(x)) & (length(x) == 1)
}

contains_space <- function (x) 
{
  grepl("\\s", x)
}

is_math_string <- function (x) 
{
  operators <- c("+", "-", "*", "^", "%%", "%/%", "/", "==", 
                 ">", "<", "!=", "<=", ">=")
  contains_math_operators <- unlist(lapply(operators, grepl, 
                                           x, fixed = TRUE))
  contains_parentheses <- grepl(pattern = "\\(.*\\)", x)
  any(c(contains_math_operators, contains_parentheses))
}




# parse_as_expression <- function (text) 
# {
#   stopifnot(is.character(text))
#   out <- vector("expression", length(text))
#   for (i in seq_along(text)) {
#     expr <- parse(text = text[[i]])
#     out[[i]] <- if (length(expr) == 0) 
#       NA
#     else expr[[1]]
#   }
#   out
# }






