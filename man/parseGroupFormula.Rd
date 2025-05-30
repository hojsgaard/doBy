% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doby_utilities.R
\name{parseGroupFormula}
\alias{parseGroupFormula}
\title{Extract components from a formula with "conditioning bar"}
\usage{
parseGroupFormula(form)
}
\arguments{
\item{form}{A formula of the form \code{y ~ x1 + ... + xn | g1 + ... + gm}}
}
\value{
If the formula is \code{y ~ x1 + x2 | g1 + g2} the result is
\item{model}{\code{y ~ x1 + x2}} \item{groups}{\code{ g1 + g2}}
\item{groupFormula}{\code{~ g1 + g2}}
}
\description{
Extract components from a formula with the form
\code{y ~ x1 + ... + xn | g1 + ... + gm}
}
\examples{

gf1 <- parseGroupFormula(y ~ x1 + x2 | g1 + g2)
gf1

gf2 <- parseGroupFormula( ~ x1 + x2 | g1 + g2)
gf2

}
\author{
Søren Højsgaard, \email{sorenh@math.aau.dk}
}
\keyword{utilities}
