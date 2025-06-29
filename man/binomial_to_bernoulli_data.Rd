% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_binomial_to_bernoulli_data.r
\name{binomial_to_bernoulli_data}
\alias{binomial_to_bernoulli_data}
\title{Convert binomial data to bernoulli data}
\usage{
binomial_to_bernoulli_data(
  data.,
  y,
  size,
  type = c("rest", "total"),
  response_name = "response",
  rest_name = NULL
)
}
\arguments{
\item{data.}{A dataframe}

\item{y}{Column with 'successes' in binomial distribution \code{y~bin(size, p)}}

\item{size}{Column with 'failures', i.e. size-y or 'total', i.e. size.}

\item{type}{Whether \code{size} is rest (i.e. 'failures') or 'total'}

\item{response_name}{Name of response variable in output dataset.}

\item{rest_name}{Name of 'failures' in column \code{response_name}.}
}
\description{
Convert binomial data to bernoulli data by expanding dataset.
}
\examples{

dat <- budworm
dat <- dat[dat$dose \%in\% c(1,2), ]
dat$ntotal <- 5
dat
dat.a <- dat |>
  binomial_to_bernoulli_data(ndead, ntotal, type="total")
dat.b <- dat |>
  dplyr::mutate(nalive=ntotal-ndead) |> dplyr::select(-ntotal) |>
  binomial_to_bernoulli_data(ndead, nalive, type="rest")

m0 <- glm(cbind(ndead, ntotal-ndead) ~ dose + sex, data=dat, family=binomial())
m1 <- glm(ndead / ntotal ~ dose + sex, data=dat, weight=ntotal, family=binomial())
ma <- glm(response ~ dose + sex, data=dat.a, family=binomial())
mb <- glm(response ~ dose + sex, data=dat.b, family=binomial())

dat.a$response
dat.b$response ## Not same and therefore the following do not match

all.equal(coef(m0), coef(ma))
all.equal(coef(m0), coef(mb))
all.equal(coef(m1), coef(ma))
all.equal(coef(m1), coef(mb))
}
