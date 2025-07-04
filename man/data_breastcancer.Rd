% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DATA_doby.R
\docType{data}
\name{data_breastcancer}
\alias{data_breastcancer}
\alias{breastcancer}
\title{Gene expression signatures for p53 mutation status in 250 breast cancer
samples}
\format{
A data frame with 250 observations on 1001 variables. The
first 1000 columns are numerical variables; the last column
(named \code{code}) is a factor with levels \code{case} and
\code{control}.
}
\source{
Chris Holmes, \email{c.holmes@stats.ox.ac.uk}
}
\usage{
breastcancer
}
\description{
Perturbations of the p53 pathway are associated with more aggressive and
therapeutically refractory tumours. We preprocessed the data using Robust
Multichip Analysis (RMA). Dataset has been truncated to the 1000 most
informative genes (as selected by Wilcoxon test statistics) to simplify
computation. The genes have been standardized to have zero mean and unit
variance (i.e. z-scored).
}
\details{
The factor \code{code} defines whether there was a mutation in the p53
sequence (code=case) or not (code=control).
}
\examples{

data(breastcancer)
bc <- breastcancer
pairs(bc[,1:5], col=bc$code)

train <- sample(1:nrow(bc), 50)
table(bc$code[train])
\dontrun{
library(MASS)
z <- lda(code ~ ., data=bc, prior = c(1, 1) / 2, subset = train)
pc <- predict(z, bc[-train, ])$class
pc
bc[-train, "code"]
table(pc, bc[-train, "code"])
}

}
\references{
Miller et al (2005, PubMed
ID:16141321)
}
\concept{dataset}
\keyword{datasets}
