% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DATA_doby.R
\docType{data}
\name{dietox}
\alias{dietox}
\title{Growth curves of pigs in a 3x3 factorial experiment}
\format{
This data frame contains the following columns:

\describe{
\item{Weight}{Weight in Kg}
\item{Feed}{Cumulated feed intake in Kg}
\item{Time}{Time (in weeks) in the experiment}
\item{Pig}{Factor; id of each pig}
\item{Evit}{Factor; vitamin E dose; see 'details'.}
\item{Cu}{Factor, copper dose; see 'details'}
\item{Start}{Start weight in experiment, i.e. weight at week 1.}
\item{Litter}{Factor, id of litter of each pig}
}
}
\source{
Lauridsen, C., Højsgaard, S.,Sørensen, M.T. C. (1999) Influence of
Dietary Rapeseed Oli, Vitamin E, and Copper on Performance and
Antioxidant and Oxidative Status of Pigs. J. Anim. Sci.77:906-916
}
\usage{
dietox
}
\description{
The \code{dietox} data frame has 861 rows and 7 columns.
}
\details{
Data contains weight of slaughter pigs measured weekly for 12
weeks. Data also contains the start weight (i.e. the weight at week
1). The treatments are 3 different levels of Evit = vitamin E (dose: 0,
100, 200 mg dl-alpha-tocopheryl acetat /kg feed) in combination with 3
different levels of Cu=copper (dose: 0, 35, 175 mg/kg feed) in the feed.
The cumulated feed intake is also recorded. The pigs are litter mates.
}
\examples{

data(dietox)
head(dietox)
coplot(Weight ~ Time | Evit * Cu, data=dietox)

}
\concept{dataset}
\keyword{datasets}
