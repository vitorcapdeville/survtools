% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surv_extract.R
\name{surv_extract_km}
\alias{surv_extract_km}
\title{Extract survival probabilities and 'failure' probabilities}
\usage{
surv_extract_km(aj)
}
\arguments{
\item{aj}{aj previously fitted model (using `survival::survfit()`).}
}
\value{
a data.frame with the following columns: `time`, `id`, `surv` and `wx`.
`id` is the value of the covariates in the model. If more than one covariate
is used, they are merged together using `tidyr::unite`.
`surv` is the survival probability and `wx` is n.event/n.risk, closely related
with the probability of failing before x + 1, for individuals alive at x.
}
\description{
Uses a previously adjusted survfit model and returns the results in
a standard and convenient way. Works only for K-M estimates.
}
