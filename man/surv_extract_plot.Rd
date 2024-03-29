% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surv_plot.r
\name{surv_extract_plot}
\alias{surv_extract_plot}
\title{Plot the output of a `surv_extract`}
\usage{
surv_extract_plot(
  data,
  color = "id",
  legendgroup = "id",
  type = c("wx", "surv")
)
}
\arguments{
\item{data}{the result of a `surv_extract` call.}

\item{color}{name of the column to be used as color.}

\item{legendgroup}{name of the column to agregate the legend. Used only when ploting more than
one curve with `plotly::subplot`.}

\item{type}{should the wx or the surv column be plotted?}
}
\value{
a plotly of the requested column versus time.
}
\description{
This function can plot any of the columns of the output of `surv_extract`
}
