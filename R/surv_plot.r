#' Plot survival curves using survival and plotly package.
#'
#' Plot a survival K-M estimate and shows both survival and n.event/n.risk.
#'
#' @param type should the "wx", "survival" or "both" be plotted?
#' @inheritParams surv_cluster
#'
#' @return a plotly with two plots, one for survival probabilities and
#' another for wx.
#'
#' @export
#'
#' @examples
#'
#' require(survival)
#' aj <- survfit(Surv(time, status) ~ sex, data = lung)
#' surv_plot(aj)
#'
surv_plot <- function(aj, type = c("both", "surv", "wx")) {
  type <- match.arg(type)

  km <- surv_extract(aj)

    if (type == "both") {
    ret <- plotly::subplot(
      plotly::style(surv_plot_aux(km, type = "surv"), showlegend = F),
      plotly::style(surv_plot_aux(km, type = "wx"), showlegend = T)
    )
  } else {
    ret <- surv_plot_aux(km, type = type)
  }
  return(ret)
}

#' Plot a comparison between the unclusterized and clusterized survival.
#'
#' @param x_cluster name of the clusterized covariate
#' @inheritParams surv_aj
#'
#' @return a plotly graph comparing the clusterization of `x` with regard to
#' wx.
#'
#' @details This function uses the following wrapper to fit a KM estimate
#' based on its arguments, for the clusterized and unclusterized column:
#'
#' `survfit(formula(paste0("Surv(time,event)~", x)), data = data, ...)`
#'
#' `survfit(formula(paste0("Surv(time,event)~", x_cluster)), data = data, ...)`
#'
#' @export
#'
surv_plot_cluster <- function(x, data, time = "time", event = "event", x_cluster = paste0(x, "Cluster"), ...) {
  by <- c(x)
  names(by) <- "id"

  km <- dplyr::left_join(
    surv_extract(surv_aj(x, data, time, event, ...)),
    dplyr::distinct(
      data,
      dplyr::across(
        dplyr::all_of(c(x, x_cluster))
      )
    ),
    by = by
  )

  km_cluster <- surv_extract(surv_aj(x_cluster, data, time, event, ...))

  fig1 <- surv_plot_aux(km, color = x_cluster, legendgroup = x_cluster, type = "wx")
  fig2 <- surv_plot_aux(km_cluster, type = "wx")

  plotly::subplot(fig1, plotly::style(fig2, showlegend = F), shareY = TRUE)
}


#'
#'
surv_plot_aux <- function(data, color = "id", legendgroup = "id", type = c("wx", "surv")) {
  type <- match.arg(type)
  shape <- if (type == "surv") "hv" else "linear"
  plotly::add_trace(
    plotly::plot_ly(type = "scatter", mode = "lines"),
    data = data,
    x = ~time,
    y = stats::formula(paste0("~", type)),
    color = stats::formula(paste0("~", color)),
    name = ~id,
    legendgroup = stats::formula(paste0("~", legendgroup)),
    line = list(shape = shape)
  )
}
