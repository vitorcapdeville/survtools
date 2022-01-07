#' Ajuste e plot dos resultados do KM
#'
#' Ajusta o KM usando o pacote survival e plota os resultados.
#' Plota tanto a curva de sobrevivência, quanto o percentual
#' de cancelamentos em cada tempo.
#'
#' @param base base com as colunas abertura, TempoCasa e event.
#' @param abertura nome da coluna com a variável explicatória.
#' @param time nome da coluna com os tempos até falha/censura.
#' @param event nome da coluna com o status de falha/censura.
#'
#' @return Nada
#'
#' @examples
#' require(survival)
#' km_aj(lung, "sex", "time", "status")
#' @export
km_aj <- function(base, abertura, time = "TempoCasa", event = "event") {
  km <- surv_extract(
    survival::survfit(
      stats::as.formula(paste0("Surv(", time, ", ", event, ") ~ ", abertura)),
      data = base
    )
  )
  fig1 <- plotly::add_trace(
    plotly::plot_ly(type = "scatter", mode = "lines"),
    data = km,
    x = ~time,
    y = ~surv,
    color = ~id,
    name = ~id,
    legendgroup = ~id,
    line = list(shape = "hv")
  )
  fig2 <- plotly::add_trace(
    plotly::plot_ly(type = "scatter", mode = "lines"),
    data = km,
    x = ~time,
    y = ~wx,
    color = ~id,
    name = ~id,
    legendgroup = ~id
  )
  plotly::subplot(
    plotly::style(fig1, showlegend = F),
    plotly::style(fig2, showlegend = T)
  )
}

#' Função para ajustar e plotar o KM com uma variável explicatória,
#' comparando com a sua clusterização
#'
#' @param abertura nome da coluna com a variável explicatória
#' @param base base com as colunas abertura, TempoCasa e event.
#' @inheritParams km_aj
#'
#' @return invisible
#'
km_aj_cluster <- function(base, abertura, time = "TempoCasa", event = "event") {
  abertura_cluster <- paste0(abertura, "Cluster")
  by <- c(abertura)
  names(by) <- "id"
  km <- dplyr::left_join(
  surv_extract(
    survival::survfit(
      stats::as.formula(paste0("Surv(TempoCasa, event) ~ ", abertura)),
      data = base
    )
  ),
  dplyr::distinct(
    base,
    dplyr::across(
      dplyr::all_of(
        c(abertura, abertura_cluster)
      )
    )
  ),
  by = by
)

  km_cluster <- surv_extract(
    survival::survfit(
      stats::as.formula(paste0("Surv(TempoCasa, event) ~ ", abertura_cluster)),
      data = base
    )
  )

  fig1 <- plotly::add_trace(
    plotly::plot_ly(type = "scatter", mode = "lines"),
    data = km,
    x = ~time,
    y = ~wx,
    color = stats::as.formula(paste0("~", abertura_cluster)),
    name = ~id,
    legendgroup = stats::as.formula(paste0("~", abertura_cluster))
  )
  fig2 <- plotly::add_trace(
    plotly::plot_ly(type = "scatter", mode = "lines"),
    data = km_cluster,
    x = ~time,
    y = ~wx,
    color = ~id,
    name = ~id,
    legendgroup = ~id
  )
  plotly::subplot(fig1, plotly::style(fig2, showlegend = F), shareY = TRUE)
}
