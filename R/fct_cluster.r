#' Clusterização via K-Means Longitudinal usando o pacote kml.
#'
#' @param base Base de dados contendo as colunas TempoCasa, event e var.
#' @param var nome da coluna a ser clusterizada.
#' @param n_clusters número de clusters.
#'
#' @return data.frame contendo os valores distintos da coluna original
#' e o cluster a qual cada um deles pertence.
cluster_km <- function(base, var, n_clusters) {
  km <- as.data.frame(
    tidyr::pivot_wider(
      dplyr::select(
        surv_extract(
          survival::survfit(
            stats::as.formula(paste0("Surv(TempoCasa, event) ~ ", var)),
            data = base
          )
        ),
        !surv
      ),
      names_from = "time",
      values_from = "wx"
    )
  )
  names(km) <- c("id", paste("t", 1:(ncol(km) - 1)))
  km_cld <- kml::cld(km)
  option <- kml::parALGO(saveFreq = Inf, distanceName = "canberra")
  kml::kml(km_cld, nbRedrawing = 1, nbClusters = n_clusters, parAlgo = option)
  aux_cluster <- paste0("c", n_clusters)
  ret <- data.frame(
    id = km_cld["idFewNA"],
    cluster = km_cld[aux_cluster][[1]]["clusters"]
  )
  names(ret) <- c("id", paste0(var, "Cluster"))
  return(ret)
}


#' Função auxiliar para adicionar a coluna de cluster na base original.
#'
#' @inheritParams cluster_km
#'
#' @return base com uma coluna extra chamada de varClusters
add_cluster <- function(base, var, n_clusters) {
  by <- c("id")
  names(by) <- var
  return(dplyr::left_join(base, cluster_km(base, var, n_clusters), by = by))
}
