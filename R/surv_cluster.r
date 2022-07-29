#' K-means clusterization for survival data
#'
#' Clusterization of survival data using `kml` and `survival`. This function
#' identify clusters of data that behaves similarly with regard to the hazard
#' function.
#'
#' @param n_clusters number of clusters, as defined in `kml::kml`.
#' @param parAlgo,nbRedrawing argumentos passados para `kml::kml`.
#' @inheritParams surv_extract_km
#'
#' @return a data.frame with the resulting groups.
#'
#' @export
#'
surv_cluster <- function(aj, n_clusters, nbRedrawing = 1, parAlgo = kml::parALGO(saveFreq = Inf, distanceName = "canberra")) {
  km <- as.data.frame(
    tidyr::pivot_wider(
      dplyr::select(
        surv_extract(aj),
        !surv
      ),
      names_from = "time",
      values_from = "wx"
    )
  )
  names(km) <- c("id", paste("t", 1:(ncol(km) - 1)))
  km_cld <- kml::cld(km)
  option <- parAlgo
  # prevent annoying print
  invisible(
    utils::capture.output(
      kml::kml(km_cld, nbRedrawing = nbRedrawing, nbClusters = n_clusters, parAlgo = option)
    )
  )
  aux_cluster <- paste0("c", n_clusters)
  ret <- data.frame(
    id = km_cld["idFewNA"],
    cluster = km_cld[aux_cluster][[1]]["clusters"]
  )
  names(ret) <- c("id", "idCluster")
  return(ret)
}


#' Add the clusterization results to the full data
#'
#' @param data data.frame with the full data to wich the clusterized column will be added.
#' @param cluster_res export of the `surv_cluster` function, usually with only one covariate.
#' @param id name of the unclusterized column, used in the join.
#' @param name_to name of the clusterized column.
#'
#' @return a data.frame with the same columns as the input, plus the cluster column.
#'
#' @export
#'
add_cluster <- function(data, cluster_res, id, name_to = paste0(id, "Cluster")) {

  by <- c("id")
  names(by) <- id

  full_data <- dplyr::left_join(data,cluster_res,by = by)

  names(full_data)[names(full_data) == "idCluster"] <- name_to

  return(full_data)
}
