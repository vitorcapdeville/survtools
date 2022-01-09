test_that("surv_cluster clusterizes", {

  aj <- surv_aj("x", surv_fake)

  res <- surv_cluster(aj, 2)
  expec <- data.frame(
    id = c("grupo1", "grupo2", "grupo3", "grupo4"),
    idCluster = factor(c("A", "A", "B", "B"))
  )

  expect_equal(res, expec)
})


test_that("add_cluster works", {

  aj <- surv_aj("x", surv_fake)


  cluster_res <- surv_cluster(aj, 2)
  res <- add_cluster(surv_fake, cluster_res, id = "x")

  expec <- surv_fake
  expec$xCluster[which(expec$x %in% cluster_res$id[cluster_res$idCluster == "A"])] <- "A"
  expec$xCluster[which(expec$x %in% cluster_res$id[cluster_res$idCluster == "B"])] <- "B"
  expec$xCluster <- factor(expec$xCluster)

  expect_equal(res, expec)
})
