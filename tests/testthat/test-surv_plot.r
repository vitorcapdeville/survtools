test_that("plots have known output", {

  aj <- surv_aj("x", surv_fake)
  km <- surv_extract(aj)
  vdiffr::expect_doppelganger("surv_extract_plot", surv_extract_plot(km))

})

test_that("plots have known output", {

  aj <- surv_aj("x", surv_fake)

  data <- add_cluster(surv_fake, surv_cluster(aj, 2), "x")

  vdiffr::expect_doppelganger("surv_plot_cluster", surv_plot_cluster("x", data))

})

test_that("plots have known output", {

  aj <- surv_aj("x", surv_fake)
  aj2 <- surv_aj("xCluster", add_cluster(surv_fake, surv_cluster(aj, 2), "x"))

  vdiffr::expect_doppelganger("surv_plot", surv_plot(aj))
  vdiffr::expect_doppelganger("surv_plot n < 3", surv_plot(aj2))

})


