test_that("surv_extract_plot have known output", {

  aj <- surv_aj("x", surv_fake)
  km <- surv_extract(aj)
  vdiffr::expect_doppelganger("surv_extract_plot", surv_extract_plot(km))

})

test_that("surv_plot_cluster have known output", {

  aj <- surv_aj("x", surv_fake)

  data <- add_cluster(surv_fake, surv_cluster(aj, 2), "x")

  vdiffr::expect_doppelganger("surv_plot_cluster", surv_plot_cluster("x", data))

})

test_that("surv_plot have known output", {

  aj <- surv_aj("x", surv_fake)
  aj2 <- surv_aj("xCluster", add_cluster(surv_fake, surv_cluster(aj, 2), "x"))

  vdiffr::expect_doppelganger("surv_plot both", surv_plot(aj))
  vdiffr::expect_doppelganger("surv_plot wx", surv_plot(aj, type = "wx"))
  vdiffr::expect_doppelganger("surv_plot surv", surv_plot(aj, type = "surv"))
  vdiffr::expect_doppelganger("surv_plot n < 3 both", surv_plot(aj2))
  vdiffr::expect_doppelganger("surv_plot n < 3 wx", surv_plot(aj2, type = "wx"))
  vdiffr::expect_doppelganger("surv_plot n < 3 surv", surv_plot(aj2, type = "surv"))

})


