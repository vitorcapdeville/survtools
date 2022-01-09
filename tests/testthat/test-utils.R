test_that("surv_aj works", {

  res <- surv_aj("x", surv_fake)
  expec <- survival::survfit(survival::Surv(time, event) ~ x, data = surv_fake)

  # ignore call value since its not supposed to be equal
  res$call <- NULL
  expec$call <- NULL

  expect_equal(res, expec, ignore_attr = T)
})
