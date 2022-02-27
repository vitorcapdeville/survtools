test_that("surv_aj works", {

  res <- surv_aj("x", surv_fake)
  expec <- survival::survfit(survival::Surv(time, event) ~ x, data = surv_fake)

  # ignore call value since its not supposed to be equal
  res$call <- NULL
  expec$call <- NULL

  expect_equal(res, expec, ignore_attr = T)
})


test_that("truncate_interval works", {

  df <- data.table::data.table(start = "2020-01-01", end = "2020-12-01", censor = 1)
  res <- truncate_interval(df, lubridate::interval(lubridate::ymd("2020-03-01"), lubridate::ymd("2020-06-01")), startDate = "start", endDate = "end", censor = "censor")
  expec <- data.table::data.table(start = lubridate::ymd("2020-01-01"), end = lubridate::ymd("2020-12-01"), censor = 1, event = 0, dataInicio = lubridate::ymd("2020-03-01"), dataFim = lubridate::ymd("2020-06-01"), tempoInicio = 2, tempoFim = 5)

  expect_equal(res, expec, ignore_attr = T)
})
