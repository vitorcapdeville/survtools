test_that("surv_extract (km) works", {
  aj <- survival::survfit(survival::Surv(c(1, 2, 3), c(0, 1, 0)) ~ 1)

  res1 <- surv_extract_km(aj)
  expec1 <- data.frame(time = c(1, 2, 3), id = c("", "", ""), surv = c(1, 0.5, 0.5), wx = c(0, 0.5, 0))
  res2 <- surv_extract(aj)
  expec2 <- tibble::tibble(id = c("", ""), time = c(1, 2), surv = c(1, 0.5), wx = c(0, 0.5))

  expect_equal(
    res1,
    expec1
  )
  expect_equal(
    res2,
    expec2
  )
  expect_warning(surv_extract(aj, newdata = data.frame(1)), "Argumento newdata ignorado no K-M.")
})

test_that("surv_extract (coxph) works", {
  test1 <- list(
    time = c(4, 3, 1, 1, 2, 2, 3),
    status = c(1, 1, 1, 0, 1, 1, 0),
    x = c(0, 2, 1, 1, 1, 0, 0),
    sex = c(0, 0, 0, 0, 1, 1, 1)
  )

  aj <- survival::coxph(survival::Surv(time, status) ~ sex, test1)
  newdata <- dplyr::distinct(data.frame(sex = test1$sex))

  res <- surv_extract(aj, newdata)

  expec <- tibble::tibble(
    id = c("0", "0", "0", "0", "1", "1", "1", "1"),
    time = c(1, 2, 3, 4, 1, 2, 3, 4),
    surv = c(0.8746123, 0.5775050, 0.4206200, 0.1547375, 0.8566717, 0.5304792, 0.3678794, 0.1159379),
    wx = c(0.1253877, 0.3397017, 0.2716600, 0.6321206, 0.1433283, 0.3807672, 0.3065148, 0.6848481)
  )

  res2 <- surv_extract_cox(aj, newdata)
  expec2 <- tibble::tibble(
    time = c(1, 1, 2, 2, 3, 3, 4, 4),
    id = c("0", "1", "0", "1", "0", "1", "0", "1"),
    surv = c(0.8746123, 0.8566717, 0.5775050, 0.5304792, 0.4206200, 0.3678794, 0.1547375, 0.1159379),
    wx = c(0.1253877, 0.1433283, 0.3397017, 0.3807672, 0.2716600, 0.3065148, 0.6321206, 0.6848481)
  )

  expect_equal(
    res,
    expec,
    tolerance = 10e-7
  )
  expect_equal(
    res2,
    expec2,
    tolerance = 10e-7
  )
})

test_that("fix_km_names_aux works", {
  expect_equal(
    fix_km_names_aux(c("sex=FEMININO", "sex=MASCULINO")), c("FEMININO", "MASCULINO")
  )
})

test_that("fix_km_names works", {
  expect_equal(
    fix_km_names(c("sex=FEMININO, x=VERDE", "sex=MASCULINO, x=VERDE", "sex=MASCULINO, x=AZUL")),
    c("FEMININO VERDE", "MASCULINO VERDE", "MASCULINO AZUL")
  )
})
