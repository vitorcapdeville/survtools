## code to prepare `surv_fake` dataset goes here

shape = c(0.5, 0.5, 0.5, 0.5)
scale = c(52, 48, 28, 32)
n <- c(10000, 10000, 10000, 10000)

set.seed(1)
grupo1 <- pmin(round(rweibull(n[1], shape[1], scale[1]),0), 100)
grupo2 <- pmin(round(rweibull(n[2], shape[2], scale[2]),0), 100)
grupo3 <- pmin(round(rweibull(n[3], shape[3], scale[3]),0), 100)
grupo4 <- pmin(round(rweibull(n[4], shape[4], scale[4]),0), 100)


surv_fake <- data.frame(
  time = c(grupo1, grupo2, grupo3, grupo4),
  event = rep(1, sum(n)),
  x = c(rep("grupo1", n[1]), rep("grupo2", n[2]), rep("grupo3", n[3]), rep("grupo4", n[4]))
)

usethis::use_data(surv_fake, overwrite = TRUE, internal = T)
