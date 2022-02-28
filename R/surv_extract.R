#' Extract survival probabilities and 'failure' probabilities
#'
#' Uses a previously adjusted survfit or coxph model and returns the results in
#' a standard and convenient way.
#'
#' @param aj previously fitted model (using `survival::survfit.formula` or `survival::coxph`).
#' @param newdata a data frame with the same variable names as those that appear
#' in the coxph formula. It is also valid to use a vector, if the data frame
#' would consist of a single row.
#' The curve(s) produced will be representative of a cohort whose covariates
#' correspond to the values in newdata.
#' Default is the mean of the covariates used in the coxph fit.
#' Only used if aj is a `coxph` fit.
#'
#' @return a data.frame with the following columns: `time`, `id`, `surv` and `wx`.
#' `id` is the value of the covariates in the model. If more than one covariate
#' is used, they are merged together using `tidyr::unite`.
#' `surv` is the survival probability and `wx` is n.event/n.risk, closely related
#' with the probability of failing before x + 1, for individuals alive at x.
#'
#' @export
#'
#' @examples
#'
#' require(survival)
#' aj <- survfit(Surv(time, status) ~ sex, data = lung)
#' surv_extract(aj)
#'
#' aj <- coxph(Surv(time, status) ~ sex, data = lung)
#' surv_extract(aj, newdata = data.frame(sex = 1))
#'
#' aj <- coxph(Surv(time, status) ~ sex, data = lung)
#' surv_extract(aj, newdata = data.frame(sex = c(1, 2)))
#'
surv_extract <- function(aj, newdata = NULL) {
  stopifnot(length(class(aj)) == 1 & class(aj) %in% c("coxph", "survfit"))
  if (class(aj) == "coxph") {
    stopifnot(!is.null(newdata))
    ret <- surv_extract_cox(aj, newdata)
  } else if (class(aj) == "survfit") {
    if (!is.null(newdata)) warning("Argumento newdata ignorado no K-M.")
    ret <- surv_extract_km(aj)
  }
  ret <- dplyr::ungroup(
    dplyr::filter(
      dplyr::group_by(
        ret,
        id
      ),
      # Entender pq as vezes da o warning aqui
      # acho q tem a ver com o KM, colocando um tempo a mais.
      time <= time[ifelse(suppressWarnings(max(which(wx != 0))) == -Inf, Inf, suppressWarnings(max(which(wx != 0))))]
    )
  )
  return(surv_fill(ret))
}


#' Função para extrair as curvas de sobrevivência a partir de um ajuste cox.
#'
#' Função interna que recebe um modelo de cox previamente ajustado e retorna os
#' resultados em um formato conveniente.
#'
#' @param aj objeto com o resutado do ajuste, usualmente o resultado de um coxph.
#' @param newdata objeto com os dados onde o ajuste será realizado. para plotar todas as curvas,
#' basta usar a combinação única de todas as covariáveis categóricas. Para covariáveis contínuas,
#' é preciso escolher o valor(ou valores) para os quais a curva deve ser plotada (nao testado).
#'
#' @return data.frame com as colunas time, id, surv e wx.
#' id apresenta as combinações unicas das covariáveis usadas no ajuste.
#' surv apresenta as probabilidades de sobrevivencia e wx apresenta as razoes
#' numero de eventos/numeros de expostos.
surv_extract_cox <- function(aj, newdata) {
  res <- survival::survfit(aj, newdata)
  surv <- as.matrix(res$surv)
  colnames(surv) <- dplyr::pull(
    tidyr::unite(
      newdata, "id", tidyselect::everything(),
      sep = " "
    )
  )
  curvas <- dplyr::ungroup(
    dplyr::mutate(
      dplyr::group_by(
        tidyr::pivot_longer(
          data.frame(time = res$time, surv, check.names = F),
          !time,
          names_to = "id",
          values_to = "surv"
        ),
        id
      ),
      wx = 1 - surv / dplyr::lag(surv, default = 1)
    )
  )
  return(curvas)
}

#' Extract survival probabilities and 'failure' probabilities
#'
#' Uses a previously adjusted survfit model and returns the results in
#' a standard and convenient way. Works only for K-M estimates.
#'
#' @param aj aj previously fitted model (using `survival::survfit()`).
#'
#' @return a data.frame with the following columns: `time`, `id`, `surv` and `wx`.
#' `id` is the value of the covariates in the model. If more than one covariate
#' is used, they are merged together using `tidyr::unite`.
#' `surv` is the survival probability and `wx` is n.event/n.risk, closely related
#' with the probability of failing before x + 1, for individuals alive at x.
#'
surv_extract_km <- function(aj) {
  if (!is.null(aj$strata)) {
    id <- fix_km_names(names(aj$strata))
    times <- aj$strata
  } else {
    id <- ""
    times <- length(aj$time)
  }
  curvas <- data.frame(
    time = aj$time,
    id = rep(id, times = times),
    surv = aj$surv,
    wx = aj$n.event / aj$n.risk
  )
  return(curvas)
}

#' Função para preencher os buracos na função de sobrevivência.
#'
#' @param sobrev data frame com as colunas 'time', 'id' e 'wx'
#'
#' @return data.frame com as mesmas colunas que sobrev, mas
#' sem buracos na coluna time. Os wx's faltantes são preenchidos
#' por zero.
#'
surv_fill <- function(sobrev) {
  aux_tempos <- dplyr::ungroup(
    dplyr::summarise(
      dplyr::group_by(
        sobrev,
        id
      ),
      time = seq(max(time))
    )
  )


  res <- dplyr::ungroup(
    dplyr::mutate(
      dplyr::arrange(
        dplyr::group_by(
          dplyr::mutate(
            dplyr::left_join(
              aux_tempos,
              sobrev,
              by = c("id", "time")
            ),
            wx = ifelse(is.na(wx), 0, wx)
          ),
          id
        ),
        id, time
      ),
      surv = cumprod(1 - wx)
    )
  )
  return(res)
}

#' Função para consertar a forma como o km faz o nome de cada grupo.
#'
#' @param km_names names do componente "strata" do resultado de um
#' ajuste km usando survfit.
#'
#' @return um vetor com os nomes formatados.
#'
fix_km_names_aux <- function(km_names) {
  return(
    stringr::str_trim(
      stringr::str_sub(
        km_names,
        stringr::str_locate(km_names, "=")[1, 1] + 1,
        -1
      )
    )
  )
}


#' Função para consertar a forma como o km faz o nome de cada grupo.
#'
#' Aplica a função para sanitizar os nomes do KM para o caso onde
#' existem varias variaveis explicativas.
#'
#' @param km_names names do componente "strata" do resultado
#' de um ajuste km usando survfit.
#'
#' @return vetor com os nomes corrigidos.
#'
fix_km_names <- function(km_names) {
  aux <- stringr::str_split_fixed(km_names, ", ", n = Inf)
  return(
    dplyr::pull(
      tidyr::unite(
        data.frame(apply(aux, 2, fix_km_names_aux)), "id", dplyr::everything(),
        sep = " "
      )
    )
  )
}


id <- time <- wx <- surv <- NULL
