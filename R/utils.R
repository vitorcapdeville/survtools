#' Fit a KM estimate with only one covariate
#'
#' Wrapper around `survival::survfit.formula` for allowing the use of `purrr::map` and fit
#' several one covariate KM estimates at a time.
#'
#' @param x name of the covariate column.
#' @param data data.frame with the survival data.
#' @param time name of the time variable column.
#' @param event name of the censor/death variable column.
#' @param ... extra arguments passed to `survival::survfit`.
#'
#' @return an object of class "survfit". See survfit.object for details. Methods defined for survfit objects are print, plot, lines, and points.
#'
#' @seealso \code{\link{survfit.formula}}
#'
#' @export
#'
surv_aj <- function(x, data, time = "time", event = "event", ...) {
  survival::survfit(stats::formula(paste0("survival::Surv(",time,",",event,")~", x)), data = data, ...)
}

#' Truncate survival times
#'
#' Function to truncate survival times in a time interval, for investiganting survival probabilities changes in time.
#'
#' @param data data.frame
#' @param time_int lubridate::interval output, with the time intervel where the survival times will be
#' truncated
#' @param startDate name of the column in data with the birth date of each subject (birth date can be seen as policy begin)
#' @param endDate name of the column in data with the death/censor date of each subject
#' @param censor name of the column with the censoring status. 1 if endDate is a death date, or 0 if endDate is a censor date.
#' if endDate is later than interval end, then the end date will be the interval end and the status will be censor.
#'
#' @return a data.table with the extra columns: event, dataInicio, dataFim, tempoInicio, tempoFim
#'
#' @importFrom data.table :=
#' @export
#'
truncate_interval <- function(data, time_int, startDate, endDate, censor){
  startInt = lubridate::ymd(lubridate::int_start(time_int))
  endInt = lubridate::ymd(lubridate::int_end(time_int))
  ret = data[
    ,
    c(startDate, endDate) := list(lubridate::ymd(get(startDate)), lubridate::ymd(get(endDate)))
  ][
    lubridate::int_overlaps(
      lubridate::interval(get(startDate),get(endDate)),
      time_int
    ),
  ][
    ,
    c("event", "dataInicio", "dataFim") := list(
      dplyr::if_else(
        endInt <= get(endDate),
        0,
        as.numeric(!!censor)
      ),
      dplyr::if_else(
        startInt >= get(startDate),
        startInt,
        get(startDate)
      ),
      dplyr::if_else(
        endInt <= get(endDate),
        endInt,
        get(endDate)
      )
    )
  ][
    ,
    c("tempoInicio", "tempoFim") := list(
      lubridate::interval(
        get(startDate),
        dataInicio
      ) %/% months(1),
      tempoFim = lubridate::interval(
        get(startDate),
        dataFim
      ) %/% months(1)
    )
  ]
  return(ret)
}
