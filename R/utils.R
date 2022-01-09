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
