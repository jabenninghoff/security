#' Calculate Log Normal Parameters
#'
#' Calculate the parameters of a log normal distribution from estimated 0.05 and 0.95 quantile. Also
#'   calculate the fractional difference between the estimated median value (0.5 quantile) and the
#'   median value calculated from the 0.05 and 0.95 values.
#'
#' @param p05 the "low" value, the 0.05 quantile.
#' @param p95 the "high" value, the 0.95 quantile.
#' @param p50 the most likely value, the 0.5 quantile.
#'
#' @return a list including the `meanlog` and `sdlog` parameters, as well as the fractional
#'         difference between the estimated and actual median (0.5 quantile), `mdiff`.
#'
#' @examples
#' lnorm_param(100000, 20000000, 1425000)
#'
#' @export
lnorm_param <- function(p05, p95, p50) {
  meanlog <- ((log(p95) - log(p05)) / 2) + log(p05)
  sdlog <- (log(p95) - log(p05)) / (2 * stats::qnorm(0.95))
  median <- stats::qlnorm(0.5, meanlog = meanlog, sdlog = sdlog)
  mdiff <- (p50 - median) / median

  return(list(meanlog = meanlog, sdlog = sdlog, mdiff = mdiff))
}
