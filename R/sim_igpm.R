#' @title Simulates fiscal impact of line 4 subway PPP
#'
#' @description This package allows the user to run Monte Carlo simulation to assess the fiscal impact of lline 4 PPP in São Paulo.
#'
#' @param igpm_0 A number
#' @param igpm_realizado A vector or NA
#'
#' @return A vector of inflation for 33 years
#'
#' @examples  sim_igpm(igpm_0= 1.1, igpm_realizado = NA)
#'
#' @export sim_igpm

sim_igpm <- function(igpm_0 = 1.1, igpm_realizado) {
  if (sum(is.na(igpm_realizado))==0) {
    # inflação anual, t-student, 7 df, com drift de .05
    igpm_base <- 1 + rt(32, 7)/75 + .05 # simular melhor depois. arima, algo assim, simples.
    igpm <- cumprod(c(igpm_base, igpm_0))
  } else {
    n <- length(igpm_realizado)
    igpm_base <- 1 +rt(33 - n, 7)/75 + .05
    igpm <- cumprod(c(1+igpm_realizado, igpm_base))
  }
  return(igpm)
}

