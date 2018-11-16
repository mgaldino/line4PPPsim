#' @title Simulates fiscal impact of line 4 subway PPP
#'
#' @description This package allows the user to run Monte Carlo simulation to assess the fiscal impact of lline 4 PPP in São Paulo.
#'
#'@import stats
#'
#' @param ipc_0 A number
#' @param ipc_realizado A vector
#'
#' @return A vector of inflation for 33 years
#'
#' @examples  sim_ipc(ipc_realizado = NA)
#'
#' @export sim_ipc

sim_ipc <- function(ipc_0 = 1.1, ipc_realizado) {
  if (sum(is.na(ipc_realizado)) == 0) {
    # inflação anual, t-student, 7 df, com drift de .05
    ipc_base <- 1 +rt(32, 7)/75 + .05 # simular melhor depois. arima, algo assim, simples.
    ipc <- cumprod(c(ipc_0, ipc_base))

  } else {
    n <- length(ipc_realizado)
    ipc_base <- 1 + rt(33 - n, 7)/75 + .05
    ipc <- cumprod(c(1+ipc_realizado, ipc_base))
  }
  return(ipc)
}

