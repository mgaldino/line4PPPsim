#' @title Taxa de cambio futura
#'
#' @description gera projecao de taxa de cambio real futura
#'
#' @param start_year A number. the year the series starts
#' @param amp_erro A number. Amplitude of error
#' @param mu mean of normal error
#'
#' @importFrom stats sd
#'
#' @return data frame with real yearly exchange rate and a column of date
#'
#' @examples  gen_cambio_futuro()
#'
#' @export gen_cambio_futuro



gen_cambio_futuro <- function(start_year=2005, amp_erro = 2, mu=0) {
  n <- 33 # number of data points
  k <- 2005 - 1990
  time_series <- seq(pi+.5, 4*pi+.5, length.out = n + k)  # pra gerar igual dados
  time_series <- time_series[-(1:15)]
  y <- sin(time_series)

  sd_cambio_atual <- 0.2825845
  mu_cambio_atual <- 0.6605721
  ajuste_sd <- sd(y)/sd_cambio_atual
  y1 <- y/ajuste_sd

  ajuste_mu <- mu_cambio_atual - mean(y1)
  y2 <- y1 + ajuste_mu
  y3 <- y2 + amp_erro*rnorm(n, mu, .05)

  df_sim <- data.frame(cambio = y3,
                       data = (as.Date("1990-01-01") + 1 + round(365.25*k)) + 366*(1:33))
  return(df_sim)
}
