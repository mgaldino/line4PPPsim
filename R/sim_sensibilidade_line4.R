#' @title Run Monte Carlo simulation for line 4 - sensibilidade parameter
#'
#' @description This package allows the user to run Monte Carlo simulation to assess the fiscal impact of lline 4 PPP in São Paulo.
#'
#'@import tidyr
#'@import dplyr
#'@import stats
#'
#' @param num_sim A number
#' @param qualityAdjustment A number
#' @param num_years A number
#' @param num_years A number
#' @param t_0 A number
#' @param ipc_0 A number
#' @param ipgm_0 A number
#' @param a A number
#' @param b A number
#' @param ipc_realizado A number
#' @param igpm_realizado A number
#' @param sensibilidade A vector]
#' @param qualityAdjustment A number
#' @param ajuste_inflacao logical
#' @param mu A number
#' @param sd A number
#' @param type character. Either "white_noise", or "random_walk" or "constant"
#' @param start_seed A number
#'
#' @return a list with two elements: a data.frame wih simulated difference betwen real demand and forecasted, and a matrizwith sensibilidade parameters generated in the simulation.
#'
#' @examples  sim_sensibilidade_line4(3, 33, start_seed = 1, ajuste_inflacao = F)
#'
#' @export sim_sensibilidade_line4



sim_sensibilidade_line4 <- function (n_sim, num_years,t_0 = 2.14, ipc_0 = 1.1,
                                     ipgm_0 = 1.1, a=.5, b=.5, ipc_realizado=NA,
                                     igpm_realizado=NA,
                                     qualityAdjustment=1, ajuste_inflacao,
                                     mu =1, sd = .1,
                                     type = "white_noise", start_seed){

  previsto  <- gen_forecast_revenue(num_years = 33, ajuste_inflacao = F)

  # criando matriz para guardar resultados
  # cada linha e uma simulacao
  realizado  <- matrix(0, nrow=n_sim, ncol=33)
  dif <- matrix(0, nrow=n_sim, ncol=33)

  # sensibilidade, random walk, constant or white noise

  param <- matrix(0, nrow=n_sim, ncol=33)
  # rodando simulacao

  for ( i in 1:n_sim) {
    param[i,] <- gen_sensibilidade(start_seed, num_years, mu, sd, type)

    realizado[i,] <- gen_ticket_revenue(sensibilidade = param[i,], num_years = 33, ajuste_inflacao = F)

    dif[i,] <- realizado[i,] - previsto
  }

  dif1 <- as.data.frame(t(dif))
  names(dif1) <- paste0("sim", 1:n_sim)

  aux <- rep(365.5,33)
  df2 <- dif1 %>%
    mutate(ano = as.Date("2006-01-01") + cumsum(aux)) %>%
    gather(sim, diferenca, -ano)

  sensibilidade_sim <- param
  sensibilidade_sim1 <- as.data.frame(t(sensibilidade_sim))

  names(sensibilidade_sim1) <- paste0("sim", 1:n_sim)
  aux <- rep(365.5,33)

  sensibilidade_sim2 <- sensibilidade_sim1 %>%
    mutate(ano = as.Date("2006-01-01") + cumsum(aux)) %>%
    gather(sim, param, -ano )

  return(list(df2, sensibilidade_sim2))
}
