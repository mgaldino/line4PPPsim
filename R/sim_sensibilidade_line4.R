#' @title Run Monte Carlo simulation for line 4 - sensibilidade parameter
#'
#' @description It computes: diferença entre compensação (MD) e Pagamento (recebimento) de (para) poder concedente (linha 28 excel). Ou seja, a diferença entre receita prevista em contrato e realizada da simulação.
#'
#' @import dplyr
#' @import tidyr
#'
#' @param n_sim A number
#' @param qualityAdjustment A number
#' @param num_years A number
#' @param t_0 A number
#' @param ipc_0 A number
#' @param ipgm_0 A number
#' @param a A number
#' @param b A number
#' @param ipc_realizado A number
#' @param igpm_realizado A number
#' @param ajuste_inflacao logical
#' @param mu_sense A number
#' @param sd_sense A number
#' @param incidencia A vector. line 27 of excel.
#' @param type character. Either "white_noise", or "random_walk" or "constant"
#' @param start_seed A number
#' @param use_random_walk Logical. If TRUE, it will use a random walk to forecast demand (instead of a fixed forecast as in excel)
#' @param start_value A number. First point of the series. Use only if random_walk = T
#' @param mu A number
#' @param sd A number
#' @param beta A number. Effect of past step on the next step.
#' @param jump logical. If TRUE, in the eight year of the series the forecasted demand increases by 25\% relative to last value (plus error). Equivalent to set beta =1.25 for year 8.
#'
#' @return a list with two elements: a data.frame wih simulated difference betwen real demand and forecasted, and a matrizwith sensibilidade parameters generated in the simulation.
#'
#' @examples  sim_sensibilidade_line4(n_sim=3, num_years=33, start_seed = 1, ajuste_inflacao = F)
#'
#' @export sim_sensibilidade_line4



sim_sensibilidade_line4 <- function (n_sim, num_years, t_0 = 2.14, ipc_0 = 1.1,
                                     ipgm_0 = 1.1, a=.5, b=.5, ipc_realizado=NA,
                                     igpm_realizado=NA,
                                     qualityAdjustment=1, ajuste_inflacao,
                                     mu_sense =1, sd_sense = .1,
                                     incidencia = c(0,0,0,.5, rep(1, 9), rep(0, num_years - 13)),
                                     type = "white_noise", start_seed,
                                     use_random_walk=F,
                                     start_value=196860,
                                     mu = start_value, sd = .07*start_value,
                                     beta=1, jump=F){

  switch(type,
         white_noise = white_noise(start_seed, num_years, mu=mu_sense, sd=sd_sense),
         random_walk = random_walk(start_seed, num_years, mu=mu_sense, sd=sd_sense),
         constant = constant(start_seed, num_years))

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
    dif[i,] <- dif[i,]*incidencia
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
    dplyr::mutate(ano = as.Date("2006-01-01") + cumsum(aux)) %>%
    tidyr::gather(sim, param, -ano )

  return(list(df2, sensibilidade_sim2))
}
