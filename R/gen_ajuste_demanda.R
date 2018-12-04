#' @title Simulates demand adjust for line 4 PPP
#'
#' @description This function allows the user to simulate amount do be paid or received according to dsifference between forecasted and "real"  (simulated) demand.
#' By setting the sensibilidade paramter, the user adjust how much the demand will be higher or lower. It computes the "MD".
#'
#' @param num_years A number
#' @param t_0 A number
#' @param ipc_0 A number
#' @param ipgm_0 A number
#' @param a A number
#' @param b A number
#' @param ipc_realizado A number
#' @param igpm_realizado A number
#' @param sensibilidade A vector
#' @param ajuste_inflacao logical
#' @param use_random_walk Logical. If TRUE, it will use a random walk to forecast demand (instead of a fixed forecast as in excel)
#' @param start_value A number. First point of the series. Use only if random_walk = T
#' @param mu A number
#' @param sd A number
#' @param beta A number. Effect of past step on the next step.
#' @param jump logical. If TRUE, in the eight year of the series the forecasted demand increases by 25\% relative to last value (plus error). Equivalent to set beta =1.25 for year 8.
#'
#' @return a vector of demand adjustement of size equal to num_years - input to compute implicit rate
#'
#' @examples  gen_ajuste_demanda(sensibilidade = 1.3, use_random_walk = F)
#'
#' @export gen_ajuste_demanda

gen_ajuste_demanda <- function(sensibilidade = 1, num_years,
                               t_0 = 2.14,
                               ipc_0 , ipgm_0 ,
                               a,b, ipc_realizado = NA ,
                               igpm_realizado = NA, ajuste_inflacao = F,
                               use_random_walk,
                               start_value,
                               mu , sd,
                               beta, jump) {

  stopifnot(.6 <= sensibilidade && sensibilidade <= 1.4)

  price_ticket <- gen_price_ticket_line4(num_years, t_0,
                                         ipc_0, ipgm_0,
                                         a, b, ipc_realizado = ipc_realizado,
                                         igpm_realizado = igpm_realizado, ajuste_inflacao)
  passengers <- gen_num_passengers(sensibilidade, use_random_walk,
                                   start_value, num_years,
                                   mu , sd,
                                   beta, jump)
  dp <- passengers[[3]]
  dr <- dp*sensibilidade
  pe <- passengers[[1]]
  pi <- passengers[[2]]
  aux <- .5*pi*price_ticket/(pi +pe) + price_ticket*pe / (pe +pi)


  if(.6 <= sensibilidade && sensibilidade <= .8) {
    md <- (.06*dp + .9*(.8*dp - dr))*aux
  }

  if(.8 < sensibilidade && sensibilidade <= .9) {
    md <- .06*(.9*dp - dr)*aux
  }

  if(.9 < sensibilidade && sensibilidade <= 1.1) {
    md <- rep(0, num_years)
  }

  if(1.1 < sensibilidade && sensibilidade <= 1.2) {
    md <- -.06*(dr - 1.1*dp)*aux
  }

  if(1.2 < sensibilidade && sensibilidade <= 1.4) {
    md <- -(.06*dp + .9*(dr - 1.2*dp))*aux
  }
  md <- round(ifelse(is.nan(md),0,md))
  return(md)

}

