#' @title Multa por Atraso Aditivo 4
#'
#' @description Computes the fine due to delay in construction, as per in "aditivo 4"
#'
#' @param vec_meses_atrasados_aditivo4_phase1 A vector with numbers - number of delay (in months) for each tranche. There are three tranches.
#'
#' @return total payment for the informed number of months delayed and delay per tranche
#'
#' @examples  aditivo4(vec_meses_atrasados_aditivo4_phase1= c(10, 14, NA))
#'
#' @export aditivo4


aditivo4 <- function(vec_meses_atrasados_aditivo4_phase1) {
  vec_meses_atrasados_aditivo4_phase1 <- ifelse(is.na(vec_meses_atrasados_aditivo4_phase1), 0,vec_meses_atrasados_aditivo4_phase1)
  vec_meses_atrasados_aditivo4_phase1 <- ifelse(vec_meses_atrasados_aditivo4_phase1 > 18, 18, vec_meses_atrasados_aditivo4_phase1)
  total <- sum(c(1740000, 580000, 1740000) *vec_meses_atrasados_aditivo4_phase1)
  return(total)

}
