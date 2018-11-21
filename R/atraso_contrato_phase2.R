#' @title Multa por Atraso Contrato Original da fase 2
#'
#' @description Computes the fine due to delay in construction, as per in original contract
#'
#' @param num_meses_atraso_phase2 Number of months delayed in phase 2 (there is no specifics about blocks/chunks)
#' @param num_meses_atraso_phase2_equipamento Number of months delayed in equipments for phase 2 (there is no specifics about blocks/chunks)
#'
#' @return total payment for the informed number of months delayed
#'
#' @examples  atraso_contrato_phase2(12, 12)
#'
#' @export atraso_contrato_phase2

atraso_contrato_phase2 <- function(num_meses_atraso_phase2 = 12,
num_meses_atraso_phase2_equipamento = 12) {

  multa2 <- 2335000*num_meses_atraso_phase2_equipamento


  atraso2 <- ifelse(num_meses_atraso_phase2 > 12, 12,num_meses_atraso_phase2)
  multa3 <- 1167500*atraso2

  total_final <-  multa2 + multa3
  return(total_final)
}
