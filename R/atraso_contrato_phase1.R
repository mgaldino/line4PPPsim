#' @title Multa da fase 1 por Atraso Contrato Original
#'
#' @description Computes the fine due to delay in construction, as per in original contract
#'
#' @param num_meses_atraso_phase1 Number of months delayed in phase 1 (there is no specifics about blocks/chunks)
#'
#' @return total payment for the informed number of months delayed
#'
#' @examples  atraso_contrato_phase1(18)
#'
#' @export atraso_contrato_phase1

atraso_contrato_phase1 <- function(num_meses_atraso_phase1 = 18) {

    atraso1 <- ifelse(num_meses_atraso_phase1 > 18, 18,num_meses_atraso_phase1)
    multa1 <- 5220000*atraso1

   return(multa1)
}
