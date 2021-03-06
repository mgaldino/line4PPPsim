% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multa_atraso.R
\name{multa_atraso}
\alias{multa_atraso}
\title{Simula multa por atraso}
\usage{
multa_atraso(num_meses_atraso_phase1, num_meses_atraso_phase2,
  num_meses_atraso_phase2_equipamento, vec_meses_atrasados_aditivo3,
  vec_meses_atrasados_aditivo4_phase1, type)
}
\arguments{
\item{num_meses_atraso_phase1}{Number of months delayed for phase 1 (oiriginal contract)}

\item{num_meses_atraso_phase2}{Number of months delayed for phase 2 (oiriginal contract)}

\item{num_meses_atraso_phase2_equipamento}{Number of months delayed for equipment for phase 2 (oiriginal contract)}

\item{vec_meses_atrasados_aditivo3}{Vector number of months delayed for each block (trecho) for phase 1 in 'aditivo 3'}

\item{vec_meses_atrasados_aditivo4_phase1}{Vector with number of months delayed for each block (trecho) for phase 1 in 'aditivo 4'}

\item{type}{character. One of "contrato_original", "aditivo_3" or "aditivo_4"}
}
\value{
total payment for the informed number of months delayed
}
\description{
Simula multa por atraso em diferentes versoes do contrato.
}
\examples{
multa_atraso(num_meses_atraso_phase2 = 12, num_meses_atraso_phase2_equipamento = 12,
 vec_meses_atrasados_aditivo3 = c(18, 14, NA),
 type= "aditivo_3")

}
