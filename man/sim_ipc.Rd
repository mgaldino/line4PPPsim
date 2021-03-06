% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_ipc.R
\name{sim_ipc}
\alias{sim_ipc}
\title{Simulates fiscal impact of line 4 subway PPP}
\usage{
sim_ipc(ipc_0 = 1.1, ipc_realizado)
}
\arguments{
\item{ipc_0}{A number}

\item{ipc_realizado}{A vector}
}
\value{
A vector of inflation for 33 years
}
\description{
This package allows the user to run Monte Carlo simulation to assess the fiscal impact of lline 4 PPP in São Paulo.
}
\examples{
 sim_ipc(ipc_0 = 1.1, ipc_realizado = NA)

}
