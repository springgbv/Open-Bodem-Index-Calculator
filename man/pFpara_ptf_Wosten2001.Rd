% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/waterretention.R
\name{pFpara_ptf_Wosten2001}
\alias{pFpara_ptf_Wosten2001}
\title{Estimate water retention curve parameters based on Wosten 2001}
\usage{
pFpara_ptf_Wosten2001(Pklei, Pleem, Psom, M50, Bovengrond)
}
\arguments{
\item{Pklei}{(numeric) The clay (<2um) content of the soil (\%)}

\item{Pleem}{(numeric) The leemt (<50um) content of the soil (\%) Pleem > 0}

\item{Psom}{(numeric) The organic matter content of the soil (\%) Psom > 0}

\item{M50}{(numeric)size of  sand fraction (um)}

\item{Bovengrond}{(boolean) whether topsoil (1) or not (0)}
}
\description{
This function estimates water retention curve parameters using Pedo transfer function of Wosten (2001)
}
\references{
Wösten, J. H. M., Veerman, G. ., de Groot, W. J., & Stolte, J. (2001). Waterretentie- en doorlatendheidskarakteristieken van boven- en ondergronden in Nederland: de Staringreeks. Alterra Rapport, 153, 86. https://doi.org/153
}
