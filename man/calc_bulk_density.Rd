% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bulk_density.R
\name{calc_bulk_density}
\alias{calc_bulk_density}
\title{Calculate the bulk density}
\usage{
calc_bulk_density(B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI = NULL)
}
\arguments{
\item{B_SOILTYPE_AGR}{(character) The agricultural type of soil}

\item{A_SOM_LOI}{(numeric) The percentage organic matter in the soil (\%)}

\item{A_CLAY_MI}{(numeric) The clay content of the soil (\%)}
}
\description{
This function calculates the bulk density of the soil based on texture and organic matter
}
