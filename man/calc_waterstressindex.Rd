% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wateravailability.R
\name{calc_waterstressindex}
\alias{calc_waterstressindex}
\title{Calculate the Water Stress Index}
\usage{
calc_waterstressindex(B_HELP_WENR, B_LU_BRP, B_GWL_CLASS, WSI = "waterstress")
}
\arguments{
\item{B_HELP_WENR}{(character) The soil type abbreviation, derived from 1:50.000 soil map}

\item{B_LU_BRP}{(numeric) The crop code (gewascode) from the BRP}

\item{B_GWL_CLASS}{(character) The groundwater table class}

\item{WSI}{(character) The type of Water Stress Index is required. Options: droughtstress, wetnessstress and the (combined) waterstress}
}
\description{
This function calculates the Water Stress Index (estimating the yield depression as a function of water deficiency or surplus)
}
\references{
STOWA (2005) Uitbreiding en Actualisering van de HELP-tabellen ten behoeve van het Waternood instrumentarium
}
