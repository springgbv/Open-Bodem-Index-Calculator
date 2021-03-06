% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crop_classification.R
\name{calc_cropclass}
\alias{calc_cropclass}
\title{Determine classification rules for crops used to prepare crops.obic}
\usage{
calc_cropclass(B_LU_BRP, B_SOILTYPE_AGR, nutrient = NULL)
}
\arguments{
\item{B_LU_BRP}{(numeric) The crop code from the BRP}

\item{B_SOILTYPE_AGR}{(character) The agricultural type of soil}

\item{nutrient}{(character) The nutrient for wich crop classification is needed. Options include P, K and S.}
}
\description{
This function determines crop classes given crop response to P, K and S fertilizers
}
