#' Calculate the difference between pH and optimum
#' 
#' This functions calculates the difference between the measured pH and the optimal pH according to the Bemestingsadvies
#' 
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The organic matter content of soil in percentage
#' @param A_CLAY_MI (numeric) The percentage A_CLAY_MI present in the soil
#' @param A_PH_CC (numeric) The pH-CaCl2 of the soil
#' @param D_CP_STARCH (numeric) The fraction of starch potatoes in the crop plan
#' @param D_CP_POTATO (numeric) The fraction of potatoes (excluding starch potatoes) in the crop plan
#' @param D_CP_SUGARBEET (numeric) The fraction of sugar beets in the crop plan
#' @param D_CP_GRASS (numeric) The fracgtion of grass in the crop plan
#' @param D_CP_MAIS (numeric) The fraction of mais in the crop plan
#' @param D_CP_OTHER (numeric) The fraction of other crops in the crop plan
#' 
#' @references \href{https://www.handboekbodemenbemesting.nl/nl/handboekbodemenbemesting/Handeling/pH-en-bekalking/Advisering-pH-en-bekalking.htm}{Handboek Bodem en Bemesting tabel 5.1, 5.2 en 5.3}
#' 
#' @import data.table
#' 
#' @export
calc_ph_delta <- function(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_PH_CC,
                          D_CP_STARCH, D_CP_POTATO, D_CP_SUGARBEET, D_CP_GRASS, D_CP_MAIS, D_CP_OTHER) {
  
  lutum.low = lutum.high = om.low = om.high = potato.low = potato.high = sugarbeet.low = sugarbeet.high = ph.optimum = NULL
  id = soiltype.ph = crop_code = crop_name = NULL
  
  # Load in the datasets
  soils.obic <- as.data.table(OBIC::soils.obic)
  crops.obic <- as.data.table(OBIC::crops.obic)
  dt.ph.delta <- as.data.table(OBIC::tbl.ph.delta)
  
  # Check inputs
  arg.length <- max(length(A_PH_CC), length(B_SOILTYPE_AGR), length(A_SOM_LOI), length(A_CLAY_MI), length(D_CP_STARCH), length(D_CP_POTATO), 
                    length(D_CP_SUGARBEET), length(D_CP_GRASS), length(D_CP_MAIS), length(D_CP_OTHER), length(B_LU_BRP))
  checkmate::assert_numeric(A_PH_CC, lower = 2, upper = 10, any.missing = FALSE, len = arg.length)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(soils.obic$soiltype))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_CP_STARCH, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_CP_POTATO, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_CP_SUGARBEET, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_CP_GRASS, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_CP_MAIS, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_CP_OTHER, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  cp.total <- D_CP_STARCH + D_CP_POTATO + D_CP_SUGARBEET + D_CP_GRASS + D_CP_MAIS + D_CP_OTHER
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code))
  if (any(cp.total != 1)) {
     #stop(paste0("The sum of the fraction of cp is not 1, but ", min(cp.total)))
  }
  
  # Collect information in table
  dt <- data.table(
    id = 1:arg.length,
    B_LU_BRP = B_LU_BRP,
    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
    A_SOM_LOI = A_SOM_LOI,
    A_CLAY_MI = A_CLAY_MI,
    A_PH_CC = A_PH_CC,
    D_CP_STARCH = D_CP_STARCH,
    D_CP_POTATO = D_CP_POTATO,
    D_CP_SUGARBEET = D_CP_SUGARBEET,
    D_CP_GRASS = D_CP_GRASS,
    D_CP_MAIS = D_CP_MAIS,
    D_CP_OTHER = D_CP_OTHER,
    table = NA_character_
  )
  
  # Join soil type used for this function and croptype 
  dt <- merge(dt, soils.obic, by.x = "B_SOILTYPE_AGR", by.y = "soiltype")
  dt <- merge(dt, crops.obic[, list(crop_code, crop_name)], by.x = "B_LU_BRP", by.y = "crop_code")

  
  # Define which table to be used
  dt[soiltype.ph == 1, table := "5.1"]
  dt[soiltype.ph == 2, table := "5.3"] 
  dt[D_CP_STARCH > 0.1, table := "5.2"]
  dt[D_CP_GRASS + D_CP_MAIS >= 0.5, table := "mh"] # grasland / melkveehouderij
  dt[D_CP_GRASS + D_CP_MAIS >= 0.5 & grepl('klaver',crop_name), table := "mh_kl"] # grasland met klaver # this is now only for crop_code 800 (Rolklaver) and 2653 (Graszaad (inclusief klaverzaad))
  
  dt[, D_CP_POTATO := D_CP_STARCH + D_CP_POTATO]
  
  # Join conditionally the tables with optimum pH to data
  dt.53 <- dt[table == "5.3"]
  dt.53 <- dt.ph.delta[dt.53, on=list(table == table, lutum.low <= A_CLAY_MI, lutum.high > A_CLAY_MI, om.low <= A_SOM_LOI, om.high > A_SOM_LOI)]
  dt.512 <- dt[table %in% c("5.1", "5.2")]
  dt.512 <- dt.ph.delta[dt.512, on=list(table == table, potato.low <= D_CP_POTATO, potato.high > D_CP_POTATO, sugarbeet.low <= D_CP_SUGARBEET, sugarbeet.high > D_CP_SUGARBEET,om.low <= A_SOM_LOI, om.high > A_SOM_LOI)]
  dt.mh <- dt[table == "mh"]
  dt.mh <- dt.ph.delta[dt.mh, on=list(table == table, om.low <= A_SOM_LOI, om.high > A_SOM_LOI)]
  dt.mh_kl <- dt[table == "mh_kl"]
  dt.mh_kl <- dt.ph.delta[dt.mh_kl, on=list(table == table, om.low <= A_SOM_LOI, om.high > A_SOM_LOI)]  
  
  dt <- rbindlist(list(dt.53, dt.512, dt.mh, dt.mh_kl), fill = TRUE)
  
  # Calculate the difference between the measured pH and the optimum pH
  dt[, ph.delta := ph.optimum - A_PH_CC]
  dt[ph.delta < 0, ph.delta := 0]
  
  # Extract the ph.delta
  setorder(dt, id)
  ph.delta <- dt[, ph.delta]
  
  return(ph.delta)
  
}

#' Calculate the indicator for pH
#' 
#' This function calculates the indicator for the pH of the soil by the difference with the optimum pH. This is calculated in \code{\link{calc_ph_delta}}.
#' 
#' @param D_PH_DELTA (numeric) The pH difference with the optimal pH.
#' 
#' @export
ind_ph <- function(D_PH_DELTA) {
  
  # Check inputs
  checkmate::assert_numeric(D_PH_DELTA, lower = 0, upper = 5, any.missing = FALSE)
  
  # Evaluate the pH
  value <- 1 - OBIC::evaluate_logistic(x = D_PH_DELTA, b = 9, x0 = 0.3, v = 0.4, increasing = TRUE)
  
  return(value)
  
}

#' Table with optimal pH for different crop plans
#' 
#' This table contains the optimal pH for different crop plans and soil types
#' 
#' @format A data.frame with 136 rows and 10 columns:
#' \describe{
#'   \item{table}{The original table from Hanboek Bodem en Bemesting}
#'   \item{lutum.low}{Lower value for A_CLAY_MI}
#'   \item{lutum.high}{Upper value for A_CLAY_MI}
#'   \item{om.low}{Lower value for organic matter}
#'   \item{om.high}{Upper value for organic matter}
#'   \item{potato.low}{Lower value for fraction potatoes in crop plan}
#'   \item{potato.high}{Upper value for fraction potatoes in crop plan}
#'   \item{sugarbeet.low}{Lower value for fraction potatoes in crop plan}
#'   \item{sugarbeet.high}{Upper value for fraction potatoes in crop plan}
#'   \item{ph.optimum}{The optimal pH (pH_CaCl2) for this range}   
#' }
#' 
#' #' @references \href{https://www.handboekbodemenbemesting.nl/nl/handboekbodemenbemesting/Handeling/pH-en-bekalking/Advisering-pH-en-bekalking.htm}{Handboek Bodem en Bemesting tabel 5.1, 5.2 en 5.3}
#' 
"tbl.ph.delta"
