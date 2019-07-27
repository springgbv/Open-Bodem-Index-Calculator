#' Calculate the capacity of soils to supply Magnesium
#' 
#' This function calculates an index for the availability of Magnesium in soil
#' 
#' @param A_MG_CC (numeric) The plant available content of Mg in the soil (mg  Mg / kg) extracted by 0.01M CaCl2
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_OS_GV (numeric) The organic matter content of the soil (%)
#' @param A_CEC_CO (numeric) The cation exchange capacity of the soil (mmol+/kg), analysed via Cobalt-hexamine extraction
#' @param A_K_CEC (numeric) The occupation of the CEC with potassium (%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (%)
#' @param B_BT_AK (character) The type of soil
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' 
#' @import data.table
#' 
#' @export
calc_magnesium_availability <- function(A_MG_CC,A_PH_CC,A_OS_GV,A_CEC_CO,A_K_CEC,A_CLAY_MI,B_BT_AK,B_LU_BRP) {
  
  # set variables to NULL
  A_MG_NC = A_PH_KCL = A_SLIB_MI = cF = A_K_CO = kg1 = kg2 = kg = mg_pred = mg_aim = NULL
  id = crop_code = soiltype = soiltype.n = crop_n = NULL
  
  # Load in the datasets for soil and crop types
  # sven: add gras-maize-arable crop group to crops.obic???
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check inputs
  arg.length <- max(length(A_MG_CC), length(A_PH_CC), length(A_OS_GV), length(A_CEC_CO), 
                    length(A_K_CEC), length(A_CLAY_MI), length(B_BT_AK), length(B_LU_BRP))
  # to be added: sven
  
  # Settings
  param.re = 180 # protein content of first cut grassland in spring (g/kg)
  param.k = 33.9 # potassium content of first cut grass in spring (g/kg)
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    A_MG_CC = A_MG_CC,
    A_PH_CC = A_PH_CC,
    A_OS_GV = A_OS_GV,
    A_CEC_CO = A_CEC_CO,
    A_K_CEC = A_K_CEC,
    A_CLAY_MI = A_CLAY_MI,
    B_LU_BRP = B_LU_BRP,
    B_BT_AK = B_BT_AK,
    value = NA_real_
  )
  
  # add crop names and soiltypes
  dt <- merge(dt, crops.obic[, list(crop_code, crop_n)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_BT_AK", by.y = "soiltype")
  
  # Calculate the Mg availability for arable land -----
  dt.arable <- dt[crop_n == "akkerbouw"]
  dt.arable[grepl('zand|loess|dalgrond',soiltype.n),value := A_MG_CC]
  dt.arable[grepl('klei|veen',soiltype.n),value := pmax(0,A_MG_CC-10)]
  
  # Calculate the Mg availability for maize land -----
  dt.maize <- dt[crop_n == "mais"]
  dt.maize[,value := A_MG_CC]
  
  # Calculate Mg availability for grassland on sandy and loamy soils -----
  dt.grass.sand <- dt[crop_n == "gras" & grepl('zand|loess|dalgrond',soiltype.n)]
  dt.grass.sand[,value := A_MG_CC]
  
  # Calculate Mg availability for grassland on clay and peat soils ----- 
  dt.grass.other <- dt[crop_n == "gras" & grepl('klei|veen',soiltype.n)]
  
  # convert CaCl2 method for Mg to former NaCl method
  dt.grass.other[,A_MG_NC := A_MG_CC * 1.987 - 6.8]
  
  # estimate pH-kcl from pH-cacl2
  dt.grass.other[,A_PH_KCL = (A_PH_CC - 0.5262)/0.9288]
  
  # estimate slib via lutum-slib-ratio (Source: bemestingsadvies.nl)
  dt.grass.other[grepl('zeeklei|veen',soiltype.n),A_SLIB_MI := A_CLAY_MI / 0.67]
  dt.grass.other[grepl('rivierklei',soiltype.n),A_SLIB_MI := A_CLAY_MI / 0.61]
  dt.grass.other[grepl('maasklei',soiltype.n),A_SLIB_MI := A_CLAY_MI / 0.55]
  
  # additional temporary variable called cF (Source: Adviesbasis, 2002)
  dt.grass.other[A_OS_GV <= 3,cF:= 2.08]
  dt.grass.other[A_OS_GV > 3,cF:= 5.703 * A_OS_GV^-0.7996]
  
  # calculate A_K_CO in mg K/ kg grond
  dt.grass.other[,A_K_CO := A_CEC_CO * A_K_CEC * 0.01 * 39.098]
  
  # estimate K-index from K-CEC (A_K_CO, mg K/ kg grond) and K-PAE (mg K/ kg grond) (Source: NMI notitie 1436.N.11)
  dt.grass.other[,kg1 := (1.56 * A_K_CC - 17 + 0.29 * A_CEC_CO) * cF * 0.12046]
  dt.grass.other[,kg2 := A_K_CO * cF * 0.12046]
  dt.grass.other[,kg := 0.5 * (kg1 + kg2)]
  
  # remove columns not needed any more
  dt.grass.other[,c(kg1,kg2,cF):=NULL]
  
  # calculate expected Mg-content in grass in the spring on peat soils (R2 = 67%, NMI report 426.98)
  dt.grass.other[grepl('peat',soiltype.n),mg_pred = 4.769 - 0.001564 * A_MG_NC - 0.01021 * kg + 
                   0.00001554 * A_MG_NC * kg -1.238 * A_PH_KCL - 
                   0.01771 * A_OS_GV - 0.0926 * A_SLIB_MI +0.0002456 * A_MG_NC * A_PH_KCL + 0.0000684 * A_MG_NC * A_SLIB_MI +
                   0.000370 * kg * A_SLIB_MI + 0.00975 * A_PH_KCL * A_SLIB_MI + 0.00135 * A_OS_GV * A_SLIB_MI +
                   0.0924 * A_PH_KCL^2 + 0.0002877 * A_SLIB_MI^2 - 0.00000 * A_OS_GV * A_SLIB_MI^2 +
                   0.0001646 * A_OS_GV^2 -0.00001289 * A_SLIB_MI * A_OS_GV^2 -
                   0.000000584 * A_MG_NC * kg * A_SLIB_MI -0.00001062 * A_MG_NC * A_PH_KCL * A_SLIB_MI]
  
  # calculate expected Mg-content in grass in the spring on clay soils (R2 = 58%, NMI report 426.98)
  dt.grass.other[grepl('peat',soiltype.n),mg_pred = 7.55 - 0.000278 * A_MG_NC -0.0405 * kg + 0.00003397 * A_MG_NC * kg
                 -1.882 * A_PH_KCL - 0.3020 * A_OS_GV - 0.1327 * A_SLIB_MI + 0.00750 * kg * A_PH_KCL + 0.0000206 * A_MG_NC * A_OS_GV
                 +0.00545 * kg * A_OS_GV + 0.04841 * A_PH_KCL * A_OS_GV + 0.00000581*A_MG_NC*A_SLIB_MI
                 -0.001054 * kg * A_SLIB_MI + 0.0364 * A_PH_KCL * A_SLIB_MI +0.004155 * A_OS_GV * A_SLIB_MI
                 +0.1096 * A_PH_KCL^2 + 0.001109 * A_OS_GV^2 - 0.0000307 * A_SLIB_MI * A_OS_GV^2 
                 -0.002522 * A_SLIB_MI * A_PH_KCL^2
                 -0.000002441 * A_MG_NC * kg * A_OS_GV -0.000975 * kg * A_PH_KCL * A_OS_GV
                 +0.0001363 * kg * A_PH_KCL * A_SLIB_MI + 0.00001820 * kg * A_OS_GV * A_SLIB_MI
                 -0.000601 * A_PH_KCL * A_OS_GV * A_SLIB_MI]
  
  # estimate optimum mg-content in grass in spring (Kemp, in Handboek Melkveehouderij)
  dt.grass.other[,mg_aim := (2.511 - 86.46/((param.k * param.re)^0.5))^2]
  
  # weighing Mg index
  dt.grass.other[,value := mg_pred - mg_aim]
  
  # Combine both tables and extract values
  dt <- rbindlist(list(dt.grass.sand,dt.grass.other, dt.arable,dt.maize), fill = TRUE)
  setorder(dt, id)
  value <- dt[, value]
  
  # return value: be aware, index is different for different land use and soil combinations
  return(value)
}



#' Calculate the indicator for Magnesium
#' 
#' This function calculates the indicator for the the Magnesium content of the soil by using the Mg-availability calculated by \code{\link{calc_magnesium_availability}}
#' 
#' @param D_MG (numeric) The value of Mg calculated by \code{\link{calc_magnesium_availability}}
#' 
#' @export
ind_magnesium <- function(D_MG,B_LU_BRP) {
  
  # Load in the datasets for soil and crop types
  # sven: add gras-maize-arable crop group to crops.obic???
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  
  # Check inputs
  checkmate::assert_numeric(D_MG, lower = -1, upper = 1, any.missing = FALSE)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  
  # make data.table to save scores
  dt = data.table(
    D_MG = D_MG,
    B_LU_BRP = B_LU_BRP,
    value = NA_real_
  )
  
  # add crop names
  dt <- merge(dt, crops.obic[, list(crop_code, crop_n)], by.x = "B_LU_BRP", by.y = "crop_code")
  
  # Evaluate Mg availability for arable land -----
  dt.arable <- dt[crop_n == "akkerbouw"]
  dt.arable[,value := evaluate_logistic(D_MG, b = 0.18, x0 = 60, v = 5)]
  
  # Evaluate Mg availability for maize land -----
  dt.maize <- dt[crop_n == "mais"]
  dt.maize[,value := evaluate_logistic(D_MG, b = 0.30, x0 = 50, v = 6)]
  
  # Evaluate Mg availability for grassland on sandy and loamy soils -----
  dt.grass.sand <- dt[crop_n == "gras" & grepl('zand|loess|dalgrond',soiltype.n)]
  dt.grass.sand[,value := evaluate_logistic(D_MG, b = 0.35, x0 = 55, v = 8)]
  
  # Evaluate Mg availability for grassland on clay and peat soils ----- 
  dt.grass.other <- dt[crop_n == "gras" & grepl('klei|veen',soiltype.n)]
  dt.grass.other[,value := evaluate_logistic(D_MG, b = 35, x0 = 0.2, v = 9)]
  
  # Combine both tables and extract values
  dt <- rbindlist(list(dt.grass.sand,dt.grass.other, dt.arable,dt.maize), fill = TRUE)
  setorder(dt, id)
  value <- dt[, value]
  
  # return output
  return(value)
}