#' Calculate indicator for workability 
#'
#' This function calculates the workability of soils, given as potential length of the growing season. Based on Huinink (2018)
#' 
#' @param A_CLAY_MI (numeric) The clay content of the soil (in procent)
#' @param A_SILT_MI (numeric) The silt content of the soil (in procent)
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param B_GWL_GLG (numeric) The lowest groundwater level averaged over the most dry periods in 8 years in cm below ground level
#' @param B_GWL_GHG (numeric) The highest groundwater level averaged over the most wet periods in 8 years in cm below ground level
#' @param B_Z_TWO  (numeric) The distance between ground level and groundwater level at which the groundwater can supply the soil surface with 2mm water per day (in cm)
#' 
#' @import data.table
#' 
#' @references Huinink (2018) Bodem/perceel geschiktheidsbeoordeling voor Landbouw, Bosbouw en Recreatie. BodemConsult-Arnhem
#'  
#' @export
calc_workability <- function(A_CLAY_MI, A_SILT_MI, B_LU_BRP, B_SOILTYPE_AGR, B_GWL_GLG, B_GWL_GHG, B_Z_TWO) {
  
  # define variables used within the function
  id =crop_code = soiltype = landuse = crop_waterstress = crop_season = NULL
  soiltype.m = spring_depth =  gws_sub_workingdepth = NULL
  early_season_day_deficit = late_season_day_deficit = NULL
  req_days_pre_glg = req_days_post_glg = total_days = NULL
  # parameters related to required depth (rd): required depth for capilairy (rdc), for hydrostatic equilibrium (rdh)
  rdh = rdc = rd_spring = rd_fall = NULL
  rdh_spring = rdh_fall = req_spring_depth_day = req_fall_depth_day = NULL
  rsl = derving = yl = yield = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  season.obic <- as.data.table(OBIC::season.obic)
  setkey(season.obic, landuse)

  # Check inputs
  arg.length <- max(length(A_CLAY_MI), length(A_SILT_MI), length(B_LU_BRP), length(B_SOILTYPE_AGR), 
                    length(B_GWL_GLG), length(B_GWL_GHG), length(B_Z_TWO))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  checkmate::assert_numeric(B_GWL_GLG, lower = 0, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_GWL_GHG, lower = 0, any.missing = FALSE, len = arg.length)
  checkmate::assert_true(all(B_GWL_GHG < B_GWL_GLG))
  checkmate::assert_numeric(B_Z_TWO, lower = 0, upper = 400, any.missing = FALSE, len = arg.length)

  # Collect in data table
  dt <- data.table(id = 1:arg.length,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   B_LU_BRP = B_LU_BRP,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_GWL_GLG = B_GWL_GLG,
                   B_GWL_GHG = B_GWL_GHG,
                   B_Z_TWO = B_Z_TWO)
  
  # merge with OBIC crop and soil table
  dt <- merge(dt, crops.obic[, list(crop_code, crop_waterstress, crop_season)], 
              by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.m)], by.x = "B_SOILTYPE_AGR", by.y = "soiltype")
  dt <- merge(dt, season.obic, by.x = 'crop_season', by.y = 'landuse')
  
  ## determine workability key numbers

    # new parameters to be added
    cols <- c('gws_sub_workingdepth','spring_depth')
    
    # sandy soils with variable silt content
    dt[soiltype.m == 'zand' & A_SILT_MI < 10, c(cols) := list(45,35)]
    dt[soiltype.m == 'zand' & A_SILT_MI >= 10 & A_SILT_MI < 20, c(cols) := list(55,30)]
    dt[soiltype.m == 'zand' & A_SILT_MI >= 20, c(cols) := list(60,30)]
      
    # loess and peat soils
    dt[soiltype.m == 'loess',c(cols) := list(65,12)]
    dt[soiltype.m == 'veen',c(cols) := list(55,22)]
    
    # clay soils
    dt[soiltype.m == 'klei' & A_CLAY_MI < 12, c(cols) := list(85,12)]
    dt[soiltype.m == 'klei' & A_CLAY_MI >= 12 & A_CLAY_MI < 17, c(cols) := list(85,12)]
    dt[soiltype.m == 'klei' & A_CLAY_MI >= 17 & A_CLAY_MI < 25, c(cols) := list(75,15)]
    dt[soiltype.m == 'klei' & A_CLAY_MI >= 25 & A_CLAY_MI < 35, c(cols) := list(65,15)]
    dt[soiltype.m == 'klei' & A_CLAY_MI >= 35, c(cols) := list(45,15)]


  # Overwrite spring working depth for perennial crops
  crops.p <- c('boomteelt', 'overig boomteelt', 'groot fruit','grasland zonder herinzaai', 'grasland met herinzaai')
  dt[crop_waterstress %in% crops.p,spring_depth := 0]
  
  # test 1: desired groundwater depth for work under hydrostatic equilibrium (abbreviated as rdh) in spring and fall
  dt[, rdh_spring := gws_sub_workingdepth+spring_depth]
  dt[, rdh_fall := gws_sub_workingdepth]
  
  # test 2: At what groundwater level is  capillary rise lower than evaporation (<2mm/d), required depth capilairy abbreviated as rdc
  dt[, rdc := B_Z_TWO]

  # Choose lowest required depth as required depth
  dt[,rd_spring := fifelse(rdh_spring <= rdc, rdh_spring,rdc)]
  dt[,rd_fall   := fifelse(rdh_fall   <= rdc, rdh_fall,  rdc)]
  
# Calculate season length -----
  
  # Calculate the day on which the desired water depth is reached for spring and fall
  
  # Spring and fall
  dt[rd_spring >= B_GWL_GHG & rd_spring <= B_GWL_GLG,
     req_spring_depth_day := round(138-(asin((-rd_spring-0.5*(-B_GWL_GHG-B_GWL_GLG))/(0.5*(-B_GWL_GHG+B_GWL_GLG)))/0.0172024))]
  dt[rd_fall >= B_GWL_GHG & rd_fall <= B_GWL_GLG,
     req_fall_depth_day := round(138-(asin((-rd_fall-0.5*(-B_GWL_GHG-B_GWL_GLG))/(0.5*(-B_GWL_GHG+B_GWL_GLG)))/0.0172024))]
  
  # if highest groundwater level is deeper than required depth, soil is always workable
  # if lowest groundwater level is higher than required depth, soil is never workable.
  # required_depth_day is set to 228 (which is 15 aug/GLG) so season length will be 0
  dt[rd_spring < B_GWL_GHG, req_spring_depth_day := 1] 
  dt[rd_spring >= B_GWL_GLG, req_spring_depth_day := 228] 
  dt[rd_fall < B_GWL_GHG, req_fall_depth_day := 1] 
  dt[rd_fall > B_GWL_GLG, req_fall_depth_day := 228]
  
  # Calculate the number of days deficit compared to ideal situation, always above zero
  dt[,early_season_day_deficit := pmax(0,req_days_pre_glg-(228-req_spring_depth_day))]
  dt[,late_season_day_deficit := pmax(0,req_days_post_glg-(228-req_fall_depth_day))]
  
  # Calculate relative season length
  dt[,rsl := (total_days-late_season_day_deficit-early_season_day_deficit)/total_days]
  
  # # Calculate percentage yield loss non-grassland by sub-optimal season length
  dt[derving == 'zaai groenten' , yl := 538*rsl^2-1144*rsl+606]
  dt[derving == 'zomergranen'   , yl := 232*rsl^2- 475*rsl+243]
  dt[derving == 'plant groenten', yl := 392*rsl^2- 785*rsl+393]
  dt[derving == 'wintergranen'  , yl :=(232*rsl^2- 475*rsl+243)*0.85/2]
  dt[derving == 'boomteelt'     , yl :=(538*rsl^2-1144*rsl+606)/2]
  dt[derving == 'overig'        , yl := 100*rsl^2- 100*rsl+100]
  
  # helper functions to determine yield loss in grass given soiltype
  ylveen <- approxfun(x = c(1, 0.9, 0.8, 0.6, 0.55, 0.43, 0.22, 0.08, 0),
                      y = c(23, 25, 28, 36, 38, 41, 54, 59, 100), method = 'linear',
                      yleft = NA_integer_, yright = NA_integer_)
  ylsoil <- approxfun(x = c(1, 0.9, 0.8, 0.6, 0.55, 0.43, 0.22, 0.08, 0),
                      y = c(23, 25, 29, 41, 43, 51, 68, 72, 100), method = 'linear',
                      yleft = NA_integer_, yright = NA_integer_)
  
  dt[derving == 'grasland' & soiltype.m == 'veen', yl := ylveen(rsl)]
  dt[derving == 'grasland' & !soiltype.m == 'veen', yl := ylsoil(rsl)]
  
  # Calculate yield fraction, always above zero
  dt[,yield := pmax(0, 1 - 0.01 * yl)] 
  
  # setorder
  setorder(dt, id)
  
  # Return yield
  value <- dt[,yield]
  
  # return value
  return(value)
}

#' Calculate indicator for workability
#'
#' This function calculates the indicator for the workability of the soil expressed as the period in which the soil can be worked without
#' inflicting structural damage that cannot be restored by the regular management on the farm.
#'  
#' @param D_P_WO (numeric) The value of workability calculated by \code{\link{calc_workability}}
#'  
#' @export
ind_workability <- function(D_P_WO) {
  
  # Check inputs
  checkmate::assert_numeric(D_P_WO, lower = 0, upper = 1, any.missing = FALSE)
  
  # Maybe insert logistic function
  return(D_P_WO)
}
