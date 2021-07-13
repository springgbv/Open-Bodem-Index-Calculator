#' Calculate the average age of the grass
#' 
#' This function calculates the average age of the grass
#' 
#' @param ID (numeric) The ID of the field
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' 
#' @import data.table
#' 
#' @export
calc_grass_age <- function(ID, B_LU_BRP) {
  
  all_count = crop_category = crop_code = grass = grass_age = grass_count = this_id = NULL
  
  # Load data
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  
  # Check inputs
  arg.length = max(length(ID), length(B_LU_BRP))
  checkmate::assert_atomic_vector(ID, len = arg.length)
  checkmate::assert_numeric(B_LU_BRP, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  
  # Collect the data into a table
  dt <- data.table(
    this_id = 1:arg.length,
    ID = ID,
    B_LU_BRP = B_LU_BRP
  )
  
  # merge with crops.obic to get crop_category
  dt <- merge(dt, crops.obic[, list(crop_code, crop_category)], by.x = "B_LU_BRP", by.y = "crop_code")
  setorder(dt, this_id)
  
  # Calculate the age of the grassland
  dt[, grass := ifelse(crop_category == "grasland", 1, 0)]
  dt[, grass_age := rev(cumsum(grass)),by = rleid(ID, grass)]
 
  # extract grass age 
  value <- dt[, grass_age]
  
  # return grass age
  return(value)
}
